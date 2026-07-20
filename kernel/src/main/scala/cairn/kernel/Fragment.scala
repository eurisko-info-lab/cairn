package cairn.kernel

/** Fragment IR (S7, §2 Fragment). Fragments provide/require sorts,
  * constructors, grammar, rewrite rules, and judgments; they compose by
  * pushout/amalgamation along shared interfaces. Conflicts are errors, never
  * silent overwrites (§4.3).
  */
enum SortMode:
  case Tree
  case Graph

final case class SortDef(name: String, mode: SortMode)

final case class CtorDef(
    name: String,
    sort: String,
    argSorts: List[String],
    /** binder positions: (binderChildIndex, scopeChildIndices) */
    binders: List[(Int, List[Int])] = Nil,
)

/** Rewrite rule as data: pattern with metavariables => template (S15).
  * Metavariable convention: `Leaf("$x")` matches any subterm, binding `x`.
  * Template primitive: `Node("$subst", List(body, name, value))` invokes the
  * engine's generic capture-avoiding substitution.
  */
final case class RewriteRule(name: String, pattern: Cst, template: Cst)

/** Declarative inference rule for judgments (typing, well-formedness):
  * premises and conclusion are Cst patterns over judgment forms.
  */
final case class InferRule(name: String, premises: List[Cst], conclusion: Cst)
final case class JudgmentDef(name: String, rules: List[InferRule])

final case class GrammarPart(
    keywords: List[String] = Nil,
    puncts: List[String] = Nil,
    categories: List[CategorySpec] = Nil,
    precCategories: List[PrecCategory] = Nil,
    printRules: List[PrintRule] = Nil,
    top: Option[String] = None,
)

final case class Fragment(
    name: String,
    provides: List[String],
    requires: List[String],
    excludes: List[String] = Nil,
    sorts: List[SortDef] = Nil,
    constructors: List[CtorDef] = Nil,
    grammar: GrammarPart = GrammarPart(),
    rewriteRules: List[RewriteRule] = Nil,
    judgments: List[JudgmentDef] = Nil,
    varCtor: Option[String] = None,
):
  def canon: Canon = FragmentCodec.toCanon(this)
  def digest: Digest = artifact.digest
  def artifact: Artifact = Artifact(ArtifactKind.Fragment, canon)

/** Structured composition error (§7: cite fragment names, paths, digests). */
final case class ComposeError(path: String, fragmentA: String, fragmentB: String, detail: String):
  def render: String = s"compose conflict at $path between '$fragmentA' and '$fragmentB': $detail"

/** The composed, closed result of amalgamating fragments. */
final case class ComposedLanguage(
    name: String,
    fragments: List[Fragment], // sorted by name — canonical order
    sorts: Map[String, SortDef],
    constructors: Map[String, CtorDef],
    grammar: GrammarSpec,
    rewriteRules: List[RewriteRule],
    judgments: Map[String, JudgmentDef],
    varCtor: Option[String],
):
  def binderSpec: BinderSpec = BinderSpec(
    constructors.values.filter(_.binders.nonEmpty).map(c => c.name -> c.binders).toMap)
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "fragments" -> Canon.CList(fragments.map(f => Canon.CStr(f.digest.hex))))
  def digest: Digest = artifact.digest
  def artifact: Artifact = Artifact(ArtifactKind.Language, canon)

object Compose:
  /** Pushout composition: order-independent amalgamation; identical shared
    * definitions unify, differing definitions with the same name conflict.
    */
  def compose(name: String, fragmentsIn: List[Fragment]): Either[List[ComposeError], ComposedLanguage] =
    val fragments = fragmentsIn.sortBy(_.name)
    var errors = List.newBuilder[ComposeError]

    def mergeNamed[A](path: String, items: List[(String, String, A)]): Map[String, A] =
      // items: (definitionName, owningFragment, definition)
      items.groupBy(_._1).flatMap { case (defName, defs) =>
        val distinct = defs.distinctBy(_._3)
        if distinct.sizeIs > 1 then
          val a = distinct(0); val b = distinct(1)
          errors += ComposeError(s"$path/$defName", a._2, b._2,
            s"conflicting definitions (digests ${Digest.of(Canon.CStr(a._3.toString)).short} vs ${Digest.of(Canon.CStr(b._3.toString)).short})")
          Nil
        else List(defName -> defs.head._3)
      }

    val sorts = mergeNamed("sorts", fragments.flatMap(f => f.sorts.map(s => (s.name, f.name, s))))
    val ctors = mergeNamed("constructors", fragments.flatMap(f => f.constructors.map(c => (c.name, f.name, c))))
    // Grammar categories AMALGAMATE: fragments contribute alternatives to a
    // shared category. Alternative order is canonical: fragments in name
    // order, each fragment's declaration order preserved. Same tag with
    // different elems is a conflict.
    val cats: Map[String, CategorySpec] =
      fragments.flatMap(f => f.grammar.categories.map(c => (f.name, c)))
        .groupBy(_._2.name)
        .map { case (catName, contribs) =>
          val ordered = contribs.sortBy(_._1)
          val allCtors = ordered.flatMap((fn, c) => c.ctors.map(k => (fn, k)))
          val merged = List.newBuilder[ConstructorSpec]
          val seen = scala.collection.mutable.Map[String, (String, ConstructorSpec)]()
          for (fn, k) <- allCtors do
            seen.get(k.tag) match
              case None => seen(k.tag) = (fn, k); merged += k
              case Some((fn0, k0)) if k0 == k => () // identical shared alternative unifies
              case Some((fn0, _)) =>
                errors += ComposeError(s"grammar/categories/$catName/${k.tag}", fn0, fn,
                  "conflicting grammar alternatives with the same tag")
          catName -> CategorySpec(catName, merged.result())
        }
    val precs = mergeNamed("grammar/prec", fragments.flatMap(f => f.grammar.precCategories.map(p => (p.name, f.name, p))))
    val prules = mergeNamed("grammar/print", fragments.flatMap(f => f.grammar.printRules.map(r => (r.tag, f.name, r))))
    val rules = mergeNamed("rewrite", fragments.flatMap(f => f.rewriteRules.map(r => (r.name, f.name, r))))
    val judgs = mergeNamed("judgments", fragments.flatMap(f => f.judgments.map(j => (j.name, f.name, j))))

    // provides/requires closure
    val provided = fragments.flatMap(_.provides).toSet
    for f <- fragments; r <- f.requires if !provided.contains(r) do
      errors += ComposeError(s"requires/$r", f.name, "-", s"required interface '$r' not provided by any fragment")
    for f <- fragments; x <- f.excludes if provided.contains(x) do
      val offender = fragments.find(_.provides.contains(x)).map(_.name).getOrElse("?")
      errors += ComposeError(s"excludes/$x", f.name, offender, s"fragment '${f.name}' excludes '$x' which is provided")

    val varCtors = fragments.flatMap(_.varCtor).distinct
    if varCtors.sizeIs > 1 then
      errors += ComposeError("varCtor", fragments.head.name, fragments.last.name,
        s"conflicting variable constructors: ${varCtors.mkString(", ")}")

    val tops = fragments.flatMap(_.grammar.top).distinct
    if tops.sizeIs > 1 then
      errors += ComposeError("grammar/top", fragments.head.name, fragments.last.name,
        s"conflicting top categories: ${tops.mkString(", ")}")

    val errs = errors.result()
    if errs.nonEmpty then Left(errs)
    else
      val tokens = TokenSpec(
        keywords = fragments.flatMap(_.grammar.keywords).distinct.sorted,
        puncts = fragments.flatMap(_.grammar.puncts).distinct.sortBy(p => (-p.length, p)),
        lineComment = Some("--"))
      val grammar = GrammarSpec(
        name = name,
        tokens = tokens,
        categories = cats.values.toList.sortBy(_.name),
        precCategories = precs.values.toList.sortBy(_.name),
        printRules = prules.values.toList.sortBy(_.tag),
        top = tops.headOption.getOrElse(""))
      Right(ComposedLanguage(
        name, fragments, sorts, ctors, grammar,
        rules.values.toList.sortBy(_.name), judgs, varCtors.headOption))

object FragmentCodec:
  import Canon.*

  private def ruleToCanon(r: RewriteRule): Canon = Canon.cmap(
    "name" -> CStr(r.name), "pattern" -> Cst.toCanon(r.pattern), "template" -> Cst.toCanon(r.template))
  private def ruleFromCanon(c: Canon): RewriteRule =
    RewriteRule(c.field("name").asStr, Cst.fromCanon(c.field("pattern")), Cst.fromCanon(c.field("template")))

  def toCanon(f: Fragment): Canon = Canon.cmap(
    "name" -> CStr(f.name),
    "provides" -> Canon.cstrs(f.provides),
    "requires" -> Canon.cstrs(f.requires),
    "excludes" -> Canon.cstrs(f.excludes),
    "sorts" -> CList(f.sorts.map(s => Canon.cmap(
      "name" -> CStr(s.name), "mode" -> CStr(s.mode.toString.toLowerCase)))),
    "constructors" -> CList(f.constructors.map(c => Canon.cmap(
      "name" -> CStr(c.name), "sort" -> CStr(c.sort),
      "argSorts" -> Canon.cstrs(c.argSorts),
      "binders" -> CList(c.binders.map((b, sc) => Canon.cmap(
        "binder" -> CInt(b), "scope" -> CList(sc.map(i => CInt(i))))))))),
    "grammar" -> Canon.cmap(
      "keywords" -> Canon.cstrs(f.grammar.keywords),
      "puncts" -> Canon.cstrs(f.grammar.puncts),
      "spec" -> GrammarSpec.toCanon(GrammarSpec("", TokenSpec(Nil, Nil, None),
        f.grammar.categories, f.grammar.precCategories, f.grammar.printRules,
        f.grammar.top.getOrElse("")))),
    "rewriteRules" -> CList(f.rewriteRules.map(ruleToCanon)),
    "judgments" -> CList(f.judgments.map(j => Canon.cmap(
      "name" -> CStr(j.name),
      "rules" -> CList(j.rules.map(r => Canon.cmap(
        "name" -> CStr(r.name),
        "premises" -> CList(r.premises.map(Cst.toCanon)),
        "conclusion" -> Cst.toCanon(r.conclusion))))))),
    "varCtor" -> f.varCtor.fold(CTag("none", CInt(0)))(s => CTag("some", CStr(s))))

  def fromCanon(c: Canon): Fragment =
    val g = c.field("grammar")
    val spec = GrammarSpec.fromCanon(g.field("spec"))
    Fragment(
      name = c.field("name").asStr,
      provides = c.field("provides").asList.map(_.asStr),
      requires = c.field("requires").asList.map(_.asStr),
      excludes = c.field("excludes").asList.map(_.asStr),
      sorts = c.field("sorts").asList.map(s => SortDef(s.field("name").asStr,
        if s.field("mode").asStr == "graph" then SortMode.Graph else SortMode.Tree)),
      constructors = c.field("constructors").asList.map(k => CtorDef(
        k.field("name").asStr, k.field("sort").asStr,
        k.field("argSorts").asList.map(_.asStr),
        k.field("binders").asList.map(b =>
          (b.field("binder").asInt.toInt, b.field("scope").asList.map(_.asInt.toInt))))),
      grammar = GrammarPart(
        keywords = g.field("keywords").asList.map(_.asStr),
        puncts = g.field("puncts").asList.map(_.asStr),
        categories = spec.categories,
        precCategories = spec.precCategories,
        printRules = spec.printRules,
        top = Option(spec.top).filter(_.nonEmpty)),
      rewriteRules = c.field("rewriteRules").asList.map(ruleFromCanon),
      judgments = c.field("judgments").asList.map(j => JudgmentDef(
        j.field("name").asStr,
        j.field("rules").asList.map(r => InferRule(
          r.field("name").asStr,
          r.field("premises").asList.map(Cst.fromCanon),
          Cst.fromCanon(r.field("conclusion")))))),
      varCtor = c.field("varCtor") match
        case CTag("some", CStr(s)) => Some(s)
        case _                     => None)
