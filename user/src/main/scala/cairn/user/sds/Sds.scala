package cairn.user.sds

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** SDS pack (M47, §5b): Safety Data Sheet authoring — flagship of
  * `PKI → Law → SDS`. An SDS is NOT a flat document: it is a compiled view
  * over typed objects (substances, mixtures, phrases / corpus phrases,
  * products, shadows, regulatory `basis` citations into Law sections,
  * EU-CLP `euSection` / `outline` / multilingual `sectionField` maps).
  *
  * Object language: [[languages/sds.cairn]] (`provides sds requires law`).
  * Closed composition pulls Law + PKI; compose without them fails.
  * ΔSDS = generic ΔL + domain validation. Scala = host glue only.
  * Phrase staleness (official corpus vs free-text restale) lives in the
  * examples host machine — see `cairn.examples.sds.PhraseStaleness`.
  * Regulatory section numbering (EU-CLP 1..16 + ordering) lives in
  * `cairn.examples.sds.SectionNumbering`. Chemicals fixtures and host report
  * projection (`Chemicals` / `SectionReport`) can project into / from
  * language `euSection` + `outline` terms. Section-field text resolves with
  * the same multilingual fallback as phrases (exact lang → `en` → any), then
  * `sectionFieldShadow` industrial overrides (parallel to phrase `shadow`).
  * Section-field staleness lives in `cairn.examples.sds.SectionFieldStaleness`
  * (reuses `PhraseStaleness.restale`).
  */
final class Sds(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("sds")

  /** Own fragment only — compose fails: `requires law` unmet without Law/PKI. */
  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("sds", fragments)

  /** Closed language: SDS + demoted Law + demoted PKI. */
  lazy val language: ComposedLanguage = packs.requireClosed("sds")

  /** EU-CLP section numbers accepted by the ΔSDS domain gate (1..16). */
  private val euClpNumbers: Set[Int] = (1 to 16).toSet

  // ---- domain validation (ΔSDS = generic ΔL + these checks) ----

  def validate(m: Module): Either[String, Unit] =
    val errs = List.newBuilder[String]
    def defined(n: String) = m.get(n).isDefined
    for (name, term) <- m.defs do term match
      case Cst.Node("mixture", List(Cst.Node("list", comps))) =>
        var total = 0L
        for c <- comps do c match
          case Cst.Node("component", List(Cst.Leaf(ref), Cst.Leaf(pct))) =>
            total += pct.toLong
            if !defined(ref) then errs += s"mixture '$name' references unknown substance '$ref'"
          case other => errs += s"mixture '$name': bad component ${other.render}"
        if total > 100 then errs += s"mixture '$name' percentages sum to $total > 100"
      case Cst.Node("product", List(_, Cst.Leaf(mix), Cst.Node("list", phraseRefs))) =>
        if !defined(mix) then errs += s"product '$name' references unknown mixture '$mix'"
        for p <- phraseRefs do p match
          case Cst.Leaf(pr) if !defined(pr) => errs += s"product '$name' references unknown phrase '$pr'"
          case _ => ()
      case Cst.Node("shadow", List(Cst.Leaf(prod), Cst.Leaf(phrase), _)) =>
        if !defined(prod) then errs += s"shadow '$name' references unknown product '$prod'"
        if !defined(phrase) then errs += s"shadow '$name' references unknown phrase '$phrase'"
      case Cst.Node("sectionFieldShadow", List(Cst.Leaf(sec), Cst.Leaf(key), _)) =>
        if key.isEmpty then errs += s"sectionFieldShadow '$name': empty field key"
        m.get(sec) match
          case Some(Cst.Node("euSection", _)) => ()
          case Some(_) =>
            errs += s"sectionFieldShadow '$name' references '$sec' which is not an euSection"
          case None =>
            errs += s"sectionFieldShadow '$name' references unknown section '$sec'"
      case Cst.Node("basis", List(Cst.Leaf(target), Cst.Leaf(section))) =>
        if !defined(target) then errs += s"basis '$name' references unknown product '$target'"
        if section.isEmpty then errs += s"basis '$name' missing Law section number"
      case Cst.Node("euSection", List(Cst.Leaf(num), Cst.Node("list", fields))) =>
        num.toIntOption match
          case Some(n) if euClpNumbers.contains(n) => ()
          case Some(n) => errs += s"euSection '$name' number $n out of range (expected 1..16)"
          case None => errs += s"euSection '$name' number '$num' is not an integer"
        val seen = scala.collection.mutable.HashSet.empty[(String, String)]
        for f <- fields do f match
          case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(_))) =>
            if k.isEmpty then errs += s"euSection '$name': empty field key"
            else if lang.isEmpty then errs += s"euSection '$name' field '$k': empty lang"
            else if !seen.add((k, lang)) then
              errs += s"euSection '$name' duplicate field '$k' lang '$lang'"
          case other => errs += s"euSection '$name': bad field ${other.render}"
      case Cst.Node("outline", List(_, _, sectionsField)) =>
        val refs = sectionsField match
          case Cst.Node("none", _) => Nil
          case Cst.Node("some", List(Cst.Node("list", rs))) => rs
          case Cst.Node("list", rs) => rs // tolerate non-opt shape
          case other =>
            errs += s"outline '$name': bad sections ${other.render}"
            Nil
        val nums = List.newBuilder[Int]
        for r <- refs do r match
          case Cst.Leaf(ref) =>
            m.get(ref) match
              case Some(Cst.Node("euSection", List(Cst.Leaf(num), _))) =>
                num.toIntOption match
                  case Some(n) if euClpNumbers.contains(n) => nums += n
                  case Some(n) =>
                    errs += s"outline '$name' section '$ref' number $n out of range"
                  case None =>
                    errs += s"outline '$name' section '$ref' has non-integer number '$num'"
              case Some(_) =>
                errs += s"outline '$name' references '$ref' which is not an euSection"
              case None =>
                errs += s"outline '$name' references unknown section '$ref'"
          case other => errs += s"outline '$name': bad section ref ${other.render}"
        val ns = nums.result()
        if ns.distinct.sizeIs != ns.size then
          errs += s"outline '$name' has duplicate section numbers"
        if ns != ns.sorted then
          errs += s"outline '$name' section numbers not ascending: ${ns.mkString(",")}"
      case _ => ()
    val es = errs.result()
    if es.isEmpty then Right(()) else Left(es.mkString("; "))

  /** ΔSDS application: the generic ΔL, then the domain gate. */
  def applySds(m: Module, change: Cst): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, m, change).flatMap { out =>
      validate(out._1).map(_ => out) }

  /** Phrase / product / euSection names a shadow change-set overrides.
    * Domain-aware footprint for GRANITE-style shadow rebase (base edit of an
    * overridden phrase/product/section is a semantic conflict, even when ΔL
    * names differ). Includes both phrase `shadow` and `sectionFieldShadow`.
    */
  def shadowOverrideTargets(change: Cst): Set[String] =
    def items(c: Cst): List[Cst] = c match
      case Cst.Node(_, List(Cst.Node("list", xs))) => xs
      case Cst.Node(_, xs)                         => xs
      case _                                       => List(c)
    items(change).flatMap {
      case Cst.Node(_, List(_, term)) => term match
        case Cst.Node("shadow", List(Cst.Leaf(prod), Cst.Leaf(phrase), _)) =>
          List(prod, phrase)
        case Cst.Node("sectionFieldShadow", List(Cst.Leaf(sec), _, _)) =>
          List(sec)
        case _ => Nil
      case _ => Nil
    }.toSet

  /** Rebase an industrial shadow over a base revision. Disjoint overrides
    * merge; overlapping phrase/product edits surface as Merge.Conflict.
    */
  def rebaseShadow(
      base: Module,
      baseChange: Cst,
      shadowChange: Cst
  ): Either[Merge.Conflict, (Module, Delta.ValidatedChangeSet)] =
    val touched = ChangeAlgebra.footprint(language, baseChange)
    val overrides = shadowOverrideTargets(shadowChange)
    val overlap = touched.intersect(overrides)
    if overlap.nonEmpty then
      Left(Merge.Conflict(
        overlap,
        Artifact(ArtifactKind.ChangeSet, Cst.toCanon(baseChange)).digest,
        Artifact(ArtifactKind.ChangeSet, Cst.toCanon(shadowChange)).digest))
    else
      Merge.threeWay(language, base, baseChange, shadowChange) match
        case Left(c) => Left(c)
        case Right(result) =>
          validate(result._1) match
            case Left(err) => throw IllegalStateException(s"rebase produced invalid SDS: $err")
            case Right(_)  => Right(result)

  // ---- the compiled document view (a bidirectional surface, M47) ----

  val docGrammar: GrammarSpec = GrammarSpec(
    name = "sds-document",
    tokens = TokenSpec(List("SDS", "section", "hazard"), List(":"), None),
    categories = List(
      CategorySpec("doc", List(
        ConstructorSpec("doc", List(
          Elem.Tok("SDS"), Elem.Tok(":"), Elem.StrLeaf, Elem.Star(Elem.Cat("line")))))),
      CategorySpec("line", List(
        ConstructorSpec("hazardLine", List(
          Elem.Tok("hazard"), Elem.NameLeaf, Elem.Tok(":"), Elem.StrLeaf))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("doc", List(
        PrintSeg.Lit("SDS:"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Newline,
        PrintSeg.SepFields(1, "\n"))),
      PrintRule("hazardLine", List(
        PrintSeg.Lit("hazard"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.StrField(1)))),
    top = "doc")

  /** Resolve a phrase (free-text or corpus) with multilingual fallback:
    * exact lang → `en` → any.
    */
  def phraseText(m: Module, phraseName: String, lang: String): Option[String] =
    def texts: List[(String, String)] = m.defs.collect {
      case (_, Cst.Node("corpusPhrase" | "phrase", List(Cst.Leaf(n), Cst.Leaf(l), Cst.Leaf(t))))
          if n == phraseName =>
        (l, t)
    }.toList
    val all = texts
    all.collectFirst { case (l, t) if l == lang => t }
      .orElse(all.collectFirst { case (l, t) if l == "en" => t })
      .orElse(all.headOption.map(_._2))

  /** Resolve a section field inside an `euSection` with the same multilingual
    * fallback as [[phraseText]]: exact lang → `en` → any. Multiple
    * `sectionField` siblings may share a key across langs.
    */
  def sectionFieldText(section: Cst, fieldKey: String, lang: String): Option[String] =
    section match
      case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
        val all = fields.collect {
          case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(t)))
              if k == fieldKey =>
            (l, t)
        }
        all.collectFirst { case (l, t) if l == lang => t }
          .orElse(all.collectFirst { case (l, t) if l == "en" => t })
          .orElse(all.headOption.map(_._2))
      case _ => None

  /** Lookup [[sectionFieldText]] by module binding (section ref), applying
    * any `sectionFieldShadow` industrial override for that (section, key).
    */
  def sectionFieldText(m: Module, sectionRef: String, fieldKey: String, lang: String)
      : Option[String] =
    val overridden = m.defs.collectFirst {
      case (_, Cst.Node("sectionFieldShadow", List(Cst.Leaf(s), Cst.Leaf(k), Cst.Leaf(text))))
          if s == sectionRef && k == fieldKey =>
        text
    }
    overridden.orElse(m.get(sectionRef).flatMap(sectionFieldText(_, fieldKey, lang)))

  /** Compile a product's document: phrases in the requested language (with
    * fallback), shadow overrides applied — a view over typed objects.
    */
  def render(m: Module, productName: String, lang: String): Either[String, String] =
    for
      _ <- validate(m)
      product <- m.get(productName).toRight(s"no product '$productName'")
      labelRefs <- product match
        case Cst.Node("product", List(Cst.Leaf(label), _, Cst.Node("list", prs))) =>
          Right((label, prs.collect { case Cst.Leaf(p) => p }))
        case other => Left(s"'$productName' is not a product: ${other.render}")
      label = labelRefs._1
      phraseRefs = labelRefs._2
      lines <- phraseRefs.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, pr) =>
        acc.flatMap { ls =>
          val overridden = m.defs.collectFirst {
            case (_, Cst.Node("shadow", List(Cst.Leaf(p), Cst.Leaf(ph), Cst.Leaf(text))))
              if p == productName && ph == pr => text }
          val text = overridden.orElse(phraseText(m, pr, lang))
          text.toRight(s"no phrase '$pr' (lang=$lang)").map(t =>
            ls :+ Cst.node("hazardLine", Cst.Leaf(pr), Cst.Leaf(t)))
        }
      }
      doc = Cst.node("doc", Cst.Leaf(label), Cst.Node("list", lines))
      text <- Printer.print(docGrammar, doc)
      _ <- RoundTrip.check(docGrammar, doc)
    yield text
