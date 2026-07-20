package cairn.examples.sds

import cairn.kernel.*
import cairn.workbench.*

/** SDS pack (M47, §5b): Safety Data Sheet authoring — a real (thin) slice of
  * GRANITE's flagship domain. An SDS is NOT a flat document: it is a compiled
  * view over typed objects (substances, mixtures, phrases, products, shadows).
  *
  * ΔSDS = the generic ΔL plus DOMAIN validation (mixture percentages, broken
  * references) enforced on every application. The rendered document is itself
  * a bidirectional surface: render output re-parses.
  */
object Sds:
  val objects: Fragment = Fragment(
    name = "sds-objects",
    provides = List("sds"),
    requires = Nil,
    sorts = List(SortDef("SdsObj", SortMode.Tree)),
    constructors = List(
      CtorDef("substance", "SdsObj", List("Cas", "Label")),
      CtorDef("phrase", "SdsObj", List("Name", "Lang", "Text")),
      CtorDef("component", "Component", List("Ref", "Pct")),
      CtorDef("mixture", "SdsObj", List("Components")),
      CtorDef("product", "SdsObj", List("Label", "Ref", "PhraseRefs")),
      CtorDef("shadow", "SdsObj", List("Ref", "Ref", "Text"))),
    varCtor = Some("sdsRef"),
    grammar = GrammarPart(
      keywords = List("substance", "phrase", "lang", "text", "mixture", "of",
        "pct", "product", "uses", "phrases", "shadow", "overrides", "with"),
      puncts = List("(", ")", ","),
      categories = List(CategorySpec("sdsObj", List(
        ConstructorSpec("substance", List(
          Elem.Tok("substance"), Elem.StrLeaf, Elem.StrLeaf)),
        ConstructorSpec("phrase", List(
          Elem.Tok("phrase"), Elem.NameLeaf, Elem.Tok("lang"), Elem.NameLeaf,
          Elem.Tok("text"), Elem.StrLeaf)),
        ConstructorSpec("mixture", List(
          Elem.Tok("mixture"), Elem.Tok("of"), Elem.Tok("("),
          Elem.SepBy1(Elem.Cat("component"), ","), Elem.Tok(")"))),
        ConstructorSpec("product", List(
          Elem.Tok("product"), Elem.StrLeaf, Elem.Tok("uses"), Elem.NameLeaf,
          Elem.Tok("phrases"), Elem.Tok("("), Elem.SepBy1(Elem.NameLeaf, ","), Elem.Tok(")"))),
        ConstructorSpec("shadow", List(
          Elem.Tok("shadow"), Elem.NameLeaf, Elem.Tok("overrides"), Elem.NameLeaf,
          Elem.Tok("with"), Elem.StrLeaf)))),
        CategorySpec("component", List(
          ConstructorSpec("component", List(Elem.NameLeaf, Elem.Tok("pct"), Elem.NumLeaf))))),
      printRules = List(
        PrintRule("substance", List(
          PrintSeg.Lit("substance"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.StrField(1))),
        PrintRule("phrase", List(
          PrintSeg.Lit("phrase"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("lang"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
          PrintSeg.Lit("text"), PrintSeg.Space, PrintSeg.StrField(2))),
        PrintRule("mixture", List(
          PrintSeg.Lit("mixture of ("), PrintSeg.Field(0), PrintSeg.Lit(")"))),
        PrintRule("component", List(
          PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("pct"), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("product", List(
          PrintSeg.Lit("product"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space,
          PrintSeg.Lit("uses"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
          PrintSeg.Lit("phrases ("), PrintSeg.Field(2), PrintSeg.Lit(")"))),
        PrintRule("shadow", List(
          PrintSeg.Lit("shadow"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("overrides"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
          PrintSeg.Lit("with"), PrintSeg.Space, PrintSeg.StrField(2)))),
      top = Some("sdsObj")))

  lazy val language: ComposedLanguage =
    Compose.compose("sds", List(objects)).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")), identity)

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
      case _ => ()
    val es = errs.result()
    if es.isEmpty then Right(()) else Left(es.mkString("; "))

  /** ΔSDS application: the generic ΔL, then the domain gate. */
  def applySds(m: Module, change: Cst): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, m, change).flatMap { out =>
      validate(out._1).map(_ => out) }

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

  /** Compile a product's document: phrases in the requested language, with
    * shadow overrides applied (a view over typed objects, not stored text).
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
          // shadow override wins over the library phrase
          val overridden = m.defs.collectFirst {
            case (_, Cst.Node("shadow", List(Cst.Leaf(p), Cst.Leaf(ph), Cst.Leaf(text))))
              if p == productName && ph == pr => text }
          val text = overridden.orElse(m.get(pr).collect {
            case Cst.Node("phrase", List(_, Cst.Leaf(l), Cst.Leaf(t))) if l == lang => t })
          text.toRight(s"no '$lang' phrase '$pr'").map(t =>
            ls :+ Cst.node("hazardLine", Cst.Leaf(pr), Cst.Leaf(t)))
        }
      }
      doc = Cst.node("doc", Cst.Leaf(label), Cst.Node("list", lines))
      text <- Printer.print(docGrammar, doc)
      _ <- RoundTrip.check(docGrammar, doc) // rendered documents re-parse (law)
    yield text
