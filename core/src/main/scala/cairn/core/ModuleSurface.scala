package cairn.core

import cairn.kernel.*

/** The generic module-file text surface of any composed language: `name = <term> ;`
  * lines derived mechanically, no per-language code (M44). Lives in `core` because
  * it only touches `ComposedLanguage`/`GrammarSpec`/`Cst`/`Module` — pure proposal
  * machinery — and format-preserving `Delta` application needs it without pulling
  * in the surface layer.
  */
object ModuleSurface:
  /** The module-file grammar of a language: `def* ` with def := name = term ; */
  def grammar(l: ComposedLanguage): GrammarSpec =
    val g = l.grammar
    g.copy(
      name = s"${l.name}-module",
      tokens = g.tokens.copy(puncts = ("=" :: ";" :: g.tokens.puncts).distinct),
      categories = CategorySpec("moduleFile", List(
        ConstructorSpec("moduleFile", List(Elem.Star(Elem.Cat("moduleDef")))))) ::
        CategorySpec("moduleDef", List(
          ConstructorSpec("moduleDef", List(
            Elem.NameLeaf, Elem.Tok("="), Elem.Cat(g.top), Elem.Tok(";"))))) :: g.categories,
      printRules =
        PrintRule("moduleFile", List(PrintSeg.SepFields(0, "\n"))) ::
        PrintRule("moduleDef", List(
          PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("="), PrintSeg.Space,
          PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(";"))) :: g.printRules,
      top = "moduleFile")

  def toModule(cst: Cst): Either[String, Module] = cst match
    case Cst.Node("moduleFile", List(Cst.Node("list", defs))) =>
      defs.foldLeft[Either[String, List[(String, Cst)]]](Right(Nil)) {
        case (acc, Cst.Node("moduleDef", List(Cst.Leaf(n), t))) => acc.map(_ :+ (n, t))
        case (_, other) => Left(s"not a module def: ${other.render}")
      }.map(Module(_))
    case other => Left(s"not a module file: ${other.render}")

  def fromModule(m: Module): Cst =
    Cst.node("moduleFile", Cst.Node("list",
      m.defs.map((n, t) => Cst.node("moduleDef", Cst.Leaf(n), t))))
