package cairn.ledger

import cairn.kernel.*
import cairn.core.Delta
import cairn.core.Parser

/** M37: the branch-policy language — defined in the grammar engine like any
  * other language, so ΔPolicy exists via the generic closure.
  *
  * Surface:
  *   branch <name> requires method <name> from <name>
  *   branch <name> open
  */
object PolicyLang:
  val fragment: Fragment = Fragment(
    name = "policy",
    provides = List("policy"),
    requires = Nil,
    sorts = List(SortDef("Policy", SortMode.Tree)),
    constructors = List(
      CtorDef("polReq", "Policy", List("Name", "Name", "Name")),
      CtorDef("polOpen", "Policy", List("Name"))),
    grammar = GrammarPart(
      keywords = List("branch", "requires", "method", "from", "open"),
      categories = List(CategorySpec("policyTerm", List(
        ConstructorSpec("polReq", List(
          Elem.Tok("branch"), Elem.NameLeaf, Elem.Tok("requires"), Elem.Tok("method"),
          Elem.NameLeaf, Elem.Tok("from"), Elem.NameLeaf)),
        ConstructorSpec("polOpen", List(
          Elem.Tok("branch"), Elem.NameLeaf, Elem.Tok("open")))))),
      printRules = List(
        PrintRule("polReq", List(
          PrintSeg.Lit("branch"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("requires method"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
          PrintSeg.Lit("from"), PrintSeg.Space, PrintSeg.Field(2))),
        PrintRule("polOpen", List(
          PrintSeg.Lit("branch"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("open")))),
      top = Some("policyTerm")))

  def language: ComposedLanguage =
    Compose.compose("policy", List(fragment)).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")), identity)

  def parse(src: String): Either[String, Cst] = Parser.parse(language.grammar, src)

  /** ΔPolicy — the closure holds for trust policies too (§2b). */
  def deltaPolicy: Either[String, ComposedLanguage] =
    Delta.deltaOf(language).left.map(_.map(_.render).mkString("; "))
