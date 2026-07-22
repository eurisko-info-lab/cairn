package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*

/** Thin report projection over host [[Chemicals.ChemicalDoc]] section maps.
  *
  * Compiles an outline-validated chemical document to a bidirectional
  * `GrammarSpec` surface (`sds-section-report`) — same RoundTrip trust gate as
  * `Sds.docGrammar`, but for EU-CLP section bodies rather than product hazard
  * lines. Host-side only: section maps are not yet `sds.cairn` constructors /
  * ΔSDS-editable fields / multilingual / Studio.
  *
  * See STATUS-2 / docs/exemplars remaining gaps.
  */
object SectionReport:
  val grammar: GrammarSpec = GrammarSpec(
    name = "sds-section-report",
    tokens = TokenSpec(
      keywords = List("SDS", "REPORT", "CAS", "section", "field"),
      puncts = List(":"),
      lineComment = None),
    categories = List(
      CategorySpec("report", List(
        ConstructorSpec("report", List(
          Elem.Tok("SDS"), Elem.Tok("REPORT"), Elem.Tok(":"), Elem.StrLeaf,
          Elem.Tok("CAS"), Elem.Tok(":"), Elem.StrLeaf,
          Elem.Star(Elem.Cat("sectionBlock")))))),
      CategorySpec("sectionBlock", List(
        ConstructorSpec("sectionBlock", List(
          Elem.Tok("section"), Elem.NumLeaf, Elem.StrLeaf,
          Elem.Star(Elem.Cat("fieldLine")))))),
      CategorySpec("fieldLine", List(
        ConstructorSpec("fieldLine", List(
          Elem.Tok("field"), Elem.NameLeaf, Elem.Tok(":"), Elem.StrLeaf))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("report", List(
        PrintSeg.Lit("SDS"), PrintSeg.Space, PrintSeg.Lit("REPORT"),
        PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.StrField(0),
        PrintSeg.Space, PrintSeg.Lit("CAS"), PrintSeg.Lit(":"), PrintSeg.Space,
        PrintSeg.StrField(1), PrintSeg.Newline,
        PrintSeg.SepFields(2, "\n"))),
      PrintRule("sectionBlock", List(
        PrintSeg.Lit("section"), PrintSeg.Space, PrintSeg.Field(0),
        PrintSeg.Space, PrintSeg.StrField(1), PrintSeg.Newline,
        PrintSeg.SepFields(2, "\n"))),
      PrintRule("fieldLine", List(
        PrintSeg.Lit("field"), PrintSeg.Space, PrintSeg.Field(0),
        PrintSeg.Space, PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.StrField(1)))),
    top = "report")

  /** Build the report CST from a chemical doc (no validation). Fields within
    * each section keep map iteration order; sections ascend by number.
    */
  def toCst(doc: Chemicals.ChemicalDoc): Cst =
    val sections = doc.populatedNumbers.map { n =>
      val body = doc.sections(n)
      val fields = body.fields.toList.map { case (k, v) =>
        Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
      }
      Cst.node(
        "sectionBlock",
        Cst.Leaf(n.toString),
        Cst.Leaf(body.title),
        Cst.Node("list", fields))
    }
    Cst.node(
      "report",
      Cst.Leaf(doc.name),
      Cst.Leaf(doc.cas),
      Cst.Node("list", sections))

  /** Validate EU-CLP outline, print, and RoundTrip-check. */
  def render(doc: Chemicals.ChemicalDoc): Either[String, String] =
    for
      _ <- doc.validateOutline.left.map { errs =>
        errs.map(_.toString).mkString("; ")
      }
      cst = toCst(doc)
      text <- Printer.print(grammar, cst)
      _ <- RoundTrip.check(grammar, cst)
    yield text
