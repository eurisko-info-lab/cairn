package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.systemhandler.EffectContext

/** Section-report projection as an ordinary surface pack (`sds-report`).
  *
  * Language + default surface live under `languages/sds-report*`. Host maps
  * ([[Chemicals.ChemicalDoc]]) and SDS `euSection`/`outline` modules both
  * compile to the same report CST. PDF/XLS remain future surfaces — not this
  * pack. RoundTrip is the trust gate (same as product `Sds.docGrammar`).
  */
object SectionReport:
  private lazy val packs = PackLoader(EffectContext.forPackLoader())
  lazy val language: ComposedLanguage = packs.requireClosed("sds-report")
  lazy val grammar: GrammarSpec = language.grammar

  /** Build the report CST from a chemical doc (no validation). */
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

  /** Project an SDS language module with `outline` + `euSection` defs. */
  def toCst(module: Module, outlineName: String): Either[String, Cst] =
    module.get(outlineName) match
      case Some(Cst.Node("outline", List(Cst.Leaf(name), Cst.Leaf(cas), sectionsField))) =>
        val refs = sectionsField match
          case Cst.Node("none", _) => Nil
          case Cst.Node("some", List(Cst.Node("list", rs))) => rs.collect { case Cst.Leaf(r) => r }
          case Cst.Node("list", rs) => rs.collect { case Cst.Leaf(r) => r }
          case other => return Left(s"bad outline sections: ${other.render}")
        val blocks = refs.flatMap { ref =>
          module.get(ref).collect {
            case Cst.Node("euSection", List(Cst.Leaf(num), Cst.Node("list", fields))) =>
              val title = SectionNumbering.byNumber.getOrElse(num.toIntOption.getOrElse(-1), s"section-$num")
              val fieldLines = fields.collect {
                case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf("en"), Cst.Leaf(v))) =>
                  Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
              }
              Cst.node("sectionBlock", Cst.Leaf(num), Cst.Leaf(title), Cst.Node("list", fieldLines))
          }
        }
        Right(Cst.node("report", Cst.Leaf(name), Cst.Leaf(cas), Cst.Node("list", blocks)))
      case Some(other) => Left(s"'$outlineName' is not an outline: ${other.render}")
      case None => Left(s"no outline '$outlineName'")

  def render(doc: Chemicals.ChemicalDoc): Either[String, String] =
    for
      _ <- doc.validateOutline.left.map { errs =>
        errs.map(_.toString).mkString("; ")
      }
      cst = toCst(doc)
      text <- Printer.print(grammar, cst)
      _ <- RoundTrip.check(grammar, cst)
    yield text

  def render(module: Module, outlineName: String): Either[String, String] =
    for
      cst <- toCst(module, outlineName)
      text <- Printer.print(grammar, cst)
      _ <- RoundTrip.check(grammar, cst)
    yield text
