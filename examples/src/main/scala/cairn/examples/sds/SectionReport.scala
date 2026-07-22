package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.systemhandler.EffectContext
import cairn.user.sds.Sds

/** Report / interchange projection pack — **not** SDS language.
  *
  * SDS (`languages/sds.cairn`) is semantic documents (typed sections, outlines,
  * ΔSDS). This pack is an ordinary Cairn surface pack used *by* SDS workflows
  * to project those documents: `languages/sds-report.cairn` + surfaces under
  * `languages/sds-report/surfaces/` (`default` text, `json`, `xml`, `csv`).
  * Host maps ([[Chemicals.ChemicalDoc]]) and SDS modules both compile to the
  * same report CST. PDF remains deferred (no library). RoundTrip is the trust
  * gate — same pattern as other surface packs, not SDS object vocabulary.
  */
object SectionReport:
  private lazy val packs = PackLoader(EffectContext.forPackLoader())
  private lazy val sds = Sds(packs)
  lazy val language: ComposedLanguage = packs.requireClosed("sds-report")
  lazy val grammar: GrammarSpec = language.grammar
  lazy val jsonLanguage: ComposedLanguage = packs.requireClosed("sds-report", "json")
  lazy val jsonGrammar: GrammarSpec = jsonLanguage.grammar
  lazy val xmlLanguage: ComposedLanguage = packs.requireClosed("sds-report", "xml")
  lazy val xmlGrammar: GrammarSpec = xmlLanguage.grammar
  lazy val csvLanguage: ComposedLanguage = packs.requireClosed("sds-report", "csv")
  lazy val csvGrammar: GrammarSpec = csvLanguage.grammar

  private def listToOpt(xs: List[Cst]): Cst =
    if xs.isEmpty then Cst.Node("none", Nil)
    else Cst.Node("some", List(Cst.Node("list", xs)))

  /** Adapt a default-surface report CST (`star` → `list`) to opt-list surfaces
    * (`json` / `xml`: `opt sepby1` → `some`/`none`).
    */
  def forOptSurface(report: Cst): Either[String, Cst] = report match
    case Cst.Node("report", List(name, cas, Cst.Node("list", sections))) =>
      val secs = sections.map {
        case Cst.Node("sectionBlock", List(num, title, Cst.Node("list", fields))) =>
          Cst.Node("sectionBlock", List(num, title, listToOpt(fields)))
        case other => return Left(s"bad sectionBlock: ${other.render}")
      }
      Right(Cst.Node("report", List(name, cas, listToOpt(secs))))
    case other => Left(s"not a report CST: ${other.render}")

  /** @deprecated use [[forOptSurface]] — kept for call sites. */
  def forJsonSurface(report: Cst): Either[String, Cst] = forOptSurface(report)

  private def enFieldLines(sec: Cst): List[Cst] = sec match
    case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
      fields.collect {
        case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf("en"), Cst.Leaf(v))) =>
          Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
      }
    case Cst.Node(tag, _) if sds.typedSectionTags.contains(tag) =>
      val keys = sds.typedSectionKeys.getOrElse(tag, Nil)
      keys.flatMap(k => sds.sectionFieldText(sec, k, "en").map { v =>
        Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
      })
    case _ => Nil

  private def sectionBlockFromBody(sec: Cst): Option[Cst] =
    sds.sectionNumber(sec).map { n =>
      val title = SectionNumbering.byNumber.getOrElse(n, s"section-$n")
      Cst.node(
        "sectionBlock",
        Cst.Leaf(n.toString),
        Cst.Leaf(title),
        Cst.Node("list", enFieldLines(sec)))
    }

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

  /** Project an SDS language module with `outline` + section body defs. */
  def toCst(module: Module, outlineName: String): Either[String, Cst] =
    module.get(outlineName) match
      case Some(Cst.Node("outline", List(Cst.Leaf(name), Cst.Leaf(cas), sectionsField))) =>
        val refs = sectionsField match
          case Cst.Node("none", _) => Nil
          case Cst.Node("some", List(Cst.Node("list", rs))) => rs.collect { case Cst.Leaf(r) => r }
          case Cst.Node("list", rs) => rs.collect { case Cst.Leaf(r) => r }
          case other => return Left(s"bad outline sections: ${other.render}")
        val blocks = refs.flatMap { ref =>
          module.get(ref).flatMap(sectionBlockFromBody)
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

  def renderJson(doc: Chemicals.ChemicalDoc): Either[String, String] =
    for
      _ <- doc.validateOutline.left.map { errs =>
        errs.map(_.toString).mkString("; ")
      }
      cst <- forJsonSurface(toCst(doc))
      text <- Printer.print(jsonGrammar, cst)
      _ <- RoundTrip.check(jsonGrammar, cst)
    yield text

  def renderJson(module: Module, outlineName: String): Either[String, String] =
    for
      base <- toCst(module, outlineName)
      cst <- forOptSurface(base)
      text <- Printer.print(jsonGrammar, cst)
      _ <- RoundTrip.check(jsonGrammar, cst)
    yield text

  def renderXml(doc: Chemicals.ChemicalDoc): Either[String, String] =
    for
      _ <- doc.validateOutline.left.map { errs =>
        errs.map(_.toString).mkString("; ")
      }
      cst = toCst(doc)
      text <- Printer.print(xmlGrammar, cst)
      _ <- RoundTrip.check(xmlGrammar, cst)
    yield text

  def renderXml(module: Module, outlineName: String): Either[String, String] =
    for
      cst <- toCst(module, outlineName)
      text <- Printer.print(xmlGrammar, cst)
      _ <- RoundTrip.check(xmlGrammar, cst)
    yield text

  def renderCsv(doc: Chemicals.ChemicalDoc): Either[String, String] =
    for
      _ <- doc.validateOutline.left.map { errs =>
        errs.map(_.toString).mkString("; ")
      }
      cst = toCst(doc)
      text <- Printer.print(csvGrammar, cst)
      _ <- RoundTrip.check(csvGrammar, cst)
    yield text

  def renderCsv(module: Module, outlineName: String): Either[String, String] =
    for
      cst <- toCst(module, outlineName)
      text <- Printer.print(csvGrammar, cst)
      _ <- RoundTrip.check(csvGrammar, cst)
    yield text
end SectionReport