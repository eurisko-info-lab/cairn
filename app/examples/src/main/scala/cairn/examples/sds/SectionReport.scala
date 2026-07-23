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
  * `languages/sds-report/surfaces/` (`default` text, `json`, `xml`, `csv`,
  * `pdf`). Host maps ([[Chemicals.ChemicalDoc]]) and SDS modules both compile
  * to the same report CST via [[toCst]] (host projection residual). Printing
  * goes through [[printSurface]] — PackLoader surface bind + RoundTrip — so
  * encodings load without recompiling Scala. [[renderPdf]] emits minimal PDF
  * 1.4 bytes via [[PdfMinimal]] (pure writer; the `pdf` surface is the
  * RoundTrip-able text twin). RoundTrip is the trust gate for text surfaces.
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
  lazy val pdfLanguage: ComposedLanguage = packs.requireClosed("sds-report", "pdf")
  lazy val pdfGrammar: GrammarSpec = pdfLanguage.grammar

  private def listToOpt(xs: List[Cst]): Cst =
    if xs.isEmpty then Cst.Node("none", Nil)
    else Cst.Node("some", List(Cst.Node("list", xs)))

  /** Adapt a default-surface report CST (`star` → `list`) to opt-list surfaces
    * (`json` / `xml`: `opt sepby1` → `some`/`none`).
    */
  def forOptSurface(report: Cst): Either[String, Cst] = report match
    case Cst.Node("report", List(name, cas, Cst.Node("list", sections))) =>
      val secsE = sections.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, sec) =>
        acc.flatMap { done =>
          sec match
            case Cst.Node("sectionBlock", List(num, title, Cst.Node("list", fields))) =>
              Right(done :+ Cst.Node("sectionBlock", List(num, title, listToOpt(fields))))
            case other => Left(s"bad sectionBlock: ${other.render}")
        }
      }
      secsE.map(secs => Cst.Node("report", List(name, cas, listToOpt(secs))))
    case other => Left(s"not a report CST: ${other.render}")

  /** @deprecated use [[forOptSurface]] — kept for call sites. */
  def forJsonSurface(report: Cst): Either[String, Cst] = forOptSurface(report)

  private def fieldLines(sec: Cst, lang: String): List[Cst] = sec match
    case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
      val preferred = fields.collect {
        case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(v))) if l == lang =>
          k -> v
      }.toMap
      val en = fields.collect {
        case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf("en"), Cst.Leaf(v))) =>
          k -> v
      }.toMap
      (en.keySet ++ preferred.keySet).toList.sorted.map { k =>
        val v = preferred.getOrElse(k, en(k))
        Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
      }
    case Cst.Node(tag, _) if sds.typedSectionTags.contains(tag) =>
      val keys = sds.typedSectionKeys.getOrElse(tag, Nil)
      keys.flatMap(k => sds.sectionFieldText(sec, k, lang).map { v =>
        Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
      })
    case _ => Nil

  private def sectionBlockFromBody(sec: Cst, lang: String = "en"): Option[Cst] =
    sds.sectionNumber(sec).map { n =>
      val title = SectionNumbering.byNumber.getOrElse(n, s"section-$n")
      Cst.node(
        "sectionBlock",
        Cst.Leaf(n.toString),
        Cst.Leaf(title),
        Cst.Node("list", fieldLines(sec, lang)))
    }

  /** Build the report CST from a chemical doc (no validation). Host maps are
    * EN-primary; use [[toCst]](module, …, lang) for locale-aware projection.
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

  /** Project an SDS language module with `outline` + section body defs.
    * [[lang]] selects locale via [[Sds.sectionFieldText]] (corpus refs + shadows).
    */
  def toCst(module: Module, outlineName: String, lang: String = "en"): Either[String, Cst] =
    module.get(outlineName) match
      case Some(Cst.Node("outline", List(Cst.Leaf(name), Cst.Leaf(cas), sectionsField))) =>
        val refs = sectionsField match
          case Cst.Node("none", _) => Nil
          case Cst.Node("some", List(Cst.Node("list", rs))) => rs.collect { case Cst.Leaf(r) => r }
          case Cst.Node("list", rs) => rs.collect { case Cst.Leaf(r) => r }
          case other => return Left(s"bad outline sections: ${other.render}")
        val blocks = refs.flatMap { ref =>
          module.get(ref).flatMap { sec =>
            sds.sectionNumber(sec).map { n =>
              val title = SectionNumbering.byNumber.getOrElse(n, s"section-$n")
              val keys = sec match
                case Cst.Node(tag, _) if sds.typedSectionTags.contains(tag) =>
                  sds.typedSectionKeys.getOrElse(tag, Nil)
                case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
                  fields.collect {
                    case Cst.Node("sectionField" | "sectionFieldRef", List(Cst.Leaf(k), _, _)) => k
                  }.distinct
                case _ => Nil
              val lines = keys.flatMap { k =>
                sds.sectionFieldText(module, ref, k, lang).map { v =>
                  Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
                }
              }
              Cst.node(
                "sectionBlock",
                Cst.Leaf(n.toString),
                Cst.Leaf(title),
                Cst.Node("list", lines))
            }
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

  def render(module: Module, outlineName: String, lang: String = "en"): Either[String, String] =
    for
      cst <- toCst(module, outlineName, lang)
      text <- printSurface("default", cst)
    yield text

  /** Thin host printer: PackLoader surface bind + RoundTrip.print.
    * Projection CST building ([[toCst]]) remains host; encodings are
    * ordinary `sds-report` surfaces loadable without recompiling Scala.
    */
  def printSurface(surface: String, report: Cst): Either[String, String] =
    val g = packs.requireClosed("sds-report", surface).grammar
    // json uses `opt sepby1` (some/none); xml/csv/default use `star`/`list`.
    val adapted = if surface == "json" then forOptSurface(report) else Right(report)
    for
      cst <- adapted
      text <- Printer.print(g, cst)
      _ <- RoundTrip.check(g, cst)
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

  def renderJson(module: Module, outlineName: String, lang: String = "en"): Either[String, String] =
    for
      base <- toCst(module, outlineName, lang)
      text <- printSurface("json", base)
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

  def renderXml(module: Module, outlineName: String, lang: String = "en"): Either[String, String] =
    for
      cst <- toCst(module, outlineName, lang)
      text <- printSurface("xml", cst)
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

  def renderCsv(module: Module, outlineName: String, lang: String = "en"): Either[String, String] =
    for
      cst <- toCst(module, outlineName, lang)
      text <- printSurface("csv", cst)
    yield text

  /** Text twin of the PDF projection (`pdf` surface) — RoundTrip trust gate. */
  def renderPdfSurface(module: Module, outlineName: String, lang: String = "en"): Either[String, String] =
    for
      cst <- toCst(module, outlineName, lang)
      text <- printSurface("pdf", cst)
    yield text

  /** Flatten a report CST to title + body lines for [[PdfMinimal]]. */
  def pdfLines(report: Cst): Either[String, (String, List[String])] = report match
    case Cst.Node("report", List(Cst.Leaf(name), Cst.Leaf(cas), Cst.Node("list", sections))) =>
      val lines = List.newBuilder[String]
      lines += s"SDS REPORT: $name"
      lines += s"CAS: $cas"
      lines += ""
      sections.foreach {
        case Cst.Node("sectionBlock", List(Cst.Leaf(num), Cst.Leaf(title), Cst.Node("list", fields))) =>
          lines += s"$num. $title"
          fields.foreach {
            case Cst.Node("fieldLine", List(Cst.Leaf(k), Cst.Leaf(v))) =>
              lines += s"  $k: $v"
            case _ => ()
          }
          lines += ""
        case _ => ()
      }
      Right((name, lines.result()))
    case other => Left(s"not a report CST: ${other.render}")

  /** Minimal PDF 1.4 bytes from an SDS outline — projection used *by* SDS,
    * not SDS vocabulary. Companion text surface: [[renderPdfSurface]].
    */
  def renderPdf(module: Module, outlineName: String, lang: String = "en"): Either[String, Array[Byte]] =
    for
      cst <- toCst(module, outlineName, lang)
      pair <- pdfLines(cst)
    yield
      val (title, lines) = pair
      PdfMinimal.writeText(title, lines)

  def renderPdf(doc: Chemicals.ChemicalDoc): Either[String, Array[Byte]] =
    for
      _ <- doc.validateOutline.left.map(_.map(_.toString).mkString("; "))
      pair <- pdfLines(toCst(doc))
    yield
      val (title, lines) = pair
      PdfMinimal.writeText(title, lines)
end SectionReport