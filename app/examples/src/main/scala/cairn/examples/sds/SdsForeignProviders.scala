package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

/** Production foreign providers for canonical `sds-report` terms.
  *
  * JSON, XML, and report text use pack-loaded grammars. XLSX is a genuine
  * minimal OOXML workbook (inline strings, one `SDS` worksheet), while PDF
  * and SVG are deterministic projections. Every provider runs through
  * [[ForeignSurfaceRuntime]], so provenance, validation, migration evidence,
  * content addressing, and surface laws are uniform across formats.
  */
object SdsForeignProviders:
  private val language = SectionReport.language
  private val validationModel = Digest.of(Canon.cmap(
    "provider" -> Canon.CStr("sds-report-structural-v1"),
    "language" -> Canon.CStr(language.digest.hex)))

  private def provider(name: String): Digest = Digest.of(Canon.cmap(
    "provider" -> Canon.CStr(s"cairn.sds.$name.v1"),
    "language" -> Canon.CStr(language.digest.hex)))

  private def descriptor(
      name: String, format: ForeignFormat, media: String,
      direction: SurfaceDirection, law: SurfaceLaw,
  ): ForeignSurfaceDescriptor = ForeignSurfaceDescriptor(
    name, language.digest, format, media, direction, law, provider(name))

  private def validateReport(term: Cst): Either[ForeignSurfaceError, Unit] =
    def fields(value: Cst): Boolean = value match
      case Cst.Node("list" | "some", xs) => xs.forall {
        case Cst.Node("fieldLine", List(Cst.Leaf(_), Cst.Leaf(_))) => true
        case _ => false
      }
      case Cst.Node("none", Nil) => true
      case _ => false
    term match
      case Cst.Node("report", List(Cst.Leaf(_), Cst.Leaf(_), sections)) =>
        val values = sections match
          case Cst.Node("list" | "some", xs) => Some(xs)
          case Cst.Node("none", Nil) => Some(Nil)
          case _ => None
        Either.cond(values.exists(_.forall {
          case Cst.Node("sectionBlock", List(Cst.Leaf(n), Cst.Leaf(_), fs)) => n.toIntOption.exists(_ > 0) && fields(fs)
          case _ => false
        }), (), ForeignSurfaceError("invalid canonical SDS report", ForeignSourceLocation()))
      case _ => Left(ForeignSurfaceError("expected canonical SDS report", ForeignSourceLocation()))

  private def list(value: Cst): List[Cst] = value match
    case Cst.Node("list", xs) => xs
    case Cst.Node("some", List(inner)) => list(inner)
    case Cst.Node("none", Nil) => Nil
    case _ => Nil

  private def normalized(term: Cst): Cst = term match
    case Cst.Node("report", List(name, cas, sections)) => Cst.Node("report", List(name, cas,
      Cst.Node("list", list(sections).map {
        case Cst.Node("sectionBlock", List(number, title, fields)) =>
          Cst.Node("sectionBlock", List(number, title, Cst.Node("list", list(fields))))
        case other => other
      })))
    case other => other

  private def textRuntime(
      name: String, format: ForeignFormat, media: String, grammar: GrammarSpec,
      adaptForEncode: Cst => Either[String, Cst] = Right(_),
      normalizeDecoded: Cst => Cst = identity,
      rootLocation: ForeignSourceLocation,
  ): ForeignSurfaceRuntime = ForeignSurfaceRuntime(
    descriptor(name, format, media, SurfaceDirection.Encoding, SurfaceLaw.RoundTrip),
    bytes =>
      val source = String(bytes, UTF_8)
      Parser.parseFull(grammar, source)
        .left.map(e => ForeignSurfaceError(e, rootLocation))
        .map(parsed => (normalizeDecoded(parsed.cst), _ => Some(rootLocation))),
    term => adaptForEncode(term)
      .left.map(e => ForeignSurfaceError(e, rootLocation))
      .flatMap(t => Printer.print(grammar, t).left.map(e => ForeignSurfaceError(e, rootLocation)))
      .map(_.getBytes(UTF_8)),
    validateReport,
    validationModel)

  lazy val report: ForeignSurfaceRuntime = textRuntime(
    "report", ForeignFormat.Report, "text/plain; charset=utf-8", SectionReport.grammar,
    rootLocation = ForeignSourceLocation(path = Some("report")))

  lazy val json: ForeignSurfaceRuntime = textRuntime(
    "json", ForeignFormat.Json, "application/json", SectionReport.jsonGrammar,
    SectionReport.forOptSurface, normalized,
    ForeignSourceLocation(path = Some("$")))

  lazy val xml: ForeignSurfaceRuntime = textRuntime(
    "xml", ForeignFormat.Xml, "application/xml", SectionReport.xmlGrammar,
    rootLocation = ForeignSourceLocation(path = Some("/sdsReport")))

  private def xmlEscape(value: String): String = value
    .replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    .replace("\"", "&quot;").replace("'", "&apos;")

  private def xmlUnescape(value: String): String = value
    .replace("&lt;", "<").replace("&gt;", ">").replace("&quot;", "\"")
    .replace("&apos;", "'").replace("&amp;", "&")

  private def column(index: Int): String =
    var n = index + 1
    val out = StringBuilder()
    while n > 0 do
      out.insert(0, ('A' + ((n - 1) % 26)).toChar)
      n = (n - 1) / 26
    out.result()

  private def reportRows(term: Cst): Either[ForeignSurfaceError, List[List[String]]] = normalized(term) match
    case Cst.Node("report", List(Cst.Leaf(name), Cst.Leaf(cas), Cst.Node("list", sections))) =>
      val rows = List.newBuilder[List[String]]
      rows += List("SDS", "name", name)
      rows += List("SDS", "cas", cas)
      rows += List("section", "number", "title", "key", "value")
      sections.foreach {
        case Cst.Node("sectionBlock", List(Cst.Leaf(number), Cst.Leaf(title), Cst.Node("list", fields))) =>
          if fields.isEmpty then rows += List("section", number, title, "", "")
          else fields.foreach {
            case Cst.Node("fieldLine", List(Cst.Leaf(key), Cst.Leaf(value))) =>
              rows += List("section", number, title, key, value)
            case _ => ()
          }
        case _ => ()
      }
      Right(rows.result())
    case _ => Left(ForeignSurfaceError("expected canonical SDS report", ForeignSourceLocation(sheet = Some("SDS"))))

  private def zip(entries: List[(String, String)]): Array[Byte] =
    val bytes = ByteArrayOutputStream()
    val out = ZipOutputStream(bytes, UTF_8)
    entries.foreach { (name, value) =>
      out.putNextEntry(ZipEntry(name))
      out.write(value.getBytes(UTF_8))
      out.closeEntry()
    }
    out.close()
    bytes.toByteArray

  private def workbook(term: Cst): Either[ForeignSurfaceError, Array[Byte]] = reportRows(term).map { rows =>
    val sheetRows = rows.zipWithIndex.map { (values, rowIndex) =>
      val cells = values.zipWithIndex.map { (value, columnIndex) =>
        s"<c r=\"${column(columnIndex)}${rowIndex + 1}\" t=\"inlineStr\"><is><t>${xmlEscape(value)}</t></is></c>"
      }.mkString
      s"<row r=\"${rowIndex + 1}\">$cells</row>"
    }.mkString
    zip(List(
      "[Content_Types].xml" -> """<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/></Types>""",
      "_rels/.rels" -> """<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>""",
      "xl/workbook.xml" -> """<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="SDS" sheetId="1" r:id="rId1"/></sheets></workbook>""",
      "xl/_rels/workbook.xml.rels" -> """<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>""",
      "xl/worksheets/sheet1.xml" -> s"""<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>$sheetRows</sheetData></worksheet>"""))
  }

  private val cellPattern = """<c\s+r="([A-Z]+[0-9]+)"[^>]*>\s*<is>\s*<t>(.*?)</t>\s*</is>\s*</c>""".r
  private val rowPattern = """<row\s+r="([0-9]+)"[^>]*>(.*?)</row>""".r

  private def unzipSheet(bytes: Array[Byte]): Either[ForeignSurfaceError, String] =
    try
      val in = ZipInputStream(ByteArrayInputStream(bytes), UTF_8)
      var entry = in.getNextEntry
      var found: Option[String] = None
      while entry != null do
        if entry.getName == "xl/worksheets/sheet1.xml" then found = Some(String(in.readAllBytes(), UTF_8))
        in.closeEntry()
        entry = in.getNextEntry
      in.close()
      found.toRight(ForeignSurfaceError("XLSX has no SDS worksheet", ForeignSourceLocation(sheet = Some("SDS"))))
    catch case e: Exception => Left(ForeignSurfaceError(s"invalid XLSX: ${e.getMessage}",
      ForeignSourceLocation(sheet = Some("SDS"))))

  private def decodeWorkbook(bytes: Array[Byte]): Either[ForeignSurfaceError, (Cst, Cst => Option[ForeignSourceLocation])] =
    unzipSheet(bytes).flatMap { sheet =>
      val rows = rowPattern.findAllMatchIn(sheet).toList.map { row =>
        row.group(1).toInt -> cellPattern.findAllMatchIn(row.group(2)).toList.map(m => m.group(1) -> xmlUnescape(m.group(2))).toMap
      }.toMap
      def value(row: Int, col: String): Option[String] = rows.get(row).flatMap(_.get(s"$col$row"))
      (value(1, "A"), value(1, "B"), value(1, "C"), value(2, "A"), value(2, "B"), value(2, "C")) match
        case (Some("SDS"), Some("name"), Some(name), Some("SDS"), Some("cas"), Some(cas)) =>
          val sectionRows = rows.toList.sortBy(_._1).dropWhile(_._1 <= 3).foldLeft[Either[ForeignSurfaceError, List[(Int, String, String, String, String)]]](Right(Nil)) {
            case (acc, (row, _)) => for
              done <- acc
              marker <- value(row, "A").toRight(ForeignSurfaceError("missing row marker", ForeignSourceLocation(sheet = Some("SDS"), cell = Some(s"A$row"))))
              _ <- Either.cond(marker == "section", (), ForeignSurfaceError("expected section row", ForeignSourceLocation(sheet = Some("SDS"), cell = Some(s"A$row"))))
              number <- value(row, "B").toRight(ForeignSurfaceError("missing section number", ForeignSourceLocation(sheet = Some("SDS"), cell = Some(s"B$row"))))
              _ <- number.toIntOption.filter(_ > 0).toRight(ForeignSurfaceError("invalid section number", ForeignSourceLocation(sheet = Some("SDS"), cell = Some(s"B$row"))))
              title <- value(row, "C").toRight(ForeignSurfaceError("missing section title", ForeignSourceLocation(sheet = Some("SDS"), cell = Some(s"C$row"))))
            yield done :+ (row, number, title, value(row, "D").getOrElse(""), value(row, "E").getOrElse(""))
          }
          sectionRows.map { values =>
            val sections = values.groupBy(x => (x._2, x._3)).toList.sortBy(_._1._1.toInt).map { case ((number, title), fields) =>
              Cst.node("sectionBlock", Cst.Leaf(number), Cst.Leaf(title), Cst.Node("list", fields.collect {
                case (_, _, _, key, text) if key.nonEmpty => Cst.node("fieldLine", Cst.Leaf(key), Cst.Leaf(text))
              }))
            }
            val report = Cst.node("report", Cst.Leaf(name), Cst.Leaf(cas), Cst.Node("list", sections))
            val locations = values.collect { case (row, _, _, key, text) if key.nonEmpty =>
              Cst.node("fieldLine", Cst.Leaf(key), Cst.Leaf(text)) -> ForeignSourceLocation(sheet = Some("SDS"), cell = Some(s"E$row"))
            }.toMap
            (report, term => locations.get(term).orElse(Some(ForeignSourceLocation(sheet = Some("SDS")))))
          }
        case _ => Left(ForeignSurfaceError("XLSX header is not an SDS report", ForeignSourceLocation(sheet = Some("SDS"), cell = Some("A1"))))
    }

  lazy val xlsx: ForeignSurfaceRuntime = ForeignSurfaceRuntime(
    descriptor("xlsx", ForeignFormat.Xlsx,
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      SurfaceDirection.Encoding, SurfaceLaw.RoundTrip),
    decodeWorkbook, workbook, validateReport, validationModel)

  private def projection(
      name: String, format: ForeignFormat, media: String,
      encode: Cst => Either[ForeignSurfaceError, Array[Byte]],
  ): ForeignSurfaceRuntime = ForeignSurfaceRuntime(
    descriptor(name, format, media, SurfaceDirection.Projection, SurfaceLaw.Projection),
    _ => Left(ForeignSurfaceError(s"$name is export-only", ForeignSourceLocation())),
    encode, validateReport, validationModel)

  lazy val pdf: ForeignSurfaceRuntime = projection("pdf", ForeignFormat.Pdf, "application/pdf", term =>
    SectionReport.pdfLines(normalized(term))
      .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation(page = Some(1))))
      .map((title, lines) => PdfMinimal.writeText(title, lines)))

  lazy val image: ForeignSurfaceRuntime = projection("svg", ForeignFormat.Image, "image/svg+xml", term =>
    SectionReport.pdfLines(normalized(term))
      .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation(region = Some((0, 0, 960, 120)))))
      .map { (title, lines) =>
        val height = math.max(120, 42 + lines.length * 20)
        val text = lines.zipWithIndex.map { (line, index) =>
          s"<text x=\"24\" y=\"${32 + index * 20}\" font-family=\"sans-serif\" font-size=\"14\">${xmlEscape(line)}</text>"
        }.mkString
        s"""<svg xmlns="http://www.w3.org/2000/svg" role="img" aria-label="${xmlEscape(title)}" width="960" height="$height" viewBox="0 0 960 $height"><rect width="100%" height="100%" fill="white"/>$text</svg>""".getBytes(UTF_8)
      })

  lazy val runtimes: Map[String, ForeignSurfaceRuntime] = Map(
    "report" -> report, "json" -> json, "xml" -> xml, "xlsx" -> xlsx,
    "pdf" -> pdf, "image" -> image)

  lazy val bundle: ForeignSurfaceBundle = ForeignSurfaceBundle(
    language.digest, runtimes.values.toList.map(_.descriptor).sortBy(_.name))
