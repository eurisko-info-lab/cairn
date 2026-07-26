package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.sds.{Chemicals, SectionReport, SdsForeignProviders}
import java.nio.charset.StandardCharsets.UTF_8

class SdsForeignProviderSuite extends munit.FunSuite:
  private val report = SectionReport.toCst(Chemicals.Acetone.thinModule, "acetoneOutline")
    .fold(e => fail(e), identity)

  test("PR19 provider bundle binds every production surface to sds-report"):
    val bundle = SdsForeignProviders.bundle
    assertEquals(bundle.validate, Right(()))
    assertEquals(SdsForeignProviders.runtimes.keySet,
      Set("report", "json", "xml", "xlsx", "pdf", "image"))
    assertEquals(bundle.surfaces.map(_.language).distinct, List(SectionReport.language.digest))
    assertEquals(bundle.surfaces.map(_.format).toSet, Set(
      ForeignFormat.Report, ForeignFormat.Json, ForeignFormat.Xml,
      ForeignFormat.Xlsx, ForeignFormat.Pdf, ForeignFormat.Image))
    assertEquals(bundle.artifact.kind, ArtifactKind.ForeignSurface)

  test("JSON, XML, and report text import to one canonical semantic result"):
    List("json", "xml", "report").foreach { name =>
      val runtime = SdsForeignProviders.runtimes(name)
      val projected = runtime.project(report).fold(e => fail(e.toString), identity)
      val imported = runtime.importBytes(s"acetone.$name", projected.bytes).fold(e => fail(e.toString), identity)
      assertEquals(imported.semantic, report, name)
      assertEquals(imported.evidence.source, Some(imported.source.digest), name)
      assertEquals(imported.evidence.law, SurfaceLaw.RoundTrip, name)
      assertEquals(projected.evidence.output, Some(Digest.ofBytes(projected.bytes)), name)
    }

  test("XLSX is genuine OOXML, round-trips, and retains workbook coordinates"):
    val runtime = SdsForeignProviders.xlsx
    val projected = runtime.project(report).fold(e => fail(e.toString), identity)
    assertEquals(projected.bytes.take(2).toList, List('P'.toByte, 'K'.toByte))
    val imported = runtime.importBytes("acetone.xlsx", projected.bytes).fold(e => fail(e.toString), identity)
    assertEquals(imported.semantic, report)
    assertEquals(imported.source.mediaType,
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

    val bad = report match
      case Cst.Node("report", List(name, cas, Cst.Node("list", first :: rest))) =>
        val changed = first match
          case Cst.Node("sectionBlock", List(_, title, fields)) => Cst.node("sectionBlock", Cst.Leaf("not-a-number"), title, fields)
          case other => other
        Cst.node("report", name, cas, Cst.Node("list", changed :: rest))
      case other => other
    val badWorkbook = runtime.encode(bad).fold(e => fail(e.toString), identity)
    val error = runtime.importBytes("bad.xlsx", badWorkbook).left.toOption.get
    assertEquals(error.location.sheet, Some("SDS"))
    assertEquals(error.location.cell, Some("B4"))

  test("PDF and SVG are deterministic projections with surface evidence"):
    val pdf = SdsForeignProviders.pdf.project(report).fold(e => fail(e.toString), identity)
    assert(PdfMinimal.isPdf(pdf.bytes))
    assertEquals(pdf.evidence.law, SurfaceLaw.Projection)
    assertEquals(pdf.evidence.output, Some(Digest.ofBytes(pdf.bytes)))

    val image = SdsForeignProviders.image.project(report).fold(e => fail(e.toString), identity)
    val svg = String(image.bytes, UTF_8)
    assert(svg.startsWith("<svg"))
    assert(svg.contains("Acetone"))
    assertEquals(image.evidence.law, SurfaceLaw.Projection)
    assertEquals(SdsForeignProviders.image.project(report).map(_.bytes.toList), Right(image.bytes.toList))

  test("semantic-first migration re-encodes through a different production provider"):
    val json = SdsForeignProviders.json
    val sourceBytes = json.project(report).toOption.get.bytes
    val imported = json.importBytes("acetone.json", sourceBytes).toOption.get
    val migration = Artifact(ArtifactKind.Migration, Canon.cmap(
      "from" -> Canon.CStr(SectionReport.language.digest.hex),
      "to" -> Canon.CStr(SectionReport.language.digest.hex)))
    val migrated = json.migrate(imported, migration, SdsForeignProviders.xlsx, Right(_))
      .fold(e => fail(e.toString), identity)
    assertEquals(migrated.evidence.migrations, List(migration.digest))
    assertEquals(SdsForeignProviders.xlsx.importBytes("migrated.xlsx", migrated.bytes).map(_.semantic), Right(report))
