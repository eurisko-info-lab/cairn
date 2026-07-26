package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

class ForeignSurfaceSuite extends munit.FunSuite:
  private val language = Stlc.language
  private val term = Stlc.tru
  private val validation = Digest.of(Canon.CStr("foreign-test-validation"))

  test("standard JSON import records source provenance, validation, canonical result, and round-trip law"):
    val runtime = ForeignSurfaces.standard(language)("json")
    val bytes = JsonSurface.encode(term).toOption.get.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val imported = runtime.importBytes("upload.json", bytes).fold(e => fail(e.toString), identity)
    assertEquals(imported.semantic, term)
    assertEquals(imported.evidence.source, Some(imported.source.digest))
    assertEquals(imported.source.artifact.kind, ArtifactKind.Source)
    assertEquals(imported.evidence.semanticResult, Artifact(ArtifactKind.Term, Cst.toCanon(term)).digest)
    assertEquals(imported.evidence.validation, runtime.validationModel)
    assertEquals(imported.evidence.law, SurfaceLaw.RoundTrip)
    assertEquals(imported.evidence.artifact.kind, ArtifactKind.SurfaceEvidence)
    assertEquals(SurfaceEvidence.fromCanon(imported.evidence.canon), Right(imported.evidence))
    assertEquals(ForeignSurfaceDescriptor.fromCanon(runtime.descriptor.canon), Right(runtime.descriptor))

  test("foreign validation errors retain workbook coordinates"):
    val descriptor = ForeignSurfaceDescriptor(
      "sheet", language.digest, ForeignFormat.Xlsx,
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      SurfaceDirection.Encoding, SurfaceLaw.RoundTrip, Digest.of(Canon.CStr("xlsx-test-provider")))
    val atB7 = ForeignSourceLocation(sheet = Some("Mixture"), cell = Some("B7"))
    val runtime = ForeignSurfaceRuntime(
      descriptor,
      _ => Right((Cst.node("invalid"), _ => Some(atB7))),
      _ => Right(Array[Byte](1, 2, 3)),
      _ => Left(ForeignSurfaceError("percentage is invalid", ForeignSourceLocation())),
      validation)
    val error = runtime.importBytes("mixture.xlsx", Array[Byte](9)).left.toOption.get
    assertEquals(error.location.sheet, Some("Mixture"))
    assertEquals(error.location.cell, Some("B7"))

  test("all constitutional foreign formats are language-bound surface descriptors"):
    val formats = ForeignFormat.values.toList
    val descriptors = formats.map { format =>
      ForeignSurfaceDescriptor(format.toString.toLowerCase, language.digest, format,
        s"application/x-${format.toString.toLowerCase}", SurfaceDirection.Projection,
        SurfaceLaw.Projection, Digest.of(Canon.CStr(s"provider-$format")))
    }
    val bundle = ForeignSurfaceBundle(language.digest, descriptors)
    assertEquals(bundle.validate, Right(()))
    assertEquals(descriptors.map(_.format).toSet, Set(
      ForeignFormat.Text, ForeignFormat.Json, ForeignFormat.Xml, ForeignFormat.Xls,
      ForeignFormat.Xlsx, ForeignFormat.Pdf, ForeignFormat.Image, ForeignFormat.Report))
    assertEquals(bundle.artifact.kind, ArtifactKind.ForeignSurface)

  test("projection law is deterministic and migration re-encodes the transported canonical term"):
    def projection(name: String, lang: Digest): ForeignSurfaceRuntime =
      val descriptor = ForeignSurfaceDescriptor(name, lang, ForeignFormat.Pdf, "application/pdf",
        SurfaceDirection.Projection, SurfaceLaw.Projection, Digest.of(Canon.CStr(s"$name-provider")))
      ForeignSurfaceRuntime(
        descriptor,
        _ => Left(ForeignSurfaceError("projection is export-only", ForeignSourceLocation(page = Some(1)))),
        c => Right(Canon.encode(Cst.toCanon(c))), _ => Right(()), validation)

    val source = ForeignSurfaceRuntime(
      ForeignSurfaceDescriptor("source-json", language.digest, ForeignFormat.Json, "application/json",
        SurfaceDirection.Encoding, SurfaceLaw.RoundTrip, Digest.of(Canon.CStr("source-provider"))),
      bytes => Canon.decode(bytes).map(Cst.fromCanon)
        .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation(byteStart = Some(0))))
        .map(c => (c, _ => Some(ForeignSourceLocation(byteStart = Some(0))))),
      c => Right(Canon.encode(Cst.toCanon(c))), _ => Right(()), validation)
    val imported = source.importBytes("source.json", Canon.encode(Cst.toCanon(term))).toOption.get
    val targetLanguage = Compose.compose("stlc-foreign-v2",
      language.fragments :+ Fragment("foreign-v2", List("foreign-v2"), Nil)).toOption.get
    val target = projection("pdf", targetLanguage.digest)
    val migration = Artifact(ArtifactKind.Migration, Canon.CStr("stlc foreign surface v1 to v2"))
    val projected = source.migrate(imported, migration, target, c => Right(c)).fold(e => fail(e.toString), identity)
    assertEquals(projected.evidence.language, targetLanguage.digest)
    assertEquals(projected.evidence.migrations, List(migration.digest))
    assertEquals(projected.evidence.output, Some(Digest.ofBytes(projected.bytes)))
    assertEquals(projected.evidence.law, SurfaceLaw.Projection)

    var flip = false
    val nondeterministic = target.copy(encode = _ => { flip = !flip; Right(Array((if flip then 1 else 2).toByte)) })
    assert(nondeterministic.project(term).left.exists(_.message.contains("not deterministic")))
