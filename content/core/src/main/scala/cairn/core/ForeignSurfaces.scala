package cairn.core

import cairn.kernel.*

/** Foreign representations are language-bound surfaces, irrespective of
  * whether their carrier is text, a workbook, pages, or pixels. */
enum ForeignFormat:
  case Text, Json, Xml, Xls, Xlsx, Pdf, Image, Report
  def canon: Canon = Canon.CStr(toString.toLowerCase)

enum SurfaceDirection:
  case Encoding, Projection
  def canon: Canon = Canon.CStr(toString.toLowerCase)

enum SurfaceLaw:
  case RoundTrip, Projection
  def canon: Canon = Canon.CStr(toString.toLowerCase)

/** A coordinate in the original carrier. Formats may add coordinates without
  * changing the error protocol (cell, page, image region, JSON pointer, …). */
final case class ForeignSourceLocation(
    path: Option[String] = None,
    byteStart: Option[Int] = None,
    byteEnd: Option[Int] = None,
    sheet: Option[String] = None,
    cell: Option[String] = None,
    page: Option[Int] = None,
    region: Option[(Int, Int, Int, Int)] = None,
):
  def canon: Canon = Canon.cmap(
    "path" -> path.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))),
    "byteStart" -> byteStart.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CInt(x))),
    "byteEnd" -> byteEnd.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CInt(x))),
    "sheet" -> sheet.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))),
    "cell" -> cell.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))),
    "page" -> page.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CInt(x))),
    "region" -> region.fold[Canon](Canon.CTag("none", Canon.CInt(0))) { case (x, y, w, h) =>
      Canon.CTag("some", Canon.CList(List(x, y, w, h).map(Canon.CInt(_)))) })

final case class ForeignSurfaceError(message: String, location: ForeignSourceLocation):
  def canon: Canon = Canon.cmap("message" -> Canon.CStr(message), "location" -> location.canon)

/** Canonical identity of a surface implementation. `provider` identifies the
  * codec/projection model; host closures are never used as identity. */
final case class ForeignSurfaceDescriptor(
    name: String,
    language: Digest,
    format: ForeignFormat,
    mediaType: String,
    direction: SurfaceDirection,
    law: SurfaceLaw,
    provider: Digest,
):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name), "language" -> Canon.CStr(language.hex),
    "format" -> format.canon, "mediaType" -> Canon.CStr(mediaType),
    "direction" -> direction.canon, "law" -> law.canon,
    "provider" -> Canon.CStr(provider.hex))
  def artifact: Artifact = Artifact(ArtifactKind.ForeignSurface, canon)
  def digest: Digest = artifact.digest

object ForeignSurfaceDescriptor:
  def fromCanon(c: Canon): Either[String, ForeignSurfaceDescriptor] =
    try
      def parseEnum[A](values: Array[A], value: String): A =
        values.find(_.toString.equalsIgnoreCase(value)).getOrElse(throw CodecError(s"unknown enum '$value'"))
      Right(ForeignSurfaceDescriptor(
        c.field("name").asStr, Digest(c.field("language").asStr),
        parseEnum(ForeignFormat.values, c.field("format").asStr), c.field("mediaType").asStr,
        parseEnum(SurfaceDirection.values, c.field("direction").asStr),
        parseEnum(SurfaceLaw.values, c.field("law").asStr), Digest(c.field("provider").asStr)))
    catch case e: Exception => Left(s"invalid foreign surface descriptor: ${e.getMessage}")

final case class ForeignSource(
    surface: Digest, origin: String, bytes: Array[Byte], mediaType: String,
):
  def canon: Canon = Canon.cmap(
    "surface" -> Canon.CStr(surface.hex), "origin" -> Canon.CStr(origin),
    "mediaType" -> Canon.CStr(mediaType), "bytes" -> Canon.CBytes(bytes.toVector))
  def artifact: Artifact = Artifact(ArtifactKind.Source, Canon.CTag("foreign-source", canon))
  def digest: Digest = artifact.digest

final case class SurfaceEvidence(
    language: Digest,
    surface: Digest,
    source: Option[Digest],
    semanticResult: Digest,
    validation: Digest,
    law: SurfaceLaw,
    output: Option[Digest],
    migrations: List[Digest],
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex), "surface" -> Canon.CStr(surface.hex),
    "source" -> source.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "semanticResult" -> Canon.CStr(semanticResult.hex), "validation" -> Canon.CStr(validation.hex),
    "law" -> law.canon,
    "output" -> output.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "migrations" -> Canon.cstrs(migrations.map(_.hex)))
  def artifact: Artifact = Artifact(ArtifactKind.SurfaceEvidence, canon)
  def digest: Digest = artifact.digest

object SurfaceEvidence:
  def fromCanon(c: Canon): Either[String, SurfaceEvidence] =
    try
      def optional(field: String): Option[Digest] = c.field(field) match
        case Canon.CTag("some", Canon.CStr(value)) => Some(Digest(value))
        case _ => None
      val law = SurfaceLaw.values.find(_.toString.equalsIgnoreCase(c.field("law").asStr))
        .getOrElse(throw CodecError("unknown surface law"))
      Right(SurfaceEvidence(
        Digest(c.field("language").asStr), Digest(c.field("surface").asStr), optional("source"),
        Digest(c.field("semanticResult").asStr), Digest(c.field("validation").asStr), law,
        optional("output"), c.field("migrations").asList.map(x => Digest(x.asStr))))
    catch case e: Exception => Left(s"invalid surface evidence: ${e.getMessage}")

final case class ImportedSurface(source: ForeignSource, semantic: Cst, evidence: SurfaceEvidence)
final case class ProjectedSurface(bytes: Array[Byte], evidence: SurfaceEvidence)

/** Executable provider selected by a canonical descriptor. Decode errors carry
  * coordinates in the foreign carrier, and validation errors may refine them
  * using the decoder-produced source map. */
final case class ForeignSurfaceRuntime(
    descriptor: ForeignSurfaceDescriptor,
    decode: Array[Byte] => Either[ForeignSurfaceError, (Cst, Cst => Option[ForeignSourceLocation])],
    encode: Cst => Either[ForeignSurfaceError, Array[Byte]],
    validate: Cst => Either[ForeignSurfaceError, Unit],
    validationModel: Digest,
):
  private def semanticDigest(term: Cst): Digest = Artifact(ArtifactKind.Term, Cst.toCanon(term)).digest

  def importBytes(origin: String, bytes: Array[Byte]): Either[ForeignSurfaceError, ImportedSurface] =
    val source = ForeignSource(descriptor.digest, origin, bytes, descriptor.mediaType)
    for
      decoded <- decode(bytes)
      (semantic, locate) = decoded
      _ <- validate(semantic).left.map(e => e.copy(location = locate(semantic).getOrElse(e.location)))
      _ <- descriptor.law match
        case SurfaceLaw.RoundTrip =>
          for
            encoded <- encode(semantic)
            replay <- decode(encoded)
            _ <- Either.cond(replay._1 == semantic, (), ForeignSurfaceError(
              "surface round-trip changed the canonical semantic result", locate(semantic).getOrElse(ForeignSourceLocation())))
          yield ()
        case SurfaceLaw.Projection => Right(())
      evidence = SurfaceEvidence(descriptor.language, descriptor.digest, Some(source.digest),
        semanticDigest(semantic), validationModel, descriptor.law, None, Nil)
    yield ImportedSurface(source, semantic, evidence)

  def project(semantic: Cst, migrations: List[Digest] = Nil): Either[ForeignSurfaceError, ProjectedSurface] =
    for
      _ <- validate(semantic)
      bytes <- encode(semantic)
      _ <- descriptor.law match
        case SurfaceLaw.RoundTrip => decode(bytes).flatMap(x => Either.cond(x._1 == semantic, (),
          ForeignSurfaceError("surface round-trip changed the canonical semantic result", ForeignSourceLocation())))
        case SurfaceLaw.Projection =>
          encode(semantic).flatMap(second => Either.cond(java.util.Arrays.equals(bytes, second), (),
            ForeignSurfaceError("projection is not deterministic", ForeignSourceLocation())))
      evidence = SurfaceEvidence(descriptor.language, descriptor.digest, None,
        semanticDigest(semantic), validationModel, descriptor.law, Some(Digest.ofBytes(bytes)), migrations)
    yield ProjectedSurface(bytes, evidence)

  /** Migration is semantic-first: transport the canonical result, validate it
    * under the target surface, then re-encode. Source bytes are never patched. */
  def migrate(
      imported: ImportedSurface,
      migration: Artifact,
      target: ForeignSurfaceRuntime,
      transport: Cst => Either[String, Cst],
  ): Either[ForeignSurfaceError, ProjectedSurface] =
    if migration.kind != ArtifactKind.Migration then
      Left(ForeignSurfaceError("surface migration requires a migration artifact", ForeignSourceLocation()))
    else if imported.evidence.surface != descriptor.digest then
      Left(ForeignSurfaceError("import evidence belongs to another surface", ForeignSourceLocation()))
    else transport(imported.semantic)
      .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation()))
      .flatMap(target.project(_, imported.evidence.migrations :+ migration.digest))

final case class ForeignSurfaceBundle(language: Digest, surfaces: List[ForeignSurfaceDescriptor]):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex),
    "surfaces" -> Canon.CList(surfaces.sortBy(_.name).map(_.canon)))
  def validate: Either[String, Unit] =
    if surfaces.exists(_.language != language) then Left("foreign surface targets another language")
    else if surfaces.map(_.name).distinct.size != surfaces.size then Left("duplicate foreign surface name")
    else if surfaces.exists(s => s.direction == SurfaceDirection.Encoding && s.law != SurfaceLaw.RoundTrip) then
      Left("encoding surfaces must prove the round-trip law")
    else if surfaces.exists(s => s.direction == SurfaceDirection.Projection && s.law != SurfaceLaw.Projection) then
      Left("projection surfaces must prove the projection law")
    else Right(())
  def artifact: Artifact = Artifact(ArtifactKind.ForeignSurface, Canon.CTag("surface-bundle", canon))

object ForeignSurfaces:
  private val utf8 = java.nio.charset.StandardCharsets.UTF_8

  private def structuralValidation(language: ComposedLanguage): (Digest, Cst => Either[ForeignSurfaceError, Unit]) =
    val digest = Digest.of(Canon.cmap(
      "kind" -> Canon.CStr("language-checker"), "language" -> Canon.CStr(language.digest.hex),
      "sort" -> Canon.CStr(language.grammar.top)))
    val check = (term: Cst) => LanguageChecker.checkTerm(language, language.grammar.top, term)
      .left.map(es => ForeignSurfaceError(es.map(_.render).mkString("; "), ForeignSourceLocation()))
      .map(_ => ())
    (digest, check)

  private def textDecoder(grammar: GrammarSpec): Array[Byte] => Either[ForeignSurfaceError, (Cst, Cst => Option[ForeignSourceLocation])] =
    bytes =>
      val source = String(bytes, utf8)
      Parser.parseFull(grammar, source).left.map(e => ForeignSurfaceError(e, ForeignSourceLocation()))
        .map { parsed =>
          val locate = (term: Cst) => parsed.spans.get(term).flatMap { (start, end) =>
            for first <- parsed.tokens.lift(start); last <- parsed.tokens.lift(math.max(start, end - 1))
            yield ForeignSourceLocation(
              byteStart = Some(first.offset), byteEnd = Some(last.offset + last.rawLen))
          }
          (parsed.cst, locate)
        }

  /** Standard language surfaces use the same evidence pipeline as workbook,
    * PDF, image, and report providers registered by packs. */
  def standard(language: ComposedLanguage): Map[String, ForeignSurfaceRuntime] =
    val (validationDigest, validate) = structuralValidation(language)
    def descriptor(name: String, format: ForeignFormat, media: String, provider: Digest) =
      ForeignSurfaceDescriptor(name, language.digest, format, media,
        SurfaceDirection.Encoding, SurfaceLaw.RoundTrip, provider)
    val textProvider = Digest.of(GrammarSpec.toCanon(language.grammar))
    val text = ForeignSurfaceRuntime(
      descriptor("text", ForeignFormat.Text, "text/plain; charset=utf-8", textProvider),
      textDecoder(language.grammar),
      term => Printer.print(language.grammar, term)
        .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation())).map(_.getBytes(utf8)),
      validate, validationDigest)
    val jsonProvider = Digest.of(GrammarSpec.toCanon(JsonSurface.grammar))
    val json = ForeignSurfaceRuntime(
      descriptor("json", ForeignFormat.Json, "application/json", jsonProvider),
      bytes => JsonSurface.decode(String(bytes, utf8))
        .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation(path = Some("$"))))
        .map(term => (term, _ => Some(ForeignSourceLocation(path = Some("$"))))),
      term => JsonSurface.encode(term)
        .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation(path = Some("$")))).map(_.getBytes(utf8)),
      validate, validationDigest)
    val canonProvider = Digest.of(Canon.CStr("cairn-canon-v1"))
    val canon = ForeignSurfaceRuntime(
      descriptor("canon", ForeignFormat.Report, "application/vnd.cairn.canon", canonProvider),
      bytes => Canon.decode(bytes).map(Cst.fromCanon)
        .left.map(e => ForeignSurfaceError(e, ForeignSourceLocation(byteStart = Some(0), byteEnd = Some(bytes.length))))
        .map(term => (term, _ => Some(ForeignSourceLocation(byteStart = Some(0), byteEnd = Some(bytes.length))))),
      term => Right(Canon.encode(Cst.toCanon(term))), validate, validationDigest)
    Map("text" -> text, "json" -> json, "canon" -> canon)
