package cairn.core

import cairn.kernel.*

final case class ApplicationLanguage(
    name: String, language: Digest, grammar: Digest, capabilities: Digest,
):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name), "language" -> Canon.CStr(language.hex),
    "grammar" -> Canon.CStr(grammar.hex), "capabilities" -> Canon.CStr(capabilities.hex))

final case class ApplicationEntry(name: String, artifact: Digest, kind: ArtifactKind):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name), "artifact" -> Canon.CStr(artifact.hex), "kind" -> Canon.CStr(kind.name))

/** The sole startup root. Runtime selection and entrypoints are data; callers
  * never pass a host-built language map or application assembly list. */
final case class ApplicationManifest(
    name: String,
    languages: List[ApplicationLanguage],
    entries: List[ApplicationEntry],
    dependencies: List[Digest] = Nil,
):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "languages" -> Canon.CList(languages.sortBy(_.name).map(_.canon)),
    "entries" -> Canon.CList(entries.sortBy(_.name).map(_.canon)),
    "dependencies" -> Canon.cstrs(dependencies.distinct.sortBy(_.hex).map(_.hex)))
  def artifact: Artifact = Artifact(ArtifactKind.Application, canon)
  def digest: Digest = artifact.digest
  def roots: List[Digest] = languages.flatMap(l => List(l.language, l.grammar, l.capabilities)) ++
    entries.map(_.artifact) ++ dependencies
  def validate: Either[String, Unit] =
    if name.isEmpty then Left("application name is required")
    else if languages.map(_.name).distinct.length != languages.length then Left("duplicate application language name")
    else if entries.map(_.name).distinct.length != entries.length then Left("duplicate application entry name")
    else Right(())

object ApplicationManifest:
  def fromArtifact(artifact: Artifact): Either[String, ApplicationManifest] =
    if artifact.kind != ArtifactKind.Application then Left("startup root is not an application artifact")
    else try Right(ApplicationManifest(
      artifact.body.field("name").asStr,
      artifact.body.field("languages").asList.map { l => ApplicationLanguage(
        l.field("name").asStr, Digest(l.field("language").asStr),
        Digest(l.field("grammar").asStr), Digest(l.field("capabilities").asStr)) },
      artifact.body.field("entries").asList.map { e => ApplicationEntry(
        e.field("name").asStr, Digest(e.field("artifact").asStr),
        ArtifactKind.parse(e.field("kind").asStr).fold(m => throw CodecError(m), identity)) },
      artifact.body.field("dependencies").asList.map(d => Digest(d.asStr)))).flatMap(m => m.validate.map(_ => m))
    catch case e: Exception => Left(s"invalid application manifest: ${e.getMessage}")

/** Artifact-specific discovery used by installers and auditors. Unknown kinds
  * are leaves; no orchestration-only dependency list is consulted. */
object ArtifactDependencies:
  def direct(artifact: Artifact): Either[String, List[Digest]] = artifact.kind match
    case ArtifactKind.Application => ApplicationManifest.fromArtifact(artifact).map(_.roots)
    case ArtifactKind.Language => scala.util.Try(
      artifact.body.field("fragments").asList.map(x => Digest(x.asStr))).toEither
      .left.map(e => s"invalid language dependency graph: ${e.getMessage}")
    case ArtifactKind.LanguageCapabilities => scala.util.Try {
      val c = LanguageCapabilities.fromCanon(artifact.body)
      List(c.changeSemantics, c.changeSurface) ++ c.validation.toList ++
        c.migrations ++ c.queries ++ c.policies ++ c.projections
    }.toEither.left.map(e => s"invalid capability dependency graph: ${e.getMessage}")
    case ArtifactKind.ChangeCapability => scala.util.Try(List(
      Digest(artifact.body.field("semantics").asStr), Digest(artifact.body.field("surface").asStr)))
      .toEither.left.map(e => s"invalid change capability graph: ${e.getMessage}")
    case _ => Right(Nil)
