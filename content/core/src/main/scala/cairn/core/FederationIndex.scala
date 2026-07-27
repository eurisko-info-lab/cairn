package cairn.core

import cairn.kernel.*

/** Thin namespace -> digest maps, one per [[FederationState]] axis. Kept as
  * distinct [[ArtifactKind]]s (not one generic map artifact) so decode-time
  * type mismatches (e.g. handing a [[NamespaceIndex]] where a
  * [[RepositoryIndex]] was expected) fail immediately rather than silently.
  */
private def entriesCanon(entries: Map[String, Digest]): Canon =
  Canon.cmap(entries.toList.sortBy(_._1).map((k, v) => k -> Canon.CStr(v.hex))*)

private def entriesFromCanon(c: Canon): Map[String, Digest] =
  c.asMap.map((k, v) => k -> Digest(v.asStr))

final case class RepositoryIndex(namespaces: Map[String, Digest]):
  def canon: Canon = Canon.CTag("repository-index-v1", entriesCanon(namespaces))
  def artifact: Artifact = Artifact(ArtifactKind.RepositoryIndex, canon)
  def digest: Digest = artifact.digest
  def dependencies: List[Digest] = namespaces.values.toList.sortBy(_.hex)

object RepositoryIndex:
  def fromArtifact(artifact: Artifact): Either[String, RepositoryIndex] =
    if artifact.kind != ArtifactKind.RepositoryIndex then Left("artifact is not a repository index")
    else artifact.body match
      case Canon.CTag("repository-index-v1", c) =>
        try Right(RepositoryIndex(entriesFromCanon(c)))
        catch case e: Exception => Left(s"invalid repository index: ${e.getMessage}")
      case _ => Left("expected repository-index-v1 body")

final case class ApplicationIndex(releases: Map[String, Digest]):
  def canon: Canon = Canon.CTag("application-index-v1", entriesCanon(releases))
  def artifact: Artifact = Artifact(ArtifactKind.ApplicationIndex, canon)
  def digest: Digest = artifact.digest
  def dependencies: List[Digest] = releases.values.toList.sortBy(_.hex)

object ApplicationIndex:
  def fromArtifact(artifact: Artifact): Either[String, ApplicationIndex] =
    if artifact.kind != ArtifactKind.ApplicationIndex then Left("artifact is not an application index")
    else artifact.body match
      case Canon.CTag("application-index-v1", c) =>
        try Right(ApplicationIndex(entriesFromCanon(c)))
        catch case e: Exception => Left(s"invalid application index: ${e.getMessage}")
      case _ => Left("expected application-index-v1 body")

final case class NamespaceIndex(manifests: Map[String, Digest]):
  def canon: Canon = Canon.CTag("namespace-index-v1", entriesCanon(manifests))
  def artifact: Artifact = Artifact(ArtifactKind.NamespaceIndex, canon)
  def digest: Digest = artifact.digest
  def dependencies: List[Digest] = manifests.values.toList.sortBy(_.hex)

object NamespaceIndex:
  def fromArtifact(artifact: Artifact): Either[String, NamespaceIndex] =
    if artifact.kind != ArtifactKind.NamespaceIndex then Left("artifact is not a namespace index")
    else artifact.body match
      case Canon.CTag("namespace-index-v1", c) =>
        try Right(NamespaceIndex(entriesFromCanon(c)))
        catch case e: Exception => Left(s"invalid namespace index: ${e.getMessage}")
      case _ => Left("expected namespace-index-v1 body")
