package cairn.core

import cairn.kernel.*

final case class ReplicatedGcEpoch(number: Long, roots: Set[Digest], previous: Option[Digest]):
  require(number >= 0, "GC epoch must be non-negative")
  def canon: Canon = Canon.CTag("replicated-gc-epoch-v1", Canon.cmap(
    "number" -> Canon.CInt(number),
    "roots" -> Canon.cstrs(roots.toList.sortBy(_.hex).map(_.hex)),
    "previous" -> previous.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex)))))
  def artifact: Artifact = Artifact(ArtifactKind.ReplicatedGcEpoch, canon)
  def digest: Digest = artifact.digest

object ReplicatedGcEpoch:
  def fromArtifact(artifact: Artifact): Either[String, ReplicatedGcEpoch] =
    if artifact.kind != ArtifactKind.ReplicatedGcEpoch then Left("artifact is not a replicated GC epoch")
    else artifact.body match
      case Canon.CTag("replicated-gc-epoch-v1", c) =>
        try Right(ReplicatedGcEpoch(
          c.field("number").asInt, c.field("roots").asList.map(x => Digest(x.asStr)).toSet,
          c.field("previous") match
            case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d)); case _ => None))
        catch case e: Exception => Left(s"invalid replicated GC epoch: ${e.getMessage}")
      case _ => Left("expected replicated-gc-epoch-v1 body")

/** One ledger-addressable generation. Publication of this artifact makes all
  * selected roots indivisible: consumers either verify the complete closure or
  * retain their previous generation. */
final case class FederationCommit(
    namespace: String,
    branch: String,
    repositoryGraph: Digest,
    branchView: Digest,
    acceptanceEvidence: Digest,
    runtime: Digest,
    application: Digest,
    ecosystemRelease: Digest,
    /** Active [[NamespaceTrustManifest]] digest for this commit's namespace —
      * binds "namespace ownership" into the per-namespace transaction (PR31).
      */
    namespaceTrust: Digest,
    gcEpoch: Digest,
    previous: Option[Digest] = None,
):
  def dependencies: List[Digest] = List(repositoryGraph, branchView, acceptanceEvidence,
    runtime, application, ecosystemRelease, namespaceTrust, gcEpoch) ++ previous
  def canon: Canon = Canon.CTag("federation-commit-v1", Canon.cmap(
    "namespace" -> Canon.CStr(namespace), "branch" -> Canon.CStr(branch),
    "repositoryGraph" -> Canon.CStr(repositoryGraph.hex), "branchView" -> Canon.CStr(branchView.hex),
    "acceptanceEvidence" -> Canon.CStr(acceptanceEvidence.hex), "runtime" -> Canon.CStr(runtime.hex),
    "application" -> Canon.CStr(application.hex), "ecosystemRelease" -> Canon.CStr(ecosystemRelease.hex),
    "namespaceTrust" -> Canon.CStr(namespaceTrust.hex),
    "gcEpoch" -> Canon.CStr(gcEpoch.hex),
    "previous" -> previous.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex)))))
  def artifact: Artifact = Artifact(ArtifactKind.FederationCommit, canon)
  def digest: Digest = artifact.digest

object FederationCommit:
  def fromArtifact(artifact: Artifact): Either[String, FederationCommit] =
    if artifact.kind != ArtifactKind.FederationCommit then Left("artifact is not a federation commit")
    else artifact.body match
      case Canon.CTag("federation-commit-v1", c) =>
        try Right(FederationCommit(
          c.field("namespace").asStr, c.field("branch").asStr,
          Digest(c.field("repositoryGraph").asStr), Digest(c.field("branchView").asStr),
          Digest(c.field("acceptanceEvidence").asStr), Digest(c.field("runtime").asStr),
          Digest(c.field("application").asStr), Digest(c.field("ecosystemRelease").asStr),
          Digest(c.field("namespaceTrust").asStr),
          Digest(c.field("gcEpoch").asStr), c.field("previous") match
            case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d)); case _ => None))
        catch case e: Exception => Left(s"invalid federation commit: ${e.getMessage}")
      case _ => Left("expected federation-commit-v1 body")
