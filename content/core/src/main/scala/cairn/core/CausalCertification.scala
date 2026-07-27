package cairn.core

import cairn.kernel.*

final case class CausalReplayEvidence(
    machine: Digest, implementation: Digest, runtime: Digest, change: Digest,
    base: Digest, result: Digest, accessTrace: Digest,
):
  def canon: Canon = Canon.cmap("machine" -> Canon.CStr(machine.hex),
    "implementation" -> Canon.CStr(implementation.hex), "runtime" -> Canon.CStr(runtime.hex),
    "change" -> Canon.CStr(change.hex), "base" -> Canon.CStr(base.hex),
    "result" -> Canon.CStr(result.hex), "accessTrace" -> Canon.CStr(accessTrace.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Trace, Canon.CTag("causal-replay-evidence", canon))

final case class CausalContextEvidence(
    causalChange: Digest, accesses: Set[SemanticLocation], context: List[ContextDependency],
):
  def canon: Canon = Canon.cmap("causalChange" -> Canon.CStr(causalChange.hex),
    "accesses" -> Canon.CList(accesses.toList.sortBy(_.render).map(_.canon)),
    "context" -> Canon.CList(context.sortBy(_.location.render).map(_.canon)))
  def artifact: Artifact = Artifact(ArtifactKind.Trace, Canon.CTag("causal-context-evidence", canon))

final case class CertifiedCausalChange(
    causalChange: Digest, runtime: Digest, replayEvidence: Digest,
    acceptanceEvidence: Digest, contextEvidence: Digest,
):
  def canon: Canon = Canon.cmap("causalChange" -> Canon.CStr(causalChange.hex),
    "runtime" -> Canon.CStr(runtime.hex), "replayEvidence" -> Canon.CStr(replayEvidence.hex),
    "acceptanceEvidence" -> Canon.CStr(acceptanceEvidence.hex),
    "contextEvidence" -> Canon.CStr(contextEvidence.hex))
  def artifact: Artifact = Artifact(ArtifactKind.CertifiedCausalChange, canon)
  def dependencies: List[Digest] =
    List(causalChange, runtime, replayEvidence, acceptanceEvidence, contextEvidence)

object CertifiedCausalChange:
  def fromArtifact(a: Artifact): Either[String, CertifiedCausalChange] =
    if a.kind != ArtifactKind.CertifiedCausalChange then Left("not a certified-causal-change artifact")
    else try Right(CertifiedCausalChange(Digest(a.body.field("causalChange").asStr),
      Digest(a.body.field("runtime").asStr), Digest(a.body.field("replayEvidence").asStr),
      Digest(a.body.field("acceptanceEvidence").asStr), Digest(a.body.field("contextEvidence").asStr)))
    catch case e: Exception => Left(s"invalid certified causal change: ${e.getMessage}")
