package cairn.systemhandler

import cairn.kernel.*

/** PR31's "equivocation as evidence" invariant: two conflicting signed
  * proposals for the same predecessor state become a first-class,
  * independently-verifiable artifact — not just a local rejection string.
  *
  * Confirmed gap before this: `BftReplica.receive`'s conflict branch
  * (a `PrePrepare` whose value digest doesn't match this sequence's
  * existing prepared lock) only ever returned `Left(...)` — a real
  * detection, but nothing durable, gossipable, or third-party-checkable.
  * `EquivocationEvidence.detect` packages the SAME two conflicting
  * `BftFinality.SignedMsg`s (the newly-arrived one and whichever one
  * established the lock) into a canon artifact, independently verifying
  * both signatures rather than trusting the caller's claim that they
  * conflict.
  *
  * Deliberately minimal-viable for PR31: this artifact is produced and
  * persisted for audit/policy consumption, but no `AcceptanceConstitution`/
  * `AuthorityRules` wiring here actively rejects a federation transition
  * signed by an equivocating replica — that is a real policy decision
  * deserving its own test corpus, left for later.
  */
final case class EquivocationEvidence(
    replica: BftQuorum.ReplicaId,
    view: Int,
    seq: Int,
    proposalA: BftFinality.SignedMsg,
    proposalB: BftFinality.SignedMsg,
):
  def canon: Canon = Canon.CTag("equivocation-evidence-v1", Canon.cmap(
    "replica" -> Canon.CStr(replica.id),
    "view" -> Canon.CInt(view),
    "seq" -> Canon.CInt(seq),
    "proposalA" -> proposalA.canon,
    "proposalB" -> proposalB.canon))
  def artifact: Artifact = Artifact(ArtifactKind.EquivocationEvidence, canon)
  def digest: Digest = artifact.digest

object EquivocationEvidence:
  def fromArtifact(artifact: Artifact): Either[String, EquivocationEvidence] =
    if artifact.kind != ArtifactKind.EquivocationEvidence then Left("artifact is not equivocation evidence")
    else artifact.body match
      case Canon.CTag("equivocation-evidence-v1", m) =>
        for
          proposalA <- BftFinality.SignedMsg.fromCanon(m.field("proposalA"))
          proposalB <- BftFinality.SignedMsg.fromCanon(m.field("proposalB"))
        yield EquivocationEvidence(
          BftQuorum.ReplicaId(m.field("replica").asStr),
          m.field("view").asInt.toInt, m.field("seq").asInt.toInt,
          proposalA, proposalB)
      case _ => Left("expected equivocation-evidence-v1 body")

  /** Verifies: both proposals are `PrePrepare`s for the SAME `(view, seq,
    * from = replica)`, their values genuinely differ, and both signatures
    * independently verify against `authorities` under the same
    * `(replicaSet, federationOrChainId)` binding — never trusting the
    * caller's bare claim that two messages conflict.
    */
  def detect(
      a: BftFinality.SignedMsg,
      b: BftFinality.SignedMsg,
      authorities: Map[String, Vector[Byte]],
      replicaSet: Digest,
      federationOrChainId: Digest,
  ): Either[String, EquivocationEvidence] =
    (a.msg, b.msg) match
      case (pa: BftQuorum.Msg.PrePrepare, pb: BftQuorum.Msg.PrePrepare) =>
        for
          _ <- Either.cond(pa.from == pb.from, (), "equivocation: proposals are from different replicas")
          _ <- Either.cond(a.signer == pa.from, (), "equivocation: proposal A signer does not match its own from")
          _ <- Either.cond(b.signer == pb.from, (), "equivocation: proposal B signer does not match its own from")
          _ <- Either.cond(pa.view == pb.view && pa.seq == pb.seq, (),
            "equivocation: proposals are not for the same (view, seq)")
          _ <- Either.cond(pa.value.digest != pb.value.digest, (),
            "equivocation: proposals carry the identical value — not a conflict")
          _ <- Either.cond(a.replicaSet == replicaSet && b.replicaSet == replicaSet, (),
            "equivocation: proposals do not both bind the expected replica set")
          _ <- Either.cond(a.chainId == federationOrChainId && b.chainId == federationOrChainId, (),
            "equivocation: proposals do not both bind the expected federation/chain id")
          _ <- BftFinality.verify(authorities, a, Some(replicaSet), Some(federationOrChainId))
          _ <- BftFinality.verify(authorities, b, Some(replicaSet), Some(federationOrChainId))
        yield EquivocationEvidence(pa.from, pa.view, pa.seq, a, b)
      case _ => Left("equivocation: both proposals must be PrePrepare messages")
