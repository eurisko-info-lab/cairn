package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.FederationFinality

/** PR32: makes [[cairn.core.FederationTransition]] the actual canonical
  * history object, not scaffolding around [[cairn.core.FederationState]]
  * publication. A [[VerifiedFederationTransition]] can only be constructed
  * by [[VerifiedFederationTransition.verify]], which checks every property
  * a publication transaction must bind: digest/shape bindings, no
  * dangling/duplicate constituents, the finality certificate binding this
  * exact before/after pair, exact per-namespace index diffs (each changed
  * entry traces to exactly one authorizing commit, every untouched entry is
  * byte-identical), namespace-trust and replica-set amendment policy
  * (whether or not a commit also touches that namespace), and GC-epoch
  * monotonicity/hash-linkage.
  */
final case class VerifiedFederationTransition private (
    transition: FederationTransition,
    before: FederationState,
    after: FederationState,
    commits: List[FederationCommit],
    finality: FederationFinality.FederationFinalityCertificate,
)

object VerifiedFederationTransition:

  /** `federationId` is the fixed chain identity (external context, not
    * itself part of [[FederationState]]); `activeManifest` is decoded from
    * `after.trustRoots`, not `before` — confirmed against
    * `FederationCeremonySuite`'s replica-set-rotation generation, where the
    * minted certificate's `replicaSet` names the SUCCESSOR manifest: the
    * incoming quorum immediately certifies the block that installs it. Every
    * non-rotating generation has `before.trustRoots == after.trustRoots`, so
    * only a rotation discriminates between the two choices, and `after` is
    * the one that matches real certificates.
    */
  def verify(
      transition: FederationTransition,
      before: FederationState,
      after: FederationState,
      commits: List[FederationCommit],
      finality: FederationFinality.FederationFinalityCertificate,
      federationId: Digest,
      cas: Cas,
  ): Either[String, VerifiedFederationTransition] =
    def noDupes(label: String, ds: List[Digest]): Either[String, Unit] =
      Either.cond(ds.distinct.length == ds.length, (), s"federation transition: duplicate $label entries")

    for
      _ <- Either.cond(transition.before == before.digest, (),
        "federation transition: transition.before does not match supplied before-state")
      _ <- Either.cond(transition.after == after.digest, (),
        "federation transition: transition.after does not match supplied after-state")
      _ <- noDupes("transactions", transition.transactions)
      _ <- noDupes("approvals", transition.approvals)
      _ <- Either.cond(transition.transactions.toSet == commits.map(_.digest).toSet, (),
        "federation transition: transition.transactions does not match supplied commits")
      _ <- Either.cond(transition.finality.contains(finality.digest), (),
        "federation transition: transition.finality does not cite the supplied certificate")
      trustArtifact <- cas.getByDigest(after.trustRoots)
      activeManifest <- ReplicaSetManifest.fromCanon(trustArtifact.body)
      _ <- FederationFinality.FederationFinalityCertificate.verifyAgainstFederationHistory(
        finality, activeManifest, federationId, before.digest, after.digest)
    yield VerifiedFederationTransition(transition, before, after, commits, finality)
