package cairn.core

import cairn.kernel.*

/** PR31: one kernel-checkable digest spanning every externally visible root
  * of the federation — ledger, repository, applications, namespace
  * ownership, replica trust, and GC retention. A node advances to a new
  * [[FederationState]] only via a finalized [[FederationTransition]]
  * (BFT-agreed, see `FederationFinality` in system-handler), never by
  * writing any one of these six roots in isolation.
  */
final case class FederationState(
    /** Sealed ledger [[cairn.kernel.Block]] digest anchoring this generation;
      * its height is this state's epoch (shared clock with [[ReplicaSetManifest]]
      * activation heights and [[ReplicatedGcEpoch]] numbering).
      */
    ledger: Digest,
    /** [[RepositoryIndex]] digest: namespace -> [[NativeRepository]] digest, every namespace. */
    repository: Digest,
    /** [[ApplicationIndex]] digest: namespace -> current signed ecosystem release digest. */
    applications: Digest,
    /** [[NamespaceIndex]] digest: namespace -> active [[NamespaceTrustManifest]] digest. */
    namespaces: Digest,
    /** Active `ReplicaSetManifest`'s own ARTIFACT digest (`.digest`, not
      * `.replicaSetDigest` — the latter is a body-only matching key used to
      * compare quorum membership, e.g. inside `FinalityCertificate`/
      * `FederationFinalityCertificate`, and is never itself a CAS key).
      * Content-addressable like every other `FederationState` field, so the
      * atomic-visibility gate can decode it directly via `cas.getByDigest`.
      */
    trustRoots: Digest,
    /** [[ReplicatedGcEpoch]] digest: the retention generation safe to reclaim against. */
    gcEpoch: Digest,
):
  def dependencies: List[Digest] = List(ledger, repository, applications, namespaces, trustRoots, gcEpoch)
  def canon: Canon = Canon.CTag("federation-state-v1", Canon.cmap(
    "ledger" -> Canon.CStr(ledger.hex),
    "repository" -> Canon.CStr(repository.hex),
    "applications" -> Canon.CStr(applications.hex),
    "namespaces" -> Canon.CStr(namespaces.hex),
    "trustRoots" -> Canon.CStr(trustRoots.hex),
    "gcEpoch" -> Canon.CStr(gcEpoch.hex)))
  def artifact: Artifact = Artifact(ArtifactKind.FederationState, canon)
  def digest: Digest = artifact.digest

object FederationState:
  def fromArtifact(artifact: Artifact): Either[String, FederationState] =
    if artifact.kind != ArtifactKind.FederationState then Left("artifact is not a federation state")
    else artifact.body match
      case Canon.CTag("federation-state-v1", c) =>
        try Right(FederationState(
          Digest(c.field("ledger").asStr), Digest(c.field("repository").asStr),
          Digest(c.field("applications").asStr), Digest(c.field("namespaces").asStr),
          Digest(c.field("trustRoots").asStr), Digest(c.field("gcEpoch").asStr)))
        catch case e: Exception => Left(s"invalid federation state: ${e.getMessage}")
      case _ => Left("expected federation-state-v1 body")

  /** Well-known empty generation: every index is empty, the GC epoch is
    * generation 0 with no roots. Used as [[FederationTransition.before]]
    * for a federation's very first transition.
    */
  def genesis(chainGenesis: Digest, genesisReplicaSet: Digest): FederationState =
    val emptyRepo = RepositoryIndex(Map.empty)
    val emptyApps = ApplicationIndex(Map.empty)
    val emptyNamespaces = NamespaceIndex(Map.empty)
    val epoch = ReplicatedGcEpoch(0, Set.empty, None)
    FederationState(chainGenesis, emptyRepo.digest, emptyApps.digest, emptyNamespaces.digest,
      genesisReplicaSet, epoch.digest)

/** One federation-wide advance: `before`/`after` [[FederationState]] digests,
  * the per-namespace [[FederationCommit]]s folded into it, and the BFT
  * evidence that finalized it. `finality` is `None` while only proposed;
  * `Some(certDigest)` once a [[FederationFinalityCertificate]] exists.
  */
final case class FederationTransition(
    before: Digest,
    transactions: List[Digest],
    after: Digest,
    approvals: List[Digest],
    finality: Option[Digest],
):
  def canon: Canon = Canon.CTag("federation-transition-v1", Canon.cmap(
    "before" -> Canon.CStr(before.hex),
    "transactions" -> Canon.cstrs(transactions.map(_.hex)),
    "after" -> Canon.CStr(after.hex),
    "approvals" -> Canon.cstrs(approvals.map(_.hex)),
    "finality" -> finality.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex)))))
  def artifact: Artifact = Artifact(ArtifactKind.FederationTransition, canon)
  def digest: Digest = artifact.digest
  def dependencies: List[Digest] = before :: after :: transactions ++ approvals ++ finality.toList

object FederationTransition:
  def fromArtifact(artifact: Artifact): Either[String, FederationTransition] =
    if artifact.kind != ArtifactKind.FederationTransition then Left("artifact is not a federation transition")
    else artifact.body match
      case Canon.CTag("federation-transition-v1", c) =>
        try Right(FederationTransition(
          Digest(c.field("before").asStr),
          c.field("transactions").asList.map(x => Digest(x.asStr)),
          Digest(c.field("after").asStr),
          c.field("approvals").asList.map(x => Digest(x.asStr)),
          c.field("finality") match
            case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d)); case _ => None))
        catch case e: Exception => Left(s"invalid federation transition: ${e.getMessage}")
      case _ => Left("expected federation-transition-v1 body")
