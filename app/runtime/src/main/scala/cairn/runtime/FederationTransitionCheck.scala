package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{Ed25519, FederationFinality}

/** PR32: makes [[cairn.core.FederationTransition]] the actual canonical
  * history object, not scaffolding around [[cairn.core.FederationState]]
  * publication. A [[VerifiedFederationTransition]] can only be constructed
  * by [[VerifiedFederationTransition.verify]], which checks every property
  * a publication transaction must bind: digest/shape bindings, no
  * dangling/duplicate constituents, the finality certificate binding this
  * exact before/after pair, exact per-namespace index diffs (each changed
  * entry traces to exactly one authorizing commit, every untouched entry is
  * byte-identical), namespace-trust and replica-set amendment policy
  * (whether or not a commit also touches that namespace), GC-epoch
  * monotonicity/hash-linkage, and an EXACT `transition.approvals` closure —
  * equal to, not merely a superset of, the namespace-trust/replica-set
  * rotations this generation actually performs, so an unrelated or
  * unresolved extra digest is rejected rather than silently accepted.
  */
final case class VerifiedFederationTransition private (
    transition: FederationTransition,
    before: FederationState,
    after: FederationState,
    commits: List[FederationCommit],
    finality: FederationFinality.FederationFinalityCertificate,
)

object VerifiedFederationTransition:

  private val emptyRepositoryIndexDigest: Digest = RepositoryIndex(Map.empty).digest
  private val emptyApplicationIndexDigest: Digest = ApplicationIndex(Map.empty).digest
  private val emptyNamespaceIndexDigest: Digest = NamespaceIndex(Map.empty).digest

  /** [[cairn.core.FederationState.genesis]] names the empty-map index
    * digests directly without ever persisting those (trivial, always-empty)
    * artifacts anywhere — a genuinely genesis-only gap, since every
    * non-genesis index is content that some prior publish actually put into
    * CAS. A digest that equals the well-known empty index's own digest can
    * be resolved to that empty index without a CAS lookup: content
    * addressing means the digest already tells us what it would decode to,
    * so this is a recognized reproducible value, not an assumption about
    * unknown content. Any other missing digest still fails normally.
    */
  private def decodeIndices(state: FederationState, cas: Cas): Either[String, (RepositoryIndex, ApplicationIndex, NamespaceIndex)] =
    for
      repo <-
        if state.repository == emptyRepositoryIndexDigest then Right(RepositoryIndex(Map.empty))
        else cas.getByDigest(state.repository).flatMap(RepositoryIndex.fromArtifact)
      app <-
        if state.applications == emptyApplicationIndexDigest then Right(ApplicationIndex(Map.empty))
        else cas.getByDigest(state.applications).flatMap(ApplicationIndex.fromArtifact)
      ns <-
        if state.namespaces == emptyNamespaceIndexDigest then Right(NamespaceIndex(Map.empty))
        else cas.getByDigest(state.namespaces).flatMap(NamespaceIndex.fromArtifact)
    yield (repo, app, ns)

  /** A changed entry must trace to exactly one commit for that namespace
    * (whose own field matches the new entry); an untouched namespace's entry
    * must be byte-identical before/after — no other mechanism may move a
    * [[RepositoryIndex]]/[[ApplicationIndex]] entry. (`NamespaceIndex`
    * entries may ALSO move via pure namespace-trust rotation with no backing
    * commit at all — that path is checked separately, not here.)
    */
  private def diffCommitBackedIndex(
      label: String, before: Map[String, Digest], after: Map[String, Digest],
      commitsByNamespace: Map[String, FederationCommit], project: FederationCommit => Digest,
  ): Either[String, Unit] =
    (before.keySet ++ after.keySet).toList.foldLeft[Either[String, Unit]](Right(())) { (acc, ns) =>
      acc.flatMap { _ =>
        if before.get(ns) == after.get(ns) then Right(())
        else commitsByNamespace.get(ns) match
          case None =>
            Left(s"federation transition: $label entry for namespace '$ns' changed without an authorizing commit")
          case Some(c) =>
            Either.cond(after.get(ns).contains(project(c)), (),
              s"federation transition: $label entry for namespace '$ns' does not match its authorizing commit")
      }
    }

  /** A namespace's trust manifest may rotate WITHOUT any backing commit (a
    * pure authority-rotation transition) — unlike repository/application
    * entries, so this is checked independently of `diffCommitBackedIndex`.
    * Every rotation must satisfy [[NamespaceTrustManifest.allowsTransition]]
    * against its own predecessor. Returns the set of new manifest digests
    * this generation rotates — `verify` requires `transition.approvals` to
    * equal EXACTLY the union of this and [[diffReplicaSet]]'s result, not
    * merely contain it, so an unrelated or unresolved extra digest in
    * `approvals` is rejected rather than silently accepted.
    *
    * No `minActivation` high-water is enforced here: a rotation is
    * self-activating within the very generation that installs it (confirmed
    * against `FederationCeremonySuite`'s rotation step, where
    * `activationEpoch`/`activationHeight` equals that generation's own
    * epoch) — `allowsTransition`'s own strictly-increasing-over-predecessor
    * check is the monotonicity guard that applies here.
    */
  private def diffNamespaceTrust(
      before: Map[String, Digest], after: Map[String, Digest], cas: Cas,
  ): Either[String, Set[Digest]] =
    (before.keySet ++ after.keySet).toList.foldLeft[Either[String, Set[Digest]]](Right(Set.empty)) { (acc, ns) =>
      acc.flatMap { xs =>
        if before.get(ns) == after.get(ns) then Right(xs)
        else
          for
            live <- before.get(ns) match
              case None => Right(None)
              case Some(d) => cas.getByDigest(d).flatMap(NamespaceTrustManifest.fromArtifact).map(Some(_))
            newDigest <- after.get(ns).toRight(s"federation transition: namespace-trust entry for '$ns' was removed")
            newArtifact <- cas.getByDigest(newDigest)
            proposed <- NamespaceTrustManifest.fromArtifact(newArtifact)
            _ <- NamespaceTrustManifest.allowsTransition(proposed, live, before.get(ns), Ed25519.verify)
          yield xs + newDigest
      }
    }

  /** Symmetric to [[diffNamespaceTrust]], for the single federation-wide
    * replica set — returns its rotated manifest digest, if any.
    */
  private def diffReplicaSet(before: Digest, after: Digest, cas: Cas): Either[String, Set[Digest]] =
    if before == after then Right(Set.empty)
    else
      for
        liveArtifact <- cas.getByDigest(before)
        live <- ReplicaSetManifest.fromCanon(liveArtifact.body)
        newArtifact <- cas.getByDigest(after)
        proposed <- ReplicaSetManifest.fromCanon(newArtifact.body)
        _ <- ReplicaSetManifest.allowsTransition(proposed, Some(live), Some(before), Ed25519.verify)
      yield Set(after)

  /** GC advancement as an explicit, checked transition constituent: if the
    * epoch actually changed, it must strictly increase and hash-link back
    * to the predecessor via [[ReplicatedGcEpoch.previous]] (that field is
    * already the epoch's own hash-link — no new field is needed anywhere).
    */
  private def diffGcEpoch(before: Digest, after: Digest, cas: Cas): Either[String, Unit] =
    if before == after then Right(())
    else
      for
        beforeArtifact <- cas.getByDigest(before)
        beforeEpoch <- ReplicatedGcEpoch.fromArtifact(beforeArtifact)
        afterArtifact <- cas.getByDigest(after)
        afterEpoch <- ReplicatedGcEpoch.fromArtifact(afterArtifact)
        _ <- Either.cond(afterEpoch.number > beforeEpoch.number, (),
          s"federation transition: gcEpoch number ${afterEpoch.number} does not exceed predecessor ${beforeEpoch.number}")
        _ <- Either.cond(afterEpoch.previous.contains(before), (),
          "federation transition: gcEpoch does not hash-link back to its predecessor")
      yield ()

  /** Everything [[verify]] checks EXCEPT the finality certificate's binding
    * — digest/shape bindings, no dangling/duplicate constituents, exact
    * per-namespace index diffs, namespace-trust/replica-set amendment
    * policy, GC-epoch monotonicity, and an exact `transition.approvals`
    * closure. Exposed separately (PR33) because a network replica must
    * verify a PROPOSED transition BEFORE voting — before any finality
    * certificate for it exists to check against (the certificate's own
    * digest isn't even determined until enough replicas have signed
    * Commits for it, which can't happen before at least a quorum has
    * already verified and voted) — so [[FederationReplicaVerification]]
    * calls this directly, then separately confirms `transition.finality`
    * once the real certificate exists (mirroring `verify`'s own check),
    * rather than needing a certificate that cannot exist yet.
    */
  def verifyStructural(
      transition: FederationTransition,
      before: FederationState,
      after: FederationState,
      commits: List[FederationCommit],
      cas: Cas,
  ): Either[String, Unit] =
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
      _ <- Either.cond(commits.map(_.namespace).distinct.length == commits.length, (),
        "federation transition: two commits in the same transition target the same namespace")
      commitsByNamespace = commits.map(c => c.namespace -> c).toMap
      beforeIndices <- decodeIndices(before, cas)
      afterIndices <- decodeIndices(after, cas)
      (repoBefore, appBefore, nsBefore) = beforeIndices
      (repoAfter, appAfter, nsAfter) = afterIndices
      _ <- diffCommitBackedIndex("repository", repoBefore.namespaces, repoAfter.namespaces, commitsByNamespace, _.repositoryGraph)
      _ <- diffCommitBackedIndex("application", appBefore.releases, appAfter.releases, commitsByNamespace, _.application)
      _ <- commits.foldLeft[Either[String, Unit]](Right(())) { (acc, c) =>
        acc.flatMap { _ =>
          Either.cond(nsAfter.manifests.get(c.namespace).contains(c.namespaceTrust), (),
            s"federation transition: commit for namespace '${c.namespace}' does not cite its own governing trust manifest")
        }
      }
      nsApprovals <- diffNamespaceTrust(nsBefore.manifests, nsAfter.manifests, cas)
      rsApprovals <- diffReplicaSet(before.trustRoots, after.trustRoots, cas)
      _ <- diffGcEpoch(before.gcEpoch, after.gcEpoch, cas)
      _ <- Either.cond(transition.approvals.toSet == (nsApprovals ++ rsApprovals), (),
        "federation transition: approvals does not equal exactly the namespace-trust/replica-set rotations this generation performs")
    yield ()

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
  /** PR33.1: the certificate's `proposal` digest is the ONLY thing every
    * Commit seal is actually cryptographically over (see
    * `FederationFinality.valueOfProposal`'s doc comment) — `transition`/
    * `stateDigest`/`previousState`/`epoch`/`replicaSet`/`federationId` on
    * the certificate are unsigned convenience projections of that same
    * proposal's own fields. `verifyAgainstFederationHistory` (system-handler,
    * no CAS access) can only compare those projections against what the
    * CALLER already claims; this independently fetches and decodes the
    * actual `FederationProposal` artifact `finality.proposal` names and
    * confirms it REALLY has those exact fields, closing the gap a
    * certificate with internally-consistent-looking but merely ASSERTED
    * projections (never checked against real content) would otherwise leave.
    *
    * `proposal.transition` names the PRE-CERT transition (`finality = None`)
    * — a structurally DIFFERENT artifact/digest from the FINAL, finality-
    * bound `transition` this function is handed (`finality = Some(finality.digest)`),
    * by the same two-step design `FederationTransactionCoordinator.publishWithCert`
    * itself uses (the final transition cannot exist before the certificate
    * naming it does). So this compares CONTENT — `before`/`transactions`/
    * `after`/`approvals`, the only fields that must be identical between
    * the two — not digest equality, which could never hold by construction.
    */
  private def verifyProposalBinding(
      finality: FederationFinality.FederationFinalityCertificate,
      transition: FederationTransition,
      before: FederationState,
      after: FederationState,
      federationId: Digest,
      cas: Cas,
  ): Either[String, Unit] =
    for
      proposalArtifact <- cas.getByDigest(finality.proposal)
      proposal <- FederationFinality.FederationProposal.fromArtifact(proposalArtifact)
      _ <- Either.cond(proposal.transition == finality.transition, (),
        "federation transition: certified proposal's transition does not match the certificate's own projection")
      votedTransitionArtifact <- cas.getByDigest(proposal.transition)
      votedTransition <- FederationTransition.fromArtifact(votedTransitionArtifact)
      _ <- Either.cond(
        votedTransition.before == transition.before && votedTransition.transactions == transition.transactions &&
          votedTransition.after == transition.after && votedTransition.approvals == transition.approvals, (),
        "federation transition: the transition the quorum actually voted on does not match this transition's content")
      _ <- Either.cond(proposal.before == before.digest, (),
        "federation transition: certified proposal's before-state does not match the supplied before-state")
      _ <- Either.cond(proposal.after == after.digest, (),
        "federation transition: certified proposal's after-state does not match the supplied after-state")
      _ <- Either.cond(proposal.epoch == finality.epoch, (),
        "federation transition: certified proposal's epoch does not match the certificate's own projection")
      _ <- Either.cond(proposal.replicaSet == finality.replicaSet, (),
        "federation transition: certified proposal's replicaSet does not match the certificate's own projection")
      _ <- Either.cond(proposal.federationId == federationId, (),
        "federation transition: certified proposal's federationId does not match the expected federation")
    yield ()

  def verify(
      transition: FederationTransition,
      before: FederationState,
      after: FederationState,
      commits: List[FederationCommit],
      finality: FederationFinality.FederationFinalityCertificate,
      federationId: Digest,
      cas: Cas,
  ): Either[String, VerifiedFederationTransition] =
    for
      _ <- verifyStructural(transition, before, after, commits, cas)
      _ <- Either.cond(transition.finality.contains(finality.digest), (),
        "federation transition: transition.finality does not cite the supplied certificate")
      trustArtifact <- cas.getByDigest(after.trustRoots)
      activeManifest <- ReplicaSetManifest.fromCanon(trustArtifact.body)
      _ <- FederationFinality.FederationFinalityCertificate.verifyAgainstFederationHistory(
        finality, activeManifest, federationId, before.digest, after.digest)
      _ <- verifyProposalBinding(finality, transition, before, after, federationId, cas)
    yield VerifiedFederationTransition(transition, before, after, commits, finality)
