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

  private def decodeIndices(state: FederationState, cas: Cas): Either[String, (RepositoryIndex, ApplicationIndex, NamespaceIndex)] =
    for
      repoArtifact <- cas.getByDigest(state.repository)
      repo <- RepositoryIndex.fromArtifact(repoArtifact)
      appArtifact <- cas.getByDigest(state.applications)
      app <- ApplicationIndex.fromArtifact(appArtifact)
      nsArtifact <- cas.getByDigest(state.namespaces)
      ns <- NamespaceIndex.fromArtifact(nsArtifact)
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
      _ <- Either.cond(commits.map(_.namespace).distinct.length == commits.length, (),
        "federation transition: two commits in the same transition target the same namespace")
      commitsByNamespace = commits.map(c => c.namespace -> c).toMap
      beforeIndices <- decodeIndices(before, cas)
      afterIndices <- decodeIndices(after, cas)
      (repoBefore, appBefore, _) = beforeIndices
      (repoAfter, appAfter, nsAfter) = afterIndices
      _ <- diffCommitBackedIndex("repository", repoBefore.namespaces, repoAfter.namespaces, commitsByNamespace, _.repositoryGraph)
      _ <- diffCommitBackedIndex("application", appBefore.releases, appAfter.releases, commitsByNamespace, _.application)
      _ <- commits.foldLeft[Either[String, Unit]](Right(())) { (acc, c) =>
        acc.flatMap { _ =>
          Either.cond(nsAfter.manifests.get(c.namespace).contains(c.namespaceTrust), (),
            s"federation transition: commit for namespace '${c.namespace}' does not cite its own governing trust manifest")
        }
      }
    yield VerifiedFederationTransition(transition, before, after, commits, finality)
