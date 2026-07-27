package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{FederationFinality, FederationReplica}

/** PR33: the pure, local-CAS-only deep verification a network replica runs
  * BEFORE voting on a proposed federation-state transition. Wraps
  * [[VerifiedFederationTransition.verifyStructural]] (PR32's structural/
  * policy checks — everything except the finality certificate's own
  * binding, which cannot exist yet at vote time) plus, for every namespace
  * the transition's own commits actually touch, PR30's FULL deep repository
  * re-certification (`ArtifactApplicationResolver` + `BranchRefStore.
  * verifyNativeRepositoryAt`) — not the SHALLOW "acceptance-evidence
  * artifact merely has the right kind" check `verifyFederationState` uses
  * for a node that already witnessed ingestion once.
  *
  * This is the real implementation behind `FederationReplica`'s injected
  * `VerifyProposal` callback (system-handler cannot depend on these
  * content/core-shaped pieces directly — see `FederationReplica`'s own doc
  * comment). It is local-CAS-only by design: any digest not yet resident
  * is reported as [[FederationReplica.VerifyOutcome.MissingClosure]], never
  * fetched here — fetching is the HTTP transport layer's job (PR33 slice 6).
  */
object FederationReplicaVerification:
  import BftQuorum.ReplicaId

  // `verifyNativeRepositoryAt` never reads `refsDir` (it re-certifies an
  // EXPLICIT repository value, not a ref-tracked "current" one) — a fixed,
  // lazily-created scratch directory is safe to share across every call.
  private lazy val scratchRefsDir: java.nio.file.Path =
    java.nio.file.Files.createTempDirectory("federation-replica-verify-refs")

  private val emptyRepositoryIndexDigest: Digest = RepositoryIndex(Map.empty).digest
  private val emptyApplicationIndexDigest: Digest = ApplicationIndex(Map.empty).digest

  private def decodeState(digest: Digest, cas: Cas): Either[String, FederationState] =
    cas.getByDigest(digest).flatMap(FederationState.fromArtifact)

  private def decodeNamespaceDigests(state: FederationState, cas: Cas): Either[String, (Map[String, Digest], Map[String, Digest])] =
    for
      repo <-
        if state.repository == emptyRepositoryIndexDigest then Right(RepositoryIndex(Map.empty))
        else cas.getByDigest(state.repository).flatMap(RepositoryIndex.fromArtifact)
      app <-
        if state.applications == emptyApplicationIndexDigest then Right(ApplicationIndex(Map.empty))
        else cas.getByDigest(state.applications).flatMap(ApplicationIndex.fromArtifact)
    yield (repo.namespaces, app.releases)

  private def certifyNamespace(namespace: String, repositoryDigest: Digest, applicationDigest: Digest, cas: Cas): Either[String, Unit] =
    for
      repoArtifact <- cas.getByDigest(repositoryDigest)
      repository <- NativeRepository.fromArtifact(repoArtifact)
      application <- ArtifactApplicationResolver(cas).resolve(applicationDigest)
      _ <- BranchRefStore(cas, scratchRefsDir, EffectContexts.forBranches()).verifyNativeRepositoryAt(repository, application)
        .left.map(e => s"federation: namespace '$namespace' repository re-certification failed: $e")
    yield ()

  /** Deep re-certifies every namespace this transition's own commits touch.
    * A commit's presence IS the "this namespace changed" signal (PR32's
    * `VerifiedFederationTransition` already requires a changed repository/
    * application index entry to trace to exactly one commit) — no separate
    * before/after diff is needed to find "which namespaces changed."
    * Namespaces NOT touched by any commit are untouched by definition and
    * skipped here — an already-bootstrapped replica has already
    * independently certified them at least once (see [[verifyWithCache]]);
    * re-certifying them again on every single round regardless of whether
    * they changed is exactly the redundant work the cache exists to avoid.
    */
  private def certifyChangedNamespaces(commits: List[FederationCommit], cas: Cas): Either[String, Unit] =
    commits.foldLeft[Either[String, Unit]](Right(())) { (acc, c) =>
      acc.flatMap(_ => certifyNamespace(c.namespace, c.repositoryGraph, c.application, cas))
    }

  /** Every namespace live in `before` — not just the ones this round's
    * commits touch — deep re-certified once. Only meaningful for a replica
    * that has never independently verified anything (a fresh join): an
    * ordinary continuously-running replica already covers each namespace
    * it cares about via [[certifyChangedNamespaces]] the round it changes,
    * so this is never called again once `cache.bootstrapped` is true (see
    * [[verifyWithCache]]). A namespace present in the repository index but
    * missing from the application index (or vice versa) is a structural
    * inconsistency in `before` itself, not a missing-closure situation —
    * rejected outright rather than silently skipped.
    */
  private def bootstrapAllNamespaces(before: FederationState, cas: Cas): Either[String, Map[String, Digest]] =
    decodeNamespaceDigests(before, cas).flatMap { (repoByNs, appByNs) =>
      repoByNs.toList.foldLeft[Either[String, Map[String, Digest]]](Right(Map.empty)) { case (acc, (ns, repoDigest)) =>
        acc.flatMap { certified =>
          appByNs.get(ns)
            .toRight(s"federation: namespace '$ns' has a repository entry but no application entry in the same state")
            .flatMap(appDigest => certifyNamespace(ns, repoDigest, appDigest, cas).map(_ => certified + (ns -> repoDigest)))
        }
      }
    }

  /** Resolves and CAS-closure-checks a proposal down to its constituent
    * parts, shared by [[verify]] and [[verifyWithCache]] so both apply the
    * exact same missing-closure classification (top-level artifacts, then
    * named commits) before doing any of the (cache-sensitive, in
    * `verifyWithCache`'s case) certification work.
    */
  private def resolveClosure(
      proposal: FederationFinality.FederationProposal, cas: Cas,
  ): Either[FederationReplica.VerifyOutcome, (FederationTransition, FederationState, FederationState, List[FederationCommit])] =
    val missingTop = Set(proposal.transition, proposal.before, proposal.after).filterNot(cas.contains)
    if missingTop.nonEmpty then Left(FederationReplica.VerifyOutcome.MissingClosure(missingTop))
    else
      val decoded = for
        transitionArtifact <- cas.getByDigest(proposal.transition)
        transition <- FederationTransition.fromArtifact(transitionArtifact)
        before <- decodeState(proposal.before, cas)
        after <- decodeState(proposal.after, cas)
      yield (transition, before, after)
      decoded match
        case Left(err) => Left(FederationReplica.VerifyOutcome.Rejected(err))
        case Right((transition, before, after)) =>
          val missingCommits = transition.transactions.filterNot(cas.contains).toSet
          if missingCommits.nonEmpty then Left(FederationReplica.VerifyOutcome.MissingClosure(missingCommits))
          else
            transition.transactions.foldLeft[Either[String, List[FederationCommit]]](Right(Nil)) { (acc, cd) =>
              for xs <- acc; a <- cas.getByDigest(cd); c <- FederationCommit.fromArtifact(a) yield xs :+ c
            } match
              case Left(err) => Left(FederationReplica.VerifyOutcome.Rejected(err))
              case Right(commits) => Right((transition, before, after, commits))

  def verify(proposerId: ReplicaId, proposal: FederationFinality.FederationProposal, cas: Cas): FederationReplica.VerifyOutcome =
    resolveClosure(proposal, cas) match
      case Left(outcome) => outcome
      case Right((transition, before, after, commits)) =>
        val outcome = for
          _ <- VerifiedFederationTransition.verifyStructural(transition, before, after, commits, cas)
          _ <- certifyChangedNamespaces(commits, cas)
        yield ()
        outcome.fold(FederationReplica.VerifyOutcome.Rejected(_), _ => FederationReplica.VerifyOutcome.Verified)

  /** Cache-aware entry point (PR33 slice 7): identical to [[verify]] except
    * that on a replica's FIRST successful verification ever
    * (`!cache.bootstrapped`), it additionally deep-certifies every
    * currently-live namespace in `before` — not just the ones this round's
    * commits touch — before it may vote. Without this, a newly-joined
    * replica would blindly trust every namespace it didn't personally
    * verify, on the say-so of whichever replica set added it; a namespace
    * tampered with before this replica joined would never be caught. The
    * cache update is never itself a correctness input — a lost/corrupted/
    * unwritable cache degrades to "treat as never bootstrapped" (redundant
    * re-certification work on the next round), never to "trust without
    * checking."
    */
  def verifyWithCache(
      proposerId: ReplicaId, proposal: FederationFinality.FederationProposal, cas: Cas,
      cache: FederationReplica.NamespaceCertCache,
  ): (FederationReplica.VerifyOutcome, FederationReplica.NamespaceCertCache) =
    resolveClosure(proposal, cas) match
      case Left(outcome) => (outcome, cache)
      case Right((transition, before, after, commits)) =>
        val result = for
          bootstrapCerts <-
            if cache.bootstrapped then Right(Map.empty[String, Digest]) else bootstrapAllNamespaces(before, cas)
          _ <- VerifiedFederationTransition.verifyStructural(transition, before, after, commits, cas)
          _ <- certifyChangedNamespaces(commits, cas)
        yield bootstrapCerts ++ commits.map(c => c.namespace -> c.repositoryGraph).toMap
        result match
          case Left(err) => (FederationReplica.VerifyOutcome.Rejected(err), cache)
          case Right(newlyCertified) =>
            (FederationReplica.VerifyOutcome.Verified, FederationReplica.NamespaceCertCache(true, cache.certified ++ newlyCertified))
