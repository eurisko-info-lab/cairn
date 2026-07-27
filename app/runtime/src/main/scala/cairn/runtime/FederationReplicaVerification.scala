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

  private def decodeState(digest: Digest, cas: Cas): Either[String, FederationState] =
    cas.getByDigest(digest).flatMap(FederationState.fromArtifact)

  /** Deep re-certifies every namespace this transition's own commits touch.
    * A commit's presence IS the "this namespace changed" signal (PR32's
    * `VerifiedFederationTransition` already requires a changed repository/
    * application index entry to trace to exactly one commit) — no separate
    * before/after diff is needed to find "which namespaces changed."
    * Namespaces NOT touched by any commit are untouched by definition and
    * skipped entirely (the "unchanged namespaces reuse previous
    * certification" half of the user's requirement is slice 7's durable
    * cache; unconditionally re-certifying every commit's namespace here is
    * correct, if not yet optimized, in the meantime).
    */
  private def certifyChangedNamespaces(commits: List[FederationCommit], cas: Cas): Either[String, Unit] =
    val ctx = EffectContexts.forBranches()
    commits.foldLeft[Either[String, Unit]](Right(())) { (acc, c) =>
      acc.flatMap { _ =>
        for
          repoArtifact <- cas.getByDigest(c.repositoryGraph)
          repository <- NativeRepository.fromArtifact(repoArtifact)
          application <- ArtifactApplicationResolver(cas).resolve(c.application)
          _ <- BranchRefStore(cas, scratchRefsDir, ctx).verifyNativeRepositoryAt(repository, application)
            .left.map(e => s"federation: namespace '${c.namespace}' repository re-certification failed: $e")
        yield ()
      }
    }

  def verify(proposerId: ReplicaId, proposal: FederationFinality.FederationProposal, cas: Cas): FederationReplica.VerifyOutcome =
    val missingTop = Set(proposal.transition, proposal.before, proposal.after).filterNot(cas.contains)
    if missingTop.nonEmpty then FederationReplica.VerifyOutcome.MissingClosure(missingTop)
    else
      val decoded = for
        transitionArtifact <- cas.getByDigest(proposal.transition)
        transition <- FederationTransition.fromArtifact(transitionArtifact)
        before <- decodeState(proposal.before, cas)
        after <- decodeState(proposal.after, cas)
      yield (transition, before, after)
      decoded match
        case Left(err) => FederationReplica.VerifyOutcome.Rejected(err)
        case Right((transition, before, after)) =>
          val missingCommits = transition.transactions.filterNot(cas.contains).toSet
          if missingCommits.nonEmpty then FederationReplica.VerifyOutcome.MissingClosure(missingCommits)
          else
            val outcome =
              for
                commits <- transition.transactions.foldLeft[Either[String, List[FederationCommit]]](Right(Nil)) { (acc, cd) =>
                  for xs <- acc; a <- cas.getByDigest(cd); c <- FederationCommit.fromArtifact(a) yield xs :+ c
                }
                _ <- VerifiedFederationTransition.verifyStructural(transition, before, after, commits, cas)
                _ <- certifyChangedNamespaces(commits, cas)
              yield ()
            outcome.fold(FederationReplica.VerifyOutcome.Rejected(_), _ => FederationReplica.VerifyOutcome.Verified)
