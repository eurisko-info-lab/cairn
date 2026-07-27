package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{FederationFinality, Node}

/** PR32: the federation's replayable history, walked directly off the
  * ledger's own hash-linked block sequence
  * ([[FederationGc.orderedTransitionDigests]]) — no separate index or
  * pointer structure is needed to enumerate it.
  */
object FederationHistory:

  private def decodeAndVerify(
      transitionDigest: Digest, federationId: Digest, cas: Cas,
  ): Either[String, VerifiedFederationTransition] =
    for
      transitionArtifact <- cas.getByDigest(transitionDigest)
      transition <- FederationTransition.fromArtifact(transitionArtifact)
      beforeArtifact <- cas.getByDigest(transition.before)
      before <- FederationState.fromArtifact(beforeArtifact)
      afterArtifact <- cas.getByDigest(transition.after)
      after <- FederationState.fromArtifact(afterArtifact)
      commits <- transition.transactions.foldLeft[Either[String, List[FederationCommit]]](Right(Nil)) { (acc, cd) =>
        for xs <- acc; a <- cas.getByDigest(cd); c <- FederationCommit.fromArtifact(a) yield xs :+ c
      }
      finalityDigest <- transition.finality.toRight(s"federation history: transition ${transitionDigest.short} missing finality")
      finalityArtifact <- cas.getByDigest(finalityDigest)
      finality <- FederationFinality.FederationFinalityCertificate.fromCanon(finalityArtifact.body)
      verified <- VerifiedFederationTransition.verify(transition, before, after, commits, finality, federationId, cas)
    yield verified

  /** Ledger-anchoring ("transition and resulting state published together")
    * is the one property [[VerifiedFederationTransition]] itself can't
    * check — it only ever sees CAS content, never ledger structure — so
    * it's checked here instead, against the actual block that carried both
    * `PublishArtifact` transactions.
    */
  private def blockPublishesBoth(blocks: List[Block], transitionDigest: Digest, stateDigest: Digest): Boolean =
    blocks.exists { b =>
      val published = b.txs.collect { case SignedTx(Tx.PublishArtifact(key), _, _) => key.valueHash }.toSet
      published.contains(transitionDigest) && published.contains(stateDigest)
    }

  /** Re-verifies a single transition by digest AND confirms it is actually
    * ledger-published — that it and its resulting state were co-anchored in
    * a real block on `node`, not merely a well-formed artifact sitting in
    * `cas`. This is the `audit transition <digest>` primitive: an audit
    * that skipped the ledger check would accept content nobody ever
    * finalized, which defeats the point of auditing a *published*
    * transition rather than an arbitrary one. (`node` was already a
    * parameter before this fix but unused for exactly this property —
    * see the naming, which now says what it checks.)
    */
  def auditPublishedTransition(node: Node, cas: Cas, digest: Digest, federationId: Digest): Either[String, VerifiedFederationTransition] =
    for
      blocks <- node.blocks
      verified <- decodeAndVerify(digest, federationId, cas)
      _ <- Either.cond(blockPublishesBoth(blocks, digest, verified.transition.after), (),
        s"federation history: transition ${digest.short} and its resulting state were not co-published in one block")
    yield verified

  /** Folds every ledger-anchored transition, in the ledger's own order,
    * starting from `genesisState`: decodes and fully re-verifies each one
    * (chaining `transition.before` against the running accumulator, and
    * confirming same-block co-publication), accumulating to `.after`.
    * Reproduces the current `FederationState` exactly when run against an
    * unmodified node — this is PR32's exit condition made concrete:
    * deleting local journals/sidecars does not destroy federation history,
    * because the history was never IN those journals — it's the ledger's
    * own transition sequence, replayable from genesis at any time.
    */
  def replayFromGenesis(
      node: Node, cas: Cas, genesisState: FederationState, federationId: Digest,
  ): Either[String, FederationState] =
    for
      blocks <- node.blocks
      digests <- FederationGc.orderedTransitionDigests(node)
      finalState <- digests.foldLeft[Either[String, FederationState]](Right(genesisState)) { (accE, td) =>
        for
          acc <- accE
          verified <- decodeAndVerify(td, federationId, cas)
          _ <- Either.cond(verified.transition.before == acc.digest, (),
            s"federation history: transition ${td.short} does not chain from ${acc.digest.short}")
          _ <- Either.cond(blockPublishesBoth(blocks, td, verified.transition.after), (),
            s"federation history: transition ${td.short} and its resulting state were not co-published in one block")
        yield verified.after
      }
    yield finalState
