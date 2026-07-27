package cairn.systemhandler

import cairn.kernel.*
import java.nio.charset.StandardCharsets

/** BFT finality over a `cairn.core.FederationState` digest — PR31's
  * "consensus over semantic state" invariant: agreement finalizes the
  * digest of the COMPLETE federation state, not a block containing loosely
  * related updates.
  *
  * Deliberately a PARALLEL certificate type, not a generalization of
  * [[BftFinality.FinalityCertificate]] in place: that certificate's quorum/
  * signature core is already digest-generic, but its chain-binding checks
  * ([[BftFinality.FinalityCertificate.verifyAgainstChain]] and friends) and
  * [[BftFinality.BftReplica]]'s live per-process state machine are deeply
  * `Block`/`Node`/`LedgerKernel.replay`-coupled — a replica only ever votes
  * on a value it can itself replay-verify as a sealed block, a real safety
  * property, not incidental coupling. A `cairn.core.FederationState` is
  * not itself a sealed ledger block; its predecessor-check is a hash-linked
  * digest chain (`transition.before == priorState.digest`), not ledger
  * replay. Reusing [[BftQuorum]]'s already-generic `Msg`/`Value`/quorum math
  * plus [[BftFinality]]'s signing/`SignedMsg` conventions (copied, not
  * imported, for the same reason — see [[agreeForFederationState]]) keeps
  * the existing, heavily-tested block-finality path untouched.
  */
object FederationFinality:
  import BftQuorum.*

  def valueOfState(stateDigest: Digest): Value =
    Value(stateDigest.hex.getBytes(StandardCharsets.US_ASCII).toVector)

  /** PR33: the network-agreement analogue of a proposed block — everything
    * a `FederationReplica` needs to independently locate and verify a
    * candidate generation, content-addressed so a replica missing any of
    * it can fetch exactly the referenced digests (`transition`/`before`/
    * `after`) from the proposer over `/blob/<hex>`, the same generic path
    * used for every other artifact closure. `transition` is the
    * `cairn.core.FederationTransition` digest binding this whole
    * generation (PR32); `before`/`after` are its `cairn.core.FederationState`
    * endpoints, named directly here (rather than requiring a replica to
    * decode `transition` first) so a replica can check `before`/`epoch`
    * against its own running state before paying the cost of fetching and
    * verifying `transition` itself.
    */
  final case class FederationProposal(
      federationId: Digest,
      transition: Digest,
      before: Digest,
      after: Digest,
      epoch: Long,
      replicaSet: Digest,
  ):
    def canon: Canon = Canon.CTag("federation-proposal-v1", Canon.cmap(
      "federationId" -> Canon.CStr(federationId.hex),
      "transition" -> Canon.CStr(transition.hex),
      "before" -> Canon.CStr(before.hex),
      "after" -> Canon.CStr(after.hex),
      "epoch" -> Canon.CInt(epoch),
      "replicaSet" -> Canon.CStr(replicaSet.hex)))
    def artifact: Artifact = Artifact(ArtifactKind.FederationProposal, canon)
    def digest: Digest = artifact.digest

  object FederationProposal:
    def fromArtifact(artifact: Artifact): Either[String, FederationProposal] =
      if artifact.kind != ArtifactKind.FederationProposal then Left("artifact is not a federation proposal")
      else artifact.body match
        case Canon.CTag("federation-proposal-v1", c) =>
          try Right(FederationProposal(
            Digest(c.field("federationId").asStr), Digest(c.field("transition").asStr),
            Digest(c.field("before").asStr), Digest(c.field("after").asStr),
            c.field("epoch").asInt, Digest(c.field("replicaSet").asStr)))
          catch case e: Exception => Left(s"invalid federation proposal: ${e.getMessage}")
        case _ => Left("expected federation-proposal-v1 body")

  /** The PrePrepare/Commit value a `FederationReplica` actually agrees over
    * — the proposal's OWN digest, not the bare state digest `valueOfState`
    * encodes for the local-orchestration path. Committing to the whole
    * proposal (not just `after`) means a replica's vote is bound to the
    * exact `transition`/`epoch`/`replicaSet` it verified, not merely the
    * resulting state — two different proposals could coincidentally name
    * the same `after` (e.g. a retried epoch) without being the same
    * verified generation.
    */
  def valueOfProposal(proposal: FederationProposal): Value =
    Value(proposal.digest.hex.getBytes(StandardCharsets.US_ASCII).toVector)

  /** Quorum certificate over a `cairn.core.FederationState` digest.
    * `epoch` is the ledger height this generation is anchored at (the same
    * shared clock [[ReplicaSetManifest]] activation heights and
    * `ReplicatedGcEpoch` numbering use); `previousState` is the
    * `cairn.core.FederationState` digest this transition claims as its
    * predecessor — the structural analogue of [[BftFinality.FinalityCertificate]]'s
    * `parent`, but a state hash-chain link, not a ledger-block one.
    * `federationId` reuses the ledger's chain identity
    * ([[BftFinality.chainId]]) rather than inventing a second genesis concept.
    */
  final case class FederationFinalityCertificate(
      stateDigest: Digest,
      view: Int,
      seq: Int,
      commits: List[(ReplicaId, Vector[Byte])],
      replicaSet: Digest,
      epoch: Long,
      previousState: Digest,
      federationId: Digest,
  ):
    def canon: Canon = Canon.CTag("federation-finality", Canon.cmap(
      "state" -> Canon.CStr(stateDigest.hex),
      "view" -> Canon.CInt(view),
      "seq" -> Canon.CInt(seq),
      "commits" -> Canon.CList(commits.sortBy(_._1.id).map { (id, seal) =>
        Canon.cmap("replica" -> Canon.CStr(id.id), "seal" -> Canon.CBytes(seal))
      }),
      "replicaSet" -> Canon.CStr(replicaSet.hex),
      "epoch" -> Canon.CInt(epoch),
      "previousState" -> Canon.CStr(previousState.hex),
      "federationId" -> Canon.CStr(federationId.hex)))
    def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)
    def digest: Digest = artifact.digest

  object FederationFinalityCertificate:
    def fromCanon(c: Canon): Either[String, FederationFinalityCertificate] =
      import Canon.*
      c match
        case CTag("federation-finality", m) =>
          try
            val commits = m.field("commits").asList.map { row =>
              ReplicaId(row.field("replica").asStr) -> (row.field("seal") match
                case CBytes(bs) => bs
                case _          => throw CodecError("seal"))
            }
            Right(FederationFinalityCertificate(
              Digest(m.field("state").asStr),
              m.field("view").asInt.toInt,
              m.field("seq").asInt.toInt,
              commits,
              Digest(m.field("replicaSet").asStr),
              m.field("epoch").asInt,
              Digest(m.field("previousState").asStr),
              Digest(m.field("federationId").asStr)))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not a federation-finality certificate: $other")

    /** Quorum certificate check: distinct known replicas, matching replica-set
      * digest, valid Commit seals for the encoded state value, `seq == epoch`.
      */
    def verify(
        cert: FederationFinalityCertificate,
        authorities: Map[String, Vector[Byte]],
        expectedReplicaSet: Digest,
    ): Either[String, Unit] =
      val n = authorities.size
      if !BftQuorum.validReplicaCount(n) then
        Left(s"federation finality: n=$n is not a valid 3f+1 size")
      else if cert.seq.toLong != cert.epoch then
        Left(s"federation finality: certificate sequence ${cert.seq} does not equal epoch ${cert.epoch}")
      else
        val q = quorumSize(n)
        val ids = cert.commits.map(_._1.id)
        if cert.replicaSet != expectedReplicaSet then
          Left(s"federation finality: replicaSet ${cert.replicaSet.short} != expected ${expectedReplicaSet.short}")
        else if ids.length != ids.distinct.length then
          Left(s"federation finality: duplicate replica commits: ${ids.mkString(",")}")
        else if ids.exists(id => !authorities.contains(id)) then
          Left(s"federation finality: unknown replica in commits")
        else if ids.distinct.length < q then
          Left(s"federation finality: ${ids.distinct.length} distinct commits < quorum $q")
        else
          val valueDigest = valueOfState(cert.stateDigest).digest
          cert.commits.foldLeft[Either[String, Unit]](Right(())) { case (acc, (id, seal)) =>
            acc.flatMap { _ =>
              val commit = Msg.Commit(cert.view, cert.seq, valueDigest, id)
              BftFinality.verify(
                authorities,
                BftFinality.SignedMsg(commit, id, seal, cert.replicaSet, cert.federationId),
                Some(cert.replicaSet), Some(cert.federationId))
            }
          }

    /** Quorum certificate check against a verified [[ReplicaSetManifest]]. */
    def verify(cert: FederationFinalityCertificate, manifest: ReplicaSetManifest): Either[String, Unit] =
      ReplicaSetManifest.verifySeals(manifest, Ed25519.verify).flatMap { _ =>
        verify(cert, manifest.authorities, manifest.replicaSetDigest)
      }

    /** `cairn.core.FederationState`-shaped analogue of
      * [[BftFinality.FinalityCertificate.verifyAgainstChain]]: a structural
      * hash-chain predecessor check instead of ledger replay, since a
      * federation state is not itself a sealed block. Takes bare digests
      * (not the `FederationState` value itself) because `system-handler`
      * does not depend on `content/core` — callers in `app/runtime`, which
      * depends on both, pass `priorState.digest`/`claimedState.digest`.
      */
    def verifyAgainstFederationHistory(
        cert: FederationFinalityCertificate,
        activeManifest: ReplicaSetManifest,
        expectedFederationId: Digest,
        priorStateDigest: Digest,
        claimedStateDigest: Digest,
    ): Either[String, Unit] =
      for
        _ <- verify(cert, activeManifest)
        _ <- Either.cond(cert.federationId == expectedFederationId, (), "federation finality: federation id mismatch")
        _ <- Either.cond(cert.previousState == priorStateDigest, (),
          "federation finality: certificate does not chain from the prior state")
        _ <- Either.cond(cert.stateDigest == claimedStateDigest, (),
          "federation finality: certificate subject is not the claimed state")
      yield ()

  /** Agree over a `cairn.core.FederationState` digest. Mirrors
    * [[BftFinality.agreeLocalProven]] (the "caller already knows
    * epoch/predecessor" variant, not [[BftFinality.agreeForSealedBlock]]'s
    * "rediscover from chain" variant — a federation state isn't
    * independently on-chain to rediscover; the caller always already knows
    * `epoch`/`previousState` because it just assembled the state).
    *
    * Takes the active [[ReplicaSetManifest]] EXPLICITLY rather than
    * building a bare one ad hoc from `replicas` (an earlier draft did this
    * via [[BftFinality.sealReplicaSet]], matching `agreeLocalProven`'s own
    * lab-fixture convenience — but that silently ignores any amendment
    * metadata (`replaces`/`activationHeight`/`predecessorApprovals`) on the
    * REAL active manifest, minting a certificate whose `replicaSet` digest
    * then mismatches the manifest everything else — `FederationState.
    * trustRoots`, `NamespaceTrustManifest`-style rotation — actually uses.
    * Caught by the PR31 exit ceremony's successor-replica-set-activation
    * step, where the active manifest is never the bare genesis shape).
    */
  def agreeForFederationState(
      replicas: List[Keypair],
      manifest: ReplicaSetManifest,
      view: Int,
      stateDigest: Digest,
      epoch: Long,
      previousState: Digest,
      federationId: Digest,
      maxRounds: Int = 16,
  ): Either[String, FederationFinalityCertificate] =
    val ids = replicas.map(_.name)
    for
      _ <- Either.cond(manifest.ids.toSet == ids.toSet, (),
        "federation finality: manifest membership does not match the supplied replicas")
      primaryId <- BftFinality.designatedPrimary(ids, view)
      primary <- replicas.find(_.name == primaryId.id).toRight(s"federation finality: missing primary ${primaryId.id}")
      cert <- runAgreement(replicas, manifest, primary, view, stateDigest, epoch, previousState, federationId, maxRounds)
    yield cert

  private def runAgreement(
      replicas: List[Keypair],
      manifest: ReplicaSetManifest,
      primary: Keypair,
      view: Int,
      stateDigest: Digest,
      epoch: Long,
      previousState: Digest,
      federationId: Digest,
      maxRounds: Int,
  ): Either[String, FederationFinalityCertificate] =
    val seq = epoch.toInt
    val ids = replicas.map(k => ReplicaId(k.name))
    val auth = manifest.authorities
    val setDig = manifest.replicaSetDigest
    locally {
      val value = valueOfState(stateDigest)
      val primaryId = ReplicaId(primary.name)
      var states: Map[ReplicaId, ReplicaState] =
        ids.map(id => id -> ReplicaState(id, ids.length, faulty = false)).toMap
      BftFinality.sign(primary, Msg.PrePrepare(view, seq, value, primaryId), setDig, federationId).flatMap { pp =>
        var inbox: List[BftFinality.SignedMsg] = List(pp)
        var round = 0
        var commitSeals: Map[ReplicaId, Vector[Byte]] = Map.empty
        while inbox.nonEmpty && round < maxRounds do
          val batch = inbox
          inbox = Nil
          batch.foreach { sm =>
            BftFinality.verify(auth, sm, Some(setDig), Some(federationId)) match
              case Left(_) => ()
              case Right(()) =>
                ids.foreach { rid =>
                  val (st2, out) = deliver(states(rid), sm.msg)
                  states = states + (rid -> st2)
                  out.foreach { m =>
                    val kp = replicas.find(_.name == rid.id).get
                    BftFinality.sign(kp, m, setDig, federationId).foreach { signed =>
                      m match
                        case Msg.Commit(_, _, _, _) => commitSeals = commitSeals + (rid -> signed.seal)
                        case _ => ()
                      inbox = inbox :+ signed
                    }
                  }
                }
          }
          round += 1
        val decided = states.values.flatMap(_.slots.get((view, seq)).flatMap(_.decided)).toList
        if decided.isEmpty then Left("federation finality: no decision")
        else if !honestAgree(
            states.map { (id, st) =>
              id -> st.slots.get((view, seq)).flatMap(s =>
                s.decided.map(v => Decision(view, seq, v, s.commits.keys.toList)))
            },
            Set.empty) then Left("federation finality: honest disagreement")
        else
          val q = quorumSize(ids.length)
          val commits = commitSeals.toList
          if commits.map(_._1.id).distinct.length < q then
            Left(s"federation finality: only ${commits.map(_._1.id).distinct.length} distinct commits, need $q")
          else
            val cert = FederationFinalityCertificate(
              stateDigest, view, seq, commits, setDig, epoch, previousState, federationId)
            FederationFinalityCertificate.verify(cert, manifest).map(_ => cert)
      }
    }
