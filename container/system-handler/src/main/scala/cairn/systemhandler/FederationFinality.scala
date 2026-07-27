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
    * `epoch`/`previousState` because it just assembled the state). Builds
    * its own ad hoc [[ReplicaSetManifest]] from `replicas` via
    * [[BftFinality.sealReplicaSet]], exactly like `agreeLocalProven` does —
    * production callers with an already-established replica set pass that
    * set's own `replicas` list.
    */
  def agreeForFederationState(
      replicas: List[Keypair],
      view: Int,
      stateDigest: Digest,
      epoch: Long,
      previousState: Digest,
      federationId: Digest,
      maxRounds: Int = 16,
  ): Either[String, FederationFinalityCertificate] =
    val ids = replicas.map(_.name)
    for
      primaryId <- BftFinality.designatedPrimary(ids, view)
      primary <- replicas.find(_.name == primaryId.id).toRight(s"federation finality: missing primary ${primaryId.id}")
      cert <- runAgreement(replicas, primary, view, 0, stateDigest, epoch, previousState, federationId, maxRounds)
    yield cert

  private def runAgreement(
      replicas: List[Keypair],
      primary: Keypair,
      view: Int,
      seqUnused: Int,
      stateDigest: Digest,
      epoch: Long,
      previousState: Digest,
      federationId: Digest,
      maxRounds: Int,
  ): Either[String, FederationFinalityCertificate] =
    val seq = epoch.toInt
    val ids = replicas.map(k => ReplicaId(k.name))
    BftFinality.sealReplicaSet(replicas).flatMap { manifest =>
      val auth = manifest.authorities
      val setDig = manifest.replicaSetDigest
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
