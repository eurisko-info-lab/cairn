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
    /** Parses the bare `federation-proposal-v1` canon body (not wrapped in
      * an [[Artifact]]) — used to recover a proposal directly from a
      * `PrePrepare`'s embedded `Value` bytes (see [[valueOfProposal]]),
      * where there is no surrounding artifact envelope.
      */
    def fromCanon(c: Canon): Either[String, FederationProposal] =
      c match
        case Canon.CTag("federation-proposal-v1", m) =>
          try Right(FederationProposal(
            Digest(m.field("federationId").asStr), Digest(m.field("transition").asStr),
            Digest(m.field("before").asStr), Digest(m.field("after").asStr),
            m.field("epoch").asInt, Digest(m.field("replicaSet").asStr)))
          catch case e: Exception => Left(s"invalid federation proposal: ${e.getMessage}")
        case _ => Left("expected federation-proposal-v1 body")

    def fromArtifact(artifact: Artifact): Either[String, FederationProposal] =
      if artifact.kind != ArtifactKind.FederationProposal then Left("artifact is not a federation proposal")
      else fromCanon(artifact.body)

  // Deliberately no separate `valueOfProposal`: `FederationFinalityCertificate.verify`
  // reconstructs each Commit's expected value as `valueOfState(cert.stateDigest)`
  // (below) — an existing, unchanged contract shared with the local-orchestration
  // path — so the wire `Value` a `FederationReplica` actually agrees over must stay
  // exactly `valueOfState(proposal.after)`. `Value.bytes` is the state digest's own
  // ASCII hex (recoverable directly, not just its hash — see `valueOfState`), which
  // is how a receiving replica recovers `stateDigest` from a bare `PrePrepare`; the
  // REST of the proposal (`transition`/`before`/`epoch`) is learned out-of-band —
  // `FederationReplica.learnProposal`, populated by `propose` locally and by the
  // HTTP `/federation/propose` request body for every other replica (PR33 slice 5).

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

  // ---------------------------------------------------------------------
  // PR33: real network agreement — FederationReplica + HttpNode's
  // /federation/* endpoints replace this section's local orchestration for
  // production use. A distinct signing domain from BftFinality.MsgDomain
  // keeps the two protocols' signed payloads structurally unmistakable even
  // before considering their different canon tags.
  // ---------------------------------------------------------------------

  val MsgDomain: String = "cairn-federation-v1"

  /** Authenticated request asking a replica to propose. Unlike
    * [[BftFinality.ProposeRequest]] (which names a bare block digest plus
    * separate chainId/replicaSet fields), this carries the FULL
    * [[FederationProposal]] directly — federationId/replicaSet are already
    * fields on it, and a receiving replica needs the whole proposal's
    * content to run `FederationReplica`'s verify callback before voting,
    * not just a value it agrees over.
    */
  final case class FederationProposeRequest(
      view: Int,
      proposal: FederationProposal,
      signer: ReplicaId,
      seal: Vector[Byte],
  ):
    def payload: Array[Byte] = Canon.encode(Canon.cmap(
      "domain" -> Canon.CStr(MsgDomain), "kind" -> Canon.CStr("propose"),
      "view" -> Canon.CInt(view), "proposal" -> proposal.canon))
    def canon: Canon = Canon.CTag("federation-propose-req", Canon.cmap(
      "view" -> Canon.CInt(view), "proposal" -> proposal.canon,
      "signer" -> Canon.CStr(signer.id), "seal" -> Canon.CBytes(seal)))

  object FederationProposeRequest:
    def sign(signer: Signer, view: Int, proposal: FederationProposal): FederationProposeRequest =
      val req = FederationProposeRequest(view, proposal, ReplicaId(signer.name), Vector.empty)
      req.copy(seal = signer.sign(req.payload))

    def fromCanon(c: Canon): Either[String, FederationProposeRequest] =
      c match
        case Canon.CTag("federation-propose-req", m) =>
          for
            proposal <- FederationProposal.fromCanon(m.field("proposal"))
            seal <- m.field("seal") match
              case Canon.CBytes(bs) => Right(bs)
              case other => Left(s"bad federation propose seal: $other")
          yield FederationProposeRequest(m.field("view").asInt.toInt, proposal, ReplicaId(m.field("signer").asStr), seal)
        case other => Left(s"not a federation-propose-req: $other")

  def verifyProposeRequest(
      authorities: Map[String, Vector[Byte]],
      req: FederationProposeRequest,
      expectedFederationId: Digest,
      expectedReplicaSet: Digest,
  ): Either[String, Unit] =
    if req.proposal.federationId != expectedFederationId then
      Left(s"federation: propose federationId ${req.proposal.federationId.short} != expected ${expectedFederationId.short}")
    else if req.proposal.replicaSet != expectedReplicaSet then
      Left(s"federation: propose replicaSet ${req.proposal.replicaSet.short} != expected ${expectedReplicaSet.short}")
    else
      authorities.get(req.signer.id) match
        case None => Left(s"unknown federation replica '${req.signer.id}'")
        case Some(pk) =>
          if Ed25519.verify(pk, req.payload, req.seal) then Right(())
          else Left(s"bad federation propose seal from ${req.signer.id}")

  /** Authenticated request to trigger a view-change on a replica. Mirrors
    * [[BftFinality.ViewChangeRequest]].
    */
  final case class FederationViewChangeRequest(
      newView: Int,
      federationId: Digest,
      replicaSet: Digest,
      signer: ReplicaId,
      seal: Vector[Byte],
  ):
    def payload: Array[Byte] = Canon.encode(Canon.cmap(
      "domain" -> Canon.CStr(MsgDomain), "kind" -> Canon.CStr("view-change"),
      "federationId" -> Canon.CStr(federationId.hex), "replicaSet" -> Canon.CStr(replicaSet.hex),
      "newView" -> Canon.CInt(newView)))
    def canon: Canon = Canon.CTag("federation-view-change-req", Canon.cmap(
      "newView" -> Canon.CInt(newView), "federationId" -> Canon.CStr(federationId.hex),
      "replicaSet" -> Canon.CStr(replicaSet.hex), "signer" -> Canon.CStr(signer.id), "seal" -> Canon.CBytes(seal)))

  object FederationViewChangeRequest:
    def sign(signer: Signer, newView: Int, federationId: Digest, replicaSet: Digest): FederationViewChangeRequest =
      val req = FederationViewChangeRequest(newView, federationId, replicaSet, ReplicaId(signer.name), Vector.empty)
      req.copy(seal = signer.sign(req.payload))

    def fromCanon(c: Canon): Either[String, FederationViewChangeRequest] =
      c match
        case Canon.CTag("federation-view-change-req", m) =>
          m.field("seal") match
            case Canon.CBytes(bs) =>
              Right(FederationViewChangeRequest(
                m.field("newView").asInt.toInt, Digest(m.field("federationId").asStr),
                Digest(m.field("replicaSet").asStr), ReplicaId(m.field("signer").asStr), bs))
            case other => Left(s"bad federation view-change seal: $other")
        case other => Left(s"not a federation-view-change-req: $other")

  def verifyViewChangeRequest(
      authorities: Map[String, Vector[Byte]],
      req: FederationViewChangeRequest,
      expectedFederationId: Digest,
      expectedReplicaSet: Digest,
  ): Either[String, Unit] =
    if req.federationId != expectedFederationId then
      Left(s"federation: view-change federationId ${req.federationId.short} != expected ${expectedFederationId.short}")
    else if req.replicaSet != expectedReplicaSet then
      Left(s"federation: view-change replicaSet ${req.replicaSet.short} != expected ${expectedReplicaSet.short}")
    else
      authorities.get(req.signer.id) match
        case None => Left(s"unknown federation replica '${req.signer.id}'")
        case Some(pk) =>
          if Ed25519.verify(pk, req.payload, req.seal) then Right(())
          else Left(s"bad federation view-change seal from ${req.signer.id}")

  /** Signed snapshot of a replica's current view + latest installed NewView
    * digest — lets a client determine the cluster's actual view via quorum
    * evidence instead of guessing from HTTP fan-out timing. Mirrors
    * [[BftFinality.ViewStatus]].
    */
  final case class FederationViewStatus(
      replica: ReplicaId,
      view: Int,
      latestNewViewDigest: Option[Digest],
      federationId: Digest,
      replicaSet: Digest,
      seal: Vector[Byte],
  ):
    def payload: Array[Byte] = Canon.encode(Canon.cmap(
      "domain" -> Canon.CStr(MsgDomain), "kind" -> Canon.CStr("status"),
      "replica" -> Canon.CStr(replica.id), "view" -> Canon.CInt(view),
      "latestNewViewDigest" -> latestNewViewDigest.fold(Canon.CTag("none", Canon.CInt(0)))(d =>
        Canon.CTag("some", Canon.CStr(d.hex))),
      "federationId" -> Canon.CStr(federationId.hex), "replicaSet" -> Canon.CStr(replicaSet.hex)))
    def canon: Canon = Canon.CTag("federation-view-status", Canon.cmap(
      "replica" -> Canon.CStr(replica.id), "view" -> Canon.CInt(view),
      "latestNewViewDigest" -> latestNewViewDigest.fold(Canon.CTag("none", Canon.CInt(0)))(d =>
        Canon.CTag("some", Canon.CStr(d.hex))),
      "federationId" -> Canon.CStr(federationId.hex), "replicaSet" -> Canon.CStr(replicaSet.hex),
      "seal" -> Canon.CBytes(seal)))

  object FederationViewStatus:
    def sign(
        signer: Signer, view: Int, latestNewViewDigest: Option[Digest],
        federationId: Digest, replicaSet: Digest,
    ): FederationViewStatus =
      val vs = FederationViewStatus(ReplicaId(signer.name), view, latestNewViewDigest, federationId, replicaSet, Vector.empty)
      vs.copy(seal = signer.sign(vs.payload))

    def fromCanon(c: Canon): Either[String, FederationViewStatus] =
      import Canon.*
      c match
        case CTag("federation-view-status", m) =>
          try
            val digest = m.asMap.get("latestNewViewDigest") match
              case Some(CTag("some", CStr(h))) => Some(Digest(h))
              case _ => None
            m.field("seal") match
              case CBytes(bs) =>
                Right(FederationViewStatus(
                  ReplicaId(m.field("replica").asStr), m.field("view").asInt.toInt, digest,
                  Digest(m.field("federationId").asStr), Digest(m.field("replicaSet").asStr), bs))
              case other => Left(s"bad federation view-status seal: $other")
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not a federation-view-status: $other")

  def verifyViewStatus(
      authorities: Map[String, Vector[Byte]],
      vs: FederationViewStatus,
      expectedFederationId: Digest,
      expectedReplicaSet: Digest,
  ): Either[String, Unit] =
    if vs.federationId != expectedFederationId then
      Left(s"federation view-status: federationId ${vs.federationId.short} != expected ${expectedFederationId.short}")
    else if vs.replicaSet != expectedReplicaSet then
      Left(s"federation view-status: replicaSet ${vs.replicaSet.short} != expected ${expectedReplicaSet.short}")
    else
      authorities.get(vs.replica.id) match
        case None => Left(s"unknown federation replica '${vs.replica.id}'")
        case Some(pk) =>
          if Ed25519.verify(pk, vs.payload, vs.seal) then Right(())
          else Left(s"bad federation view-status seal from ${vs.replica.id}")

  def postMsg(baseUrl: String, sm: BftFinality.SignedMsg): Either[String, Unit] =
    BftFinality.httpPost(s"$baseUrl/federation/msg", Canon.encode(sm.canon)).map(_ => ())

  def fetchCerts(baseUrl: String): Either[String, List[FederationFinalityCertificate]] =
    BftFinality.httpGet(s"$baseUrl/federation/certs").flatMap { body =>
      Canon.decode(body).flatMap {
        case Canon.CList(xs) =>
          xs.foldLeft[Either[String, List[FederationFinalityCertificate]]](Right(Nil)) { (acc, c) =>
            acc.flatMap(cs => FederationFinalityCertificate.fromCanon(c).map(cs :+ _))
          }
        case other => Left(s"bad federation certs payload: $other")
      }
    }

  def fetchViewStatus(baseUrl: String): Either[String, FederationViewStatus] =
    BftFinality.httpGet(s"$baseUrl/federation/status").flatMap(body => Canon.decode(body).flatMap(FederationViewStatus.fromCanon))

  /** Unlike [[BftFinality.fanoutQuorum]] (which returns — and CANCELS every
    * still-pending request — the instant `q` requests succeed), this waits
    * out each request's own share of the timeout regardless of how many
    * others already finished. Only appropriate where every recipient
    * actually needs the payload delivered, not merely "some quorum agrees
    * something happened": cancelling delivery to an arbitrary subset is
    * fine for a view-change vote (that's a quorum decision), but wrong for
    * fanning out a proposal's content — if the unlucky cancelled replica
    * happens to be the one about to become primary, NOBODY ends up knowing
    * the proposal and the round hangs to a timeout for no protocol reason.
    */
  private def fanoutAll(
      replicaUrls: Map[String, String], post: (String, String) => Either[String, Unit], timeoutMs: Long = 3000L,
  ): Either[String, Unit] =
    val n = replicaUrls.size
    if !BftQuorum.validReplicaCount(n) then Left(s"federation: n=$n is not a valid 3f+1 size")
    else
      val q = quorumSize(n)
      val exec = java.util.concurrent.Executors.newFixedThreadPool(math.min(n, 8).max(1))
      try
        val futs = replicaUrls.toList.map { (name, url) =>
          java.util.concurrent.CompletableFuture.supplyAsync(() => name -> post(name, url), exec)
        }
        val deadline = System.nanoTime() + timeoutMs * 1000000L
        val results = futs.map { f =>
          val remainingMs = math.max(1L, (deadline - System.nanoTime()) / 1000000L)
          try Some(f.get(remainingMs, java.util.concurrent.TimeUnit.MILLISECONDS))
          catch case _: Exception => None
        }
        val ok = results.count(_.exists(_._2.isRight))
        if ok >= q then Right(())
        else
          val errs = results.collect { case Some((name, Left(e))) => s"$name: $e" }
          Left(s"federation: propose fan-out reached $ok/$n (need $q): ${errs.mkString("; ")}")
      finally exec.shutdownNow()

  /** Fans the propose request out to EVERY replica URL, not just the
    * primary's — the request body carries the full [[FederationProposal]],
    * and every receiving replica's `/federation/propose` handler calls
    * `learnProposal` unconditionally before checking whether it is itself
    * the primary for `view` (only the primary additionally mints a
    * PrePrepare). This is how a replica other than the primary comes to
    * know a proposal's content at all — the PrePrepare it later receives
    * over `/federation/msg` only carries a state digest, not the proposal.
    */
  def propose(replicaUrls: Map[String, String], initiator: Signer, view: Int, proposal: FederationProposal): Either[String, Unit] =
    val req = FederationProposeRequest.sign(initiator, view, proposal)
    val body = Canon.encode(req.canon)
    fanoutAll(replicaUrls, { (name, url) => BftFinality.httpPost(s"$url/federation/propose", body).map(_ => ()) })

  def requestNetworkViewChange(
      replicaUrls: Map[String, String], newView: Int, initiator: Signer, federationId: Digest, replicaSet: Digest,
  ): Either[String, Unit] =
    val req = FederationViewChangeRequest.sign(initiator, newView, federationId, replicaSet)
    val body = Canon.encode(req.canon)
    BftFinality.fanoutQuorum(replicaUrls, { (name, url) => BftFinality.httpPost(s"$url/federation/view-change", body).map(_ => ()) })

  /** Quorum EVIDENCE (not response timing) that the cluster has installed
    * `targetView` or later. Mirrors [[BftFinality]]'s private helper of the
    * same purpose.
    */
  private def viewQuorumConfirmed(
      replicaUrls: Map[String, String], targetView: Int, authorities: Map[String, Vector[Byte]],
      federationId: Digest, expectedReplicaSet: Digest, timeoutMs: Long = 1500L,
  ): Boolean =
    val urls = replicaUrls.values.toList
    if urls.isEmpty then false
    else
      val exec = java.util.concurrent.Executors.newFixedThreadPool(math.min(urls.size, 8).max(1))
      try
        val futs = urls.map { url => java.util.concurrent.CompletableFuture.supplyAsync(() => fetchViewStatus(url), exec) }
        val pending = scala.collection.mutable.ListBuffer.from(futs)
        val results = scala.collection.mutable.ListBuffer.empty[Either[String, FederationViewStatus]]
        val deadline = System.nanoTime() + timeoutMs * 1000000L
        while pending.nonEmpty && System.nanoTime() < deadline do
          val done = pending.filter(_.isDone).toList
          if done.isEmpty then Thread.sleep(5)
          else
            done.foreach { f =>
              try results += f.getNow(null)
              catch case e: Exception => results += Left(e.getMessage)
              pending -= f
            }
        pending.foreach(_.cancel(true))
        val verified = results.collect { case Right(vs) => vs }
          .filter(vs => verifyViewStatus(authorities, vs, federationId, expectedReplicaSet).isRight)
        val q = BftQuorum.quorumSize(authorities.size)
        verified.filter(_.view >= targetView).map(_.replica.id).distinct.length >= q
      finally exec.shutdownNow()

  /** Polls replica URLs for a certificate matching this exact proposal
    * (state digest AND epoch, since a state digest alone could coincide
    * across generations), verifying each candidate before it can win the
    * race — mirrors [[BftFinality]]'s private `pollCert`.
    */
  private def pollCert(
      replicaUrls: Map[String, String], proposal: FederationProposal, polls: Int, pollSleepMs: Long,
      authorities: Map[String, Vector[Byte]], expectedReplicaSet: Digest,
  ): Either[String, FederationFinalityCertificate] =
    def fetchOnce(): Option[FederationFinalityCertificate] =
      val urls = replicaUrls.values.toList
      if urls.isEmpty then None
      else
        val exec = java.util.concurrent.Executors.newFixedThreadPool(math.min(urls.size, 8).max(1))
        val found = new java.util.concurrent.atomic.AtomicReference[FederationFinalityCertificate](null)
        val remaining = new java.util.concurrent.atomic.AtomicInteger(urls.size)
        val done = new java.util.concurrent.CountDownLatch(1)
        try
          urls.foreach { url =>
            exec.execute(() =>
              try
                fetchCerts(url).toOption.toList.flatten
                  .find(c => c.stateDigest == proposal.after && c.epoch == proposal.epoch)
                  .foreach { c =>
                    if FederationFinalityCertificate.verify(c, authorities, expectedReplicaSet).isRight then
                      found.compareAndSet(null, c)
                      done.countDown()
                  }
              finally
                if remaining.decrementAndGet() == 0 then done.countDown()
            )
          }
          done.await(BftFinality.RpcTimeout.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
          Option(found.get())
        finally exec.shutdownNow()
    def loop(n: Int): Either[String, FederationFinalityCertificate] =
      if n <= 0 then Left("federation network: no certificate minted")
      else
        fetchOnce() match
          case Some(c) => Right(c)
          case None =>
            if pollSleepMs > 0 then Thread.sleep(pollSleepMs)
            loop(n - 1)
    loop(polls)

  /** Request successor view-change; advances only on quorum EVIDENCE the
    * cluster actually installed it (never this client's own fan-out
    * response timing) — mirrors [[BftFinality]]'s private `kickViewChange`.
    */
  private def kickViewChange(
      replicaUrls: Map[String, String], currentView: Int, initiator: Signer, proposal: FederationProposal,
      polls: Int, backoffMs: Long, authorities: Map[String, Vector[Byte]],
  ): Either[String, Int] =
    val target = currentView + 1
    requestNetworkViewChange(replicaUrls, target, initiator, proposal.federationId, proposal.replicaSet)
    Thread.sleep(Math.max(backoffMs, 40L))
    if viewQuorumConfirmed(replicaUrls, target, authorities, proposal.federationId, proposal.replicaSet) then
      Right(target)
    else
      pollCert(replicaUrls, proposal, Math.min(polls, 4), Math.max(backoffMs / 4, 10L), authorities, proposal.replicaSet) match
        case Right(_) => Right(currentView)
        case Left(_)  => Right(currentView)

  /** Deployable path: ask the primary to propose; on timeout, run
    * view-change and retry. Mirrors [[BftFinality.agreeNetworkRemote]]
    * exactly, keyed to [[FederationProposal]] instead of a bare block
    * digest/height/parent/chainId.
    */
  def agreeNetworkRemote(
      replicaUrls: Map[String, String],
      proposal: FederationProposal,
      initiator: Signer,
      authorities: Map[String, Vector[Byte]],
      view: Int = 0,
      polls: Int = 64,
      pollSleepMs: Long = 30,
      maxViews: Int = 4,
  ): Either[String, FederationFinalityCertificate] =
    val ids = replicaUrls.keys.toList.map(ReplicaId(_))
    def attempt(v: Int, remaining: Int, backoffMs: Long): Either[String, FederationFinalityCertificate] =
      if remaining <= 0 then Left(s"federation network: no certificate after $maxViews views")
      else if v > view + maxViews then Left(s"federation network: view $v exceeds bound ${view + maxViews}")
      else
        designatedPrimary(ids, v).toRight(s"federation: no designated primary for view $v").flatMap { primaryId =>
          replicaUrls.get(primaryId.id).toRight(s"federation: no URL for primary ${primaryId.id}").flatMap { _ =>
            val shortPolls = Math.max(4, polls / 8)
            propose(replicaUrls, initiator, v, proposal) match
              case Left(_) =>
                pollCert(replicaUrls, proposal, shortPolls, pollSleepMs, authorities, proposal.replicaSet) match
                  case Right(c) => Right(c)
                  case Left(_) =>
                    kickViewChange(replicaUrls, v, initiator, proposal, shortPolls, backoffMs, authorities).flatMap { nextV =>
                      attempt(nextV, remaining - 1, Math.min(backoffMs * 2, 2000L))
                    }
              case Right(()) =>
                pollCert(replicaUrls, proposal, polls, pollSleepMs, authorities, proposal.replicaSet) match
                  case Right(c) => Right(c)
                  case Left(_) =>
                    kickViewChange(replicaUrls, v, initiator, proposal, shortPolls, backoffMs, authorities).flatMap { nextV =>
                      attempt(nextV, remaining - 1, Math.min(backoffMs * 2, 2000L))
                    }
          }
        }
    attempt(view, maxViews, Math.max(pollSleepMs, 50L))
