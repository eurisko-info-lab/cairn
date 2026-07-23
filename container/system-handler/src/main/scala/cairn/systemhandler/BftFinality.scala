package cairn.systemhandler

import cairn.kernel.*
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets

/** Network BFT finality over sealed PoA block digests.
  *
  * Elevates [[BftQuorum]] from an in-process research sim to a certificate
  * protocol: authenticated replicas exchange signed PrePrepare/Prepare/Commit
  * messages, then mint a [[FinalityCertificate]] once **2f+1 distinct**
  * replicas commit.
  *
  * Honesty bounds:
  * - Replica set is static ([[PeerRegistry]] `role=replica` or explicit keys).
  * - Safety under `f < n/3`; n must be a valid `3f+1` size (`n >= 4` for f≥1,
  *   or n=1 for the trivial single-replica lab case).
  * - Certifies that a **replay-valid sealed PoA block** on a local chain
  *   reached BFT agreement — not an arbitrary digest.
  */
object BftFinality:
  import BftQuorum.*

  /** Signed-message domain tag — binds protocol version into every seal. */
  val MsgDomain: String = "cairn-bft-v1"

  /** Chain identity = genesis block digest (first digest on the ledger chain). */
  def chainId(node: Node): Either[String, Digest] =
    node.readChainDigests.flatMap {
      case Nil    => Left("bft: empty chain has no chainId (genesis required)")
      case g :: _ => Right(g)
    }

  def chainId(blocks: List[Block]): Either[String, Digest] =
    blocks.headOption.map(_.digest).toRight("bft: empty candidate chain has no chainId")

  /** Canonical identity of a replica set: digest of sorted (id, publicKey) pairs. */
  def replicaSetDigest(authorities: Map[String, Vector[Byte]]): Digest =
    Digest.of(Canon.CList(
      authorities.toList.sortBy(_._1).map { (id, pk) =>
        Canon.cmap("id" -> Canon.CStr(id), "publicKey" -> Canon.CBytes(pk))
      }))

  /** @deprecated Prefer [[replicaSetDigest(Map)]] — names alone do not bind keys. */
  def replicaSetDigest(replicaIds: List[String]): Digest =
    Digest.of(Canon.cstrs(replicaIds.sorted))

  def designatedPrimary(replicaIds: List[String], view: Int): Either[String, ReplicaId] =
    val sorted = replicaIds.sorted
    if sorted.isEmpty then Left("bft: empty replica set")
    else if sorted.length != sorted.distinct.length then Left("bft: duplicate replica ids")
    else if !BftQuorum.validReplicaCount(sorted.length) then
      Left(s"bft: n=${sorted.length} is not a valid 3f+1 size (need 1,4,7,10,…)")
    else Right(ReplicaId(sorted(Math.floorMod(view, sorted.length))))

  def msgFrom(msg: Msg): ReplicaId = msg match
    case Msg.PrePrepare(_, _, _, from) => from
    case Msg.Prepare(_, _, _, from)    => from
    case Msg.Commit(_, _, _, from)     => from
    case Msg.ViewChange(_, _, from)    => from
    case Msg.NewView(_, _, from, _)    => from

  final case class SignedMsg(
      msg: Msg,
      signer: ReplicaId,
      seal: Vector[Byte],
      replicaSet: Digest,
      chainId: Digest,
  ):
    def payload: Array[Byte] = Canon.encode(Canon.cmap(
      "domain" -> Canon.CStr(MsgDomain),
      "chainId" -> Canon.CStr(chainId.hex),
      "replicaSet" -> Canon.CStr(replicaSet.hex),
      "msg" -> msgCanon(msg)))
    def canon: Canon = Canon.CTag("bft-signed", Canon.cmap(
      "msg" -> msgCanon(msg),
      "signer" -> Canon.CStr(signer.id),
      "seal" -> Canon.CBytes(seal),
      "replicaSet" -> Canon.CStr(replicaSet.hex),
      "chainId" -> Canon.CStr(chainId.hex)))

  object SignedMsg:
    def fromCanon(c: Canon): Either[String, SignedMsg] =
      import Canon.*
      c match
        case CTag("bft-signed", m) =>
          parseMsg(m.field("msg")).flatMap { msg =>
            m.field("seal") match
              case CBytes(bs) =>
                val fields = m.asMap
                val rs = fields.get("replicaSet").map(f => Digest(f.asStr)).getOrElse(
                  Digest.of(Canon.CStr("")))
                fields.get("chainId") match
                  case None => Left("bft-signed: missing chainId")
                  case Some(cid) =>
                    Right(SignedMsg(
                      msg, ReplicaId(m.field("signer").asStr), bs, rs, Digest(cid.asStr)))
              case other => Left(s"bad seal: $other")
          }
        case other => Left(s"not a bft-signed message: $other")

  def sign(
      kp: Signer,
      msg: Msg,
      replicaSet: Digest,
      chainId: Digest,
  ): Either[String, SignedMsg] =
    val rid = ReplicaId(kp.name)
    if rid != msgFrom(msg) then Left(s"bft: cannot sign msg.from=${msgFrom(msg).id} as ${rid.id}")
    else
      val sm = SignedMsg(msg, rid, Vector.empty, replicaSet, chainId)
      val seal = kp.sign(sm.payload)
      Right(sm.copy(seal = seal))

  /** @deprecated Prefer [[sign(Signer, Msg, Digest, Digest)]]. */
  def sign(kp: Signer, msg: Msg, replicaSet: Digest): Either[String, SignedMsg] =
    sign(kp, msg, replicaSet, Digest.of(Canon.CStr("")))

  /** @deprecated Prefer [[sign(Signer, Msg, Digest, Digest)]]. */
  def sign(kp: Signer, msg: Msg): Either[String, SignedMsg] =
    sign(kp, msg, Digest.of(Canon.CStr("")), Digest.of(Canon.CStr("")))

  /** Verify signature AND `signer == msg.from`, and that signer is known. */
  def verify(
      authorities: Map[String, Vector[Byte]],
      sm: SignedMsg,
      expectedReplicaSet: Option[Digest] = None,
      expectedChainId: Option[Digest] = None,
  ): Either[String, Unit] =
    if sm.signer != msgFrom(sm.msg) then
      Left(s"bft: signer '${sm.signer.id}' != msg.from '${msgFrom(sm.msg).id}'")
    else
      expectedChainId match
        case Some(cid) if sm.chainId != cid =>
          Left(s"bft: message chainId ${sm.chainId.short} != expected ${cid.short}")
        case _ =>
          expectedReplicaSet match
            case Some(rs) if sm.replicaSet != rs =>
              Left(
                s"bft: message replicaSet ${sm.replicaSet.short} != expected ${rs.short}")
            case _ =>
              authorities.get(sm.signer.id) match
                case None => Left(s"unknown bft replica '${sm.signer.id}'")
                case Some(pk) =>
                  if Ed25519.verify(pk, sm.payload, sm.seal) then Right(())
                  else Left(s"bad bft seal from ${sm.signer.id}")

  private def preparedCanon(ps: List[PreparedCert]): Canon =
    Canon.CList(ps.map { p =>
      Canon.cmap(
        "seq" -> Canon.CInt(p.seq),
        "digest" -> Canon.CStr(p.valueDigest.hex),
        "preparedView" -> Canon.CInt(p.preparedView),
        "value" -> p.value.fold(Canon.CTag("none", Canon.CInt(0)))(v =>
          Canon.CTag("some", Canon.CBytes(v.bytes))),
        "prepareVotes" -> Canon.CList(p.prepareVotes.map { (rid, seal) =>
          Canon.cmap("from" -> Canon.CStr(rid.id), "seal" -> Canon.CBytes(seal))
        }),
        "prePrepareSeal" -> p.prePrepareSeal.fold(Canon.CTag("none", Canon.CInt(0)))(s =>
          Canon.CTag("some", Canon.CBytes(s))),
        "prePrepareFrom" -> p.prePrepareFrom.fold(Canon.CTag("none", Canon.CInt(0)))(r =>
          Canon.CTag("some", Canon.CStr(r.id))))
    })

  private def parsePrepared(c: Canon): List[PreparedCert] =
    import Canon.*
    c.asList.map { row =>
      val fields = row.asMap
      val value = fields.get("value") match
        case Some(CTag("some", CBytes(bs))) => Some(Value(bs))
        case _                              => None
      val votes = fields.get("prepareVotes") match
        case Some(CList(xs)) =>
          xs.map { r =>
            ReplicaId(r.field("from").asStr) -> (r.field("seal") match
              case CBytes(bs) => bs
              case _ => throw CodecError("prepare vote seal"))
          }
        case _ => Nil
      val ppSeal = fields.get("prePrepareSeal") match
        case Some(CTag("some", CBytes(bs))) => Some(bs)
        case _                              => None
      val ppFrom = fields.get("prePrepareFrom") match
        case Some(CTag("some", CStr(id))) => Some(ReplicaId(id))
        case _                            => None
      PreparedCert(
        row.field("seq").asInt.toInt,
        Digest(row.field("digest").asStr),
        row.field("preparedView").asInt.toInt,
        value,
        votes,
        ppSeal,
        ppFrom)
    }

  private def viewChangeEvidenceCanon(ev: ViewChangeEvidence): Canon =
    Canon.cmap(
      "vc" -> msgCanon(ev.vc),
      "seal" -> Canon.CBytes(ev.seal),
      "replicaSet" -> Canon.CStr(ev.replicaSet.hex),
      "chainId" -> Canon.CStr(ev.chainId.hex))

  private def parseViewChangeEvidence(c: Canon): ViewChangeEvidence =
    import Canon.*
    parseMsg(c.field("vc")) match
      case Right(vc: Msg.ViewChange) =>
        val seal = c.field("seal") match
          case CBytes(bs) => bs
          case _ => throw CodecError("view-change evidence seal")
        ViewChangeEvidence(
          vc,
          seal,
          Digest(c.field("replicaSet").asStr),
          Digest(c.field("chainId").asStr))
      case Right(_) => throw CodecError("view-change evidence must wrap ViewChange")
      case Left(e) => throw CodecError(e)

  def msgCanon(msg: Msg): Canon = msg match
    case Msg.PrePrepare(view, seq, value, from) =>
      Canon.CTag("pre-prepare", Canon.cmap(
        "view" -> Canon.CInt(view),
        "seq" -> Canon.CInt(seq),
        "value" -> Canon.CBytes(value.bytes),
        "from" -> Canon.CStr(from.id)))
    case Msg.Prepare(view, seq, d, from) =>
      Canon.CTag("prepare", Canon.cmap(
        "view" -> Canon.CInt(view),
        "seq" -> Canon.CInt(seq),
        "digest" -> Canon.CStr(d.hex),
        "from" -> Canon.CStr(from.id)))
    case Msg.Commit(view, seq, d, from) =>
      Canon.CTag("commit", Canon.cmap(
        "view" -> Canon.CInt(view),
        "seq" -> Canon.CInt(seq),
        "digest" -> Canon.CStr(d.hex),
        "from" -> Canon.CStr(from.id)))
    case Msg.ViewChange(newView, prepared, from) =>
      Canon.CTag("view-change", Canon.cmap(
        "newView" -> Canon.CInt(newView),
        "prepared" -> preparedCanon(prepared),
        "from" -> Canon.CStr(from.id)))
    case Msg.NewView(newView, prepared, from, evidence) =>
      Canon.CTag("new-view", Canon.cmap(
        "newView" -> Canon.CInt(newView),
        "prepared" -> preparedCanon(prepared),
        "from" -> Canon.CStr(from.id),
        "evidence" -> Canon.CList(evidence.map(viewChangeEvidenceCanon))))

  def parseMsg(c: Canon): Either[String, Msg] =
    import Canon.*
    try c match
      case CTag("pre-prepare", m) =>
        m.field("value") match
          case CBytes(bs) =>
            Right(Msg.PrePrepare(
              m.field("view").asInt.toInt,
              m.field("seq").asInt.toInt,
              Value(bs),
              ReplicaId(m.field("from").asStr)))
          case _ => Left("pre-prepare value")
      case CTag("prepare", m) =>
        Right(Msg.Prepare(
          m.field("view").asInt.toInt,
          m.field("seq").asInt.toInt,
          Digest(m.field("digest").asStr),
          ReplicaId(m.field("from").asStr)))
      case CTag("commit", m) =>
        Right(Msg.Commit(
          m.field("view").asInt.toInt,
          m.field("seq").asInt.toInt,
          Digest(m.field("digest").asStr),
          ReplicaId(m.field("from").asStr)))
      case CTag("view-change", m) =>
        Right(Msg.ViewChange(
          m.field("newView").asInt.toInt,
          parsePrepared(m.field("prepared")),
          ReplicaId(m.field("from").asStr)))
      case CTag("new-view", m) =>
        val fields = m.asMap
        val evidence = fields.get("evidence") match
          case Some(CList(xs)) => xs.map(parseViewChangeEvidence)
          case _ =>
            // Legacy unsigned bodies — rejected by the network path.
            fields.get("viewChanges") match
              case Some(CList(xs)) =>
                xs.map { row =>
                  parseMsg(row) match
                    case Right(vc: Msg.ViewChange) =>
                      ViewChangeEvidence(
                        vc, Vector.empty, Digest.of(Canon.CStr("")), Digest.of(Canon.CStr("")))
                    case Right(_) => throw CodecError("new-view evidence must be view-change")
                    case Left(e) => throw CodecError(e)
                }
              case _ => Nil
        Right(Msg.NewView(
          m.field("newView").asInt.toInt,
          parsePrepared(m.field("prepared")),
          ReplicaId(m.field("from").asStr),
          evidence))
      case other => Left(s"unknown bft msg: $other")
    catch case e: CodecError => Left(e.getMessage)

  /** Authenticated request to ask a primary to propose a sealed block. */
  final case class ProposeRequest(
      view: Int,
      block: Digest,
      signer: ReplicaId,
      seal: Vector[Byte],
      chainId: Digest,
      replicaSet: Digest,
  ):
    def payload: Array[Byte] = Canon.encode(Canon.cmap(
      "domain" -> Canon.CStr(MsgDomain),
      "chainId" -> Canon.CStr(chainId.hex),
      "replicaSet" -> Canon.CStr(replicaSet.hex),
      "view" -> Canon.CInt(view),
      "block" -> Canon.CStr(block.hex)))
    def canon: Canon = Canon.CTag("bft-propose-req", Canon.cmap(
      "view" -> Canon.CInt(view),
      "block" -> Canon.CStr(block.hex),
      "signer" -> Canon.CStr(signer.id),
      "seal" -> Canon.CBytes(seal),
      "chainId" -> Canon.CStr(chainId.hex),
      "replicaSet" -> Canon.CStr(replicaSet.hex)))

  object ProposeRequest:
    def sign(
        signer: Signer,
        block: Digest,
        chainId: Digest,
        replicaSet: Digest,
        view: Int = 0,
    ): ProposeRequest =
      val req = ProposeRequest(
        view, block, ReplicaId(signer.name), Vector.empty, chainId, replicaSet)
      req.copy(seal = signer.sign(req.payload))

    def fromCanon(c: Canon): Either[String, ProposeRequest] =
      import Canon.*
      c match
        case CTag("bft-propose-req", m) =>
          Digest.parse(m.field("block").asStr).flatMap { block =>
            m.field("seal") match
              case CBytes(bs) =>
                val fields = m.asMap
                (fields.get("chainId"), fields.get("replicaSet")) match
                  case (Some(cid), Some(rs)) =>
                    Right(ProposeRequest(
                      fields.get("view").map(_.asInt.toInt).getOrElse(0),
                      block,
                      ReplicaId(m.field("signer").asStr),
                      bs,
                      Digest(cid.asStr),
                      Digest(rs.asStr)))
                  case _ => Left("bft-propose-req: missing chainId/replicaSet epoch binding")
              case other => Left(s"bad propose seal: $other")
          }
        case other => Left(s"not a bft-propose-req: $other")

  def verifyProposeRequest(
      authorities: Map[String, Vector[Byte]],
      req: ProposeRequest,
      expectedChainId: Option[Digest] = None,
      expectedReplicaSet: Option[Digest] = None,
  ): Either[String, Unit] =
    expectedChainId match
      case Some(cid) if req.chainId != cid =>
        Left(s"bft: propose chainId ${req.chainId.short} != expected ${cid.short}")
      case _ =>
        expectedReplicaSet match
          case Some(rs) if req.replicaSet != rs =>
            Left(s"bft: propose replicaSet ${req.replicaSet.short} != expected ${rs.short}")
          case _ =>
            authorities.get(req.signer.id) match
              case None => Left(s"unknown bft replica '${req.signer.id}'")
              case Some(pk) =>
                if Ed25519.verify(pk, req.payload, req.seal) then Right(())
                else Left(s"bad propose seal from ${req.signer.id}")

  /** Authenticated request to trigger a view-change on a replica. */
  final case class ViewChangeRequest(
      newView: Int,
      signer: ReplicaId,
      seal: Vector[Byte],
      chainId: Digest,
      replicaSet: Digest,
  ):
    def payload: Array[Byte] = Canon.encode(Canon.cmap(
      "domain" -> Canon.CStr(MsgDomain),
      "chainId" -> Canon.CStr(chainId.hex),
      "replicaSet" -> Canon.CStr(replicaSet.hex),
      "newView" -> Canon.CInt(newView)))
    def canon: Canon = Canon.CTag("bft-view-change-req", Canon.cmap(
      "newView" -> Canon.CInt(newView),
      "signer" -> Canon.CStr(signer.id),
      "seal" -> Canon.CBytes(seal),
      "chainId" -> Canon.CStr(chainId.hex),
      "replicaSet" -> Canon.CStr(replicaSet.hex)))

  object ViewChangeRequest:
    def sign(
        signer: Signer,
        newView: Int,
        chainId: Digest,
        replicaSet: Digest,
    ): ViewChangeRequest =
      val req = ViewChangeRequest(
        newView, ReplicaId(signer.name), Vector.empty, chainId, replicaSet)
      req.copy(seal = signer.sign(req.payload))

    def fromCanon(c: Canon): Either[String, ViewChangeRequest] =
      import Canon.*
      c match
        case CTag("bft-view-change-req", m) =>
          m.field("seal") match
            case CBytes(bs) =>
              val fields = m.asMap
              (fields.get("chainId"), fields.get("replicaSet")) match
                case (Some(cid), Some(rs)) =>
                  Right(ViewChangeRequest(
                    m.field("newView").asInt.toInt,
                    ReplicaId(m.field("signer").asStr),
                    bs,
                    Digest(cid.asStr),
                    Digest(rs.asStr)))
                case _ => Left("bft-view-change-req: missing chainId/replicaSet epoch binding")
            case other => Left(s"bad view-change seal: $other")
        case other => Left(s"not a bft-view-change-req: $other")

  def verifyViewChangeRequest(
      authorities: Map[String, Vector[Byte]],
      req: ViewChangeRequest,
      expectedChainId: Option[Digest] = None,
      expectedReplicaSet: Option[Digest] = None,
  ): Either[String, Unit] =
    expectedChainId match
      case Some(cid) if req.chainId != cid =>
        Left(s"bft: view-change chainId ${req.chainId.short} != expected ${cid.short}")
      case _ =>
        expectedReplicaSet match
          case Some(rs) if req.replicaSet != rs =>
            Left(s"bft: view-change replicaSet ${req.replicaSet.short} != expected ${rs.short}")
          case _ =>
            authorities.get(req.signer.id) match
              case None => Left(s"unknown bft replica '${req.signer.id}'")
              case Some(pk) =>
                if Ed25519.verify(pk, req.payload, req.seal) then Right(())
                else Left(s"bad view-change seal from ${req.signer.id}")

  /** Drop slots / seals at or below the finalized high-water; drop meta for certified blocks. */
  def compactFinalized(
      state: BftQuorum.ReplicaState,
      commitSeals: Map[(Int, Int, String, String), Vector[Byte]],
      prepareSeals: Map[(Int, Int, String, String), Vector[Byte]],
      blockMeta: Map[Digest, (Long, Digest)],
      certificates: List[FinalityCertificate],
      finalizedHighWater: Long,
      viewChangeEvidence: Map[(Int, String), ViewChangeEvidence] = Map.empty,
      prePrepareSeals: Map[(Int, Int, String), (ReplicaId, Vector[Byte])] = Map.empty,
  ): (
      BftQuorum.ReplicaState,
      Map[(Int, Int, String, String), Vector[Byte]],
      Map[(Int, Int, String, String), Vector[Byte]],
      Map[Digest, (Long, Digest)],
      Map[(Int, String), ViewChangeEvidence],
      Map[(Int, Int, String), (ReplicaId, Vector[Byte])],
  ) =
    val hw = finalizedHighWater.toInt
    val slots = state.slots.filter { case ((_, seq), _) => seq > hw }
    val seals = commitSeals.filter { case ((_, seq, _, _), _) => seq > hw }
    val prepares = prepareSeals.filter { case ((_, seq, _, _), _) => seq > hw }
    val certified = certificates.map(_.blockDigest).toSet
    val meta = blockMeta.filterNot { case (d, _) => certified.contains(d) }
    // Keep VC evidence for the current and future views only.
    val keptVcs = viewChangeEvidence.filter { case ((nv, _), _) => nv >= state.view }
    val keptPp = prePrepareSeals.filter { case ((_, seq, _), _) => seq > hw }
    (state.copy(slots = slots), seals, prepares, meta, keptVcs, keptPp)

  /** Seal a genesis replica-set from in-process keypairs (lab / tests). */
  def sealReplicaSet(
      replicas: List[Keypair],
      replaces: Option[Digest] = None,
      activationHeight: Long = 0L,
  ): Either[String, ReplicaSetManifest] =
    for
      unsigned <- ReplicaSetManifest.of(
        replicas.map(k => k.name -> k.publicBytes), replaces, activationHeight)
      sealedM <- ReplicaSetManifest.seal(
        unsigned,
        replicas.map(k => k.name -> ((msg: Array[Byte]) => k.sign(msg))))
      _ <- ReplicaSetManifest.verifySeals(sealedM, Ed25519.verify)
    yield sealedM

  /** Certificate that a sealed PoA block reached 2f+1 **distinct** commits. */
  final case class FinalityCertificate(
      blockDigest: Digest,
      view: Int,
      seq: Int,
      commits: List[(ReplicaId, Vector[Byte])],
      replicaSet: Digest,
      height: Long,
      parent: Digest,
      chainId: Digest,
  ):
    def canon: Canon = Canon.CTag("bft-finality", Canon.cmap(
      "block" -> Canon.CStr(blockDigest.hex),
      "view" -> Canon.CInt(view),
      "seq" -> Canon.CInt(seq),
      "commits" -> Canon.CList(commits.sortBy(_._1.id).map { (id, seal) =>
        Canon.cmap("replica" -> Canon.CStr(id.id), "seal" -> Canon.CBytes(seal))
      }),
      "replicaSet" -> Canon.CStr(replicaSet.hex),
      "height" -> Canon.CInt(height),
      "parent" -> Canon.CStr(parent.hex),
      "chainId" -> Canon.CStr(chainId.hex)))
    def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)
    def digest: Digest = artifact.digest

  object FinalityCertificate:
    def fromCanon(c: Canon): Either[String, FinalityCertificate] =
      import Canon.*
      c match
        case CTag("bft-finality", m) =>
          try
            val commits = m.field("commits").asList.map { row =>
              ReplicaId(row.field("replica").asStr) -> (row.field("seal") match
                case CBytes(bs) => bs
                case _          => throw CodecError("seal"))
            }
            val fields = m.asMap
            val chainId = fields.get("chainId").map(f => Digest(f.asStr)).getOrElse(
              throw CodecError("bft-finality: missing chainId"))
            Right(FinalityCertificate(
              Digest(m.field("block").asStr),
              m.field("view").asInt.toInt,
              m.field("seq").asInt.toInt,
              commits,
              Digest(m.field("replicaSet").asStr),
              m.field("height").asInt,
              Digest(m.field("parent").asStr),
              chainId))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not bft-finality: $other")

    /** Quorum certificate check against a verified [[ReplicaSetManifest]]. */
    def verify(cert: FinalityCertificate, manifest: ReplicaSetManifest): Either[String, Unit] =
      ReplicaSetManifest.verifySeals(manifest, Ed25519.verify).flatMap { _ =>
        verify(cert, manifest.authorities, manifest.replicaSetDigest)
      }

    /** Quorum certificate check: distinct known replicas, matching replica-set
      * digest, valid Commit seals for the encoded block value.
      *
      * Prefer [[verify(FinalityCertificate, ReplicaSetManifest)]] so the digest
      * is the full sealed-manifest body (keys + transition metadata).
      */
    def verify(
        cert: FinalityCertificate,
        authorities: Map[String, Vector[Byte]],
        expectedReplicaSet: Digest,
    ): Either[String, Unit] =
      val n = authorities.size
      if !BftQuorum.validReplicaCount(n) then
        Left(s"bft finality: n=$n is not a valid 3f+1 size")
      else if cert.seq.toLong != cert.height then
        Left(
          s"bft finality: certificate sequence ${cert.seq} does not equal block height ${cert.height}")
      else
        val q = quorumSize(n)
        val ids = cert.commits.map(_._1.id)
        if cert.replicaSet != expectedReplicaSet then
          Left(s"bft finality: replicaSet ${cert.replicaSet.short} != expected ${expectedReplicaSet.short}")
        else if ids.length != ids.distinct.length then
          Left(s"bft finality: duplicate replica commits: ${ids.mkString(",")}")
        else if ids.exists(id => !authorities.contains(id)) then
          Left(s"bft finality: unknown replica in commits")
        else if ids.distinct.length < q then
          Left(s"bft finality: ${ids.distinct.length} distinct commits < quorum $q")
        else
          val valueDigest = valueOfBlock(cert.blockDigest).digest
          cert.commits.foldLeft[Either[String, Unit]](Right(())) { case (acc, (id, seal)) =>
            acc.flatMap { _ =>
              val commit = Msg.Commit(cert.view, cert.seq, valueDigest, id)
              BftFinality.verify(
                authorities,
                SignedMsg(commit, id, seal, cert.replicaSet, cert.chainId),
                Some(cert.replicaSet),
                Some(cert.chainId))
            }
          }

    /** Re-check [[FinalityCertificate.height]] / [[FinalityCertificate.parent]]
      * against a replay-valid sealed block on `node`.
      */
    def verifyAgainstChain(
        cert: FinalityCertificate,
        authorities: Map[String, Vector[Byte]],
        expectedReplicaSet: Digest,
        node: Node,
        ledgerAuth: Map[String, Vector[Byte]],
    ): Either[String, Unit] =
      verify(cert, authorities, expectedReplicaSet).flatMap { _ =>
        chainId(node).flatMap { genesis =>
          if cert.chainId != genesis then
            Left(s"bft finality: chainId ${cert.chainId.short} != genesis ${genesis.short}")
          else
            requireSealedBlock(node, ledgerAuth, cert.blockDigest).flatMap { (block, height) =>
              if cert.height != height then
                Left(s"bft finality: height ${cert.height} != chain height $height")
              else if cert.parent != block.parent then
                Left(
                  s"bft finality: parent ${cert.parent.short} != block parent ${block.parent.short}")
              else Right(())
            }
        }
      }

    def verifyAgainstChain(
        cert: FinalityCertificate,
        manifest: ReplicaSetManifest,
        node: Node,
        ledgerAuth: Map[String, Vector[Byte]],
    ): Either[String, Unit] =
      verifyAgainstChain(cert, manifest.authorities, manifest.replicaSetDigest, node, ledgerAuth)

    /** Verify seals/chain and that `cert.replicaSet` is the active set at `cert.height`. */
    def verifyAgainstHistory(
        cert: FinalityCertificate,
        history: ValidatedReplicaSetHistory,
        node: Node,
        ledgerAuth: Map[String, Vector[Byte]],
    ): Either[String, Unit] =
      history.activeAt(cert.height).flatMap { active =>
        if cert.replicaSet != active.replicaSetDigest then
          Left(
            s"bft finality: cert replicaSet ${cert.replicaSet.short} is not active " +
              s"at height ${cert.height} (active=${active.replicaSetDigest.short})")
        else verifyAgainstChain(cert, active, node, ledgerAuth)
      }

    /** Verify a certificate against a candidate replayed chain, before that
      * chain is installed as this node's durable chain. */
    def verifyAgainstBlocks(
        cert: FinalityCertificate,
        history: ValidatedReplicaSetHistory,
        blocks: List[Block],
        ledgerAuth: Map[String, Vector[Byte]],
    ): Either[String, Unit] =
      history.activeAt(cert.height).flatMap { active =>
        if cert.replicaSet != active.replicaSetDigest then
          Left(
            s"bft finality: cert replicaSet ${cert.replicaSet.short} is not active " +
              s"at height ${cert.height} (active=${active.replicaSetDigest.short})")
        else
          blocks.zipWithIndex.find(_._1.digest == cert.blockDigest) match
            case None => Left(s"bft finality: ${cert.blockDigest.short} is not in candidate chain")
            case Some((block, index)) =>
              chainId(blocks).flatMap { genesis =>
                if cert.chainId != genesis then
                  Left(s"bft finality: chainId ${cert.chainId.short} != genesis ${genesis.short}")
                else
                  LedgerKernel.replay(ledgerAuth, blocks.take(index + 1), Ed25519.verify).flatMap { _ =>
                    if cert.height != index.toLong then
                      Left(s"bft finality: height ${cert.height} != candidate height $index")
                    else if cert.parent != block.parent then
                      Left(
                        s"bft finality: parent ${cert.parent.short} != block parent ${block.parent.short}")
                    else verify(cert, active)
                  }
              }
      }

  def valueOfBlock(blockDigest: Digest): Value =
    Value(blockDigest.hex.getBytes(StandardCharsets.US_ASCII).toVector)

  /** Require `d` to be a block on `node`'s replay-valid chain. */
  def requireSealedBlock(
      node: Node,
      authorities: Map[String, Vector[Byte]],
      d: Digest,
  ): Either[String, (Block, Long)] =
    node.blocks.flatMap { bs =>
      bs.zipWithIndex.find(_._1.digest == d) match
        case None => Left(s"bft: ${d.short} is not a sealed block on this chain")
        case Some((block, idx)) =>
          // Replay through this height — reject if the chain is invalid.
          LedgerKernel.replay(authorities, bs.take(idx + 1), Ed25519.verify).map(_ => (block, idx.toLong))
    }

  /** In-process agreement bound to a replay-valid sealed block on `node`.
    * Sequence is always the sealed block height (caller `seq` is ignored).
    */
  def agreeForSealedBlock(
      node: Node,
      ledgerAuth: Map[String, Vector[Byte]],
      replicas: List[Keypair],
      view: Int,
      seq: Int,
      blockDigest: Digest,
  ): Either[String, FinalityCertificate] =
    for
      genesis <- chainId(node)
      proved <- requireSealedBlock(node, ledgerAuth, blockDigest)
      (block, height) = proved
      derived <- seqForHeight(height)
      cert <- agreeLocalProven(
        replicas, view, derived, blockDigest, height, block.parent, genesis)
    yield cert

  /** Prefer [[agreeForSealedBlock(Node, Map, List, Digest, Int)]] — seq is height-bound. */
  def agreeForSealedBlock(
      node: Node,
      ledgerAuth: Map[String, Vector[Byte]],
      replicas: List[Keypair],
      blockDigest: Digest,
      view: Int = 0,
  ): Either[String, FinalityCertificate] =
    agreeForSealedBlock(node, ledgerAuth, replicas, view, seq = 0, blockDigest)

  /** Agree when the caller already proved the block (height/parent known). */
  def agreeLocalProven(
      replicas: List[Keypair],
      view: Int,
      seq: Int,
      blockDigest: Digest,
      height: Long,
      parent: Digest,
      chainId: Digest,
      maxRounds: Int = 16,
  ): Either[String, FinalityCertificate] =
    val ids = replicas.map(_.name)
    for
      primaryId <- designatedPrimary(ids, view)
      primary <- replicas.find(_.name == primaryId.id).toRight(s"bft: missing primary ${primaryId.id}")
      cert <- runAgreement(
        replicas, primary, view, seq, blockDigest, height, parent, chainId, maxRounds)
    yield cert

  private def runAgreement(
      replicas: List[Keypair],
      primary: Keypair,
      view: Int,
      seq: Int,
      blockDigest: Digest,
      height: Long,
      parent: Digest,
      chainId: Digest,
      maxRounds: Int,
  ): Either[String, FinalityCertificate] =
    val ids = replicas.map(k => ReplicaId(k.name))
    sealReplicaSet(replicas).flatMap { manifest =>
      val auth = manifest.authorities
      val setDig = manifest.replicaSetDigest
      val value = valueOfBlock(blockDigest)
      val primaryId = ReplicaId(primary.name)
      var states: Map[ReplicaId, ReplicaState] =
        ids.map(id => id -> ReplicaState(id, ids.length, faulty = false)).toMap
      sign(primary, Msg.PrePrepare(view, seq, value, primaryId), setDig, chainId).flatMap { pp =>
        var inbox: List[SignedMsg] = List(pp)
        var round = 0
        var commitSeals: Map[ReplicaId, Vector[Byte]] = Map.empty
        while inbox.nonEmpty && round < maxRounds do
          val batch = inbox
          inbox = Nil
          batch.foreach { sm =>
            verify(auth, sm, Some(setDig), Some(chainId)) match
              case Left(_) => ()
              case Right(()) =>
                ids.foreach { rid =>
                  val (st2, out) = deliver(states(rid), sm.msg)
                  states = states + (rid -> st2)
                  out.foreach { m =>
                    val kp = replicas.find(_.name == rid.id).get
                    sign(kp, m, setDig, chainId).foreach { signed =>
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
        if decided.isEmpty then Left("bft: no decision")
        else if !honestAgree(
            states.map { (id, st) =>
              id -> st.slots.get((view, seq)).flatMap(s =>
                s.decided.map(v => Decision(view, seq, v, s.commits.keys.toList)))
            },
            Set.empty) then Left("bft: honest disagreement")
        else
          val q = quorumSize(ids.length)
          val commits = commitSeals.toList
          if commits.map(_._1.id).distinct.length < q then
            Left(s"bft: only ${commits.map(_._1.id).distinct.length} distinct commits, need $q")
          else
            val cert = FinalityCertificate(
              blockDigest, view, seq, commits, setDig, height, parent, chainId)
            FinalityCertificate.verify(cert, manifest).map(_ => cert)
      }
    }

  private val client = HttpClient.newHttpClient()

  def loadCerts(path: java.nio.file.Path): Either[String, List[FinalityCertificate]] =
    if !java.nio.file.Files.exists(path) then Right(Nil)
    else
      Canon.decode(java.nio.file.Files.readAllBytes(path)).flatMap {
        case Canon.CList(xs) =>
          xs.foldLeft[Either[String, List[FinalityCertificate]]](Right(Nil)) { (acc, c) =>
            acc.flatMap(cs => FinalityCertificate.fromCanon(c).map(cs :+ _))
          }
        case other => Left(s"bad cert store: $other")
      }

  def saveCerts(path: java.nio.file.Path, certs: List[FinalityCertificate]): Either[String, Unit] =
    DurableIo.writeConsensus(path, Canon.encode(Canon.CList(certs.map(_.canon))))

  def defaultCertStorePath(home: java.nio.file.Path): java.nio.file.Path =
    home.resolve("bft-certs.canon")

  def defaultAdoptionIntentPath(home: java.nio.file.Path): java.nio.file.Path =
    home.resolve("bft-adoption.canon")

  /** One recoverable follower adoption generation: certs → checkpoint → chain. */
  final case class AdoptionIntent(
      remoteChain: List[Digest],
      certDigests: List[Digest],
      phase: String,
  ):
    def canon: Canon = Canon.CTag("bft-adoption", Canon.cmap(
      "remoteChain" -> Canon.cstrs(remoteChain.map(_.hex)),
      "certDigests" -> Canon.cstrs(certDigests.map(_.hex)),
      "phase" -> Canon.CStr(phase)))

  object AdoptionIntent:
    def fromCanon(c: Canon): Either[String, AdoptionIntent] =
      import Canon.*
      c match
        case CTag("bft-adoption", m) =>
          try
            Right(AdoptionIntent(
              m.field("remoteChain").asList.map(r => Digest(r.asStr)),
              m.field("certDigests").asList.map(r => Digest(r.asStr)),
              m.field("phase").asStr))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not bft-adoption: $other")

  def loadAdoptionIntent(home: java.nio.file.Path): Either[String, Option[AdoptionIntent]] =
    val path = defaultAdoptionIntentPath(home)
    if !java.nio.file.Files.exists(path) then Right(None)
    else
      Canon.decode(java.nio.file.Files.readAllBytes(path)).flatMap(AdoptionIntent.fromCanon).map(Some(_))

  def saveAdoptionIntent(home: java.nio.file.Path, intent: AdoptionIntent): Either[String, Unit] =
    DurableIo.writeConsensus(defaultAdoptionIntentPath(home), Canon.encode(intent.canon))

  def clearAdoptionIntent(home: java.nio.file.Path): Either[String, Unit] =
    val path = defaultAdoptionIntentPath(home)
    try
      java.nio.file.Files.deleteIfExists(path)
      Right(())
    catch case e: Exception => Left(e.getMessage)

  /** Merge verified remote certificates into the durable store (before checkpoint). */
  def mergeVerifiedCerts(
      home: java.nio.file.Path,
      verified: List[FinalityCertificate],
  ): Either[String, List[FinalityCertificate]] =
    val path = defaultCertStorePath(home)
    loadCerts(path).flatMap { existing =>
      val byDigest = (existing ++ verified).groupBy(_.digest).view.mapValues(_.head).toMap
      val merged = byDigest.values.toList.sortBy(c => (c.height, c.digest.hex))
      saveCerts(path, merged).map(_ => merged)
    }

  /** Persist verified certs, then advance checkpoint summaries — never reverse that order. */
  def adoptVerifiedCertificates(
      home: java.nio.file.Path,
      verified: List[FinalityCertificate],
  ): Either[String, Boolean] =
    if verified.isEmpty then Right(false)
    else
      mergeVerifiedCerts(home, verified).flatMap { _ =>
        verified.sortBy(_.height).foldLeft[Either[String, Boolean]](Right(false)) { (acc, cert) =>
          acc.flatMap { changed =>
            advanceCheckpoint(home, cert).map(cp => changed || cp.certificate == cert.digest)
          }
        }
      }

  /** Commit chain+certs+checkpoint as one recoverable generation.
    * Call after remote certs are verified against the candidate chain.
    */
  def adoptFollowerGeneration(
      home: java.nio.file.Path,
      node: Node,
      remoteChain: List[Digest],
      verifiedCerts: List[FinalityCertificate],
  ): Either[String, Unit] =
    val intent = AdoptionIntent(
      remoteChain,
      verifiedCerts.map(_.digest),
      phase = "started")
    for
      _ <- saveAdoptionIntent(home, intent)
      _ <- adoptVerifiedCertificates(home, verifiedCerts)
      _ <- saveAdoptionIntent(home, intent.copy(phase = "certs-checkpoint"))
      _ <- node.writeChain(remoteChain)
      _ <- clearAdoptionIntent(home)
    yield ()

  /** Resume a crash-interrupted follower adoption generation. */
  def resumeFollowerAdoption(
      home: java.nio.file.Path,
      node: Node,
  ): Either[String, Unit] =
    loadAdoptionIntent(home).flatMap {
      case None => Right(())
      case Some(intent) =>
        loadCerts(defaultCertStorePath(home)).flatMap { certs =>
          val wanted = intent.certDigests.toSet
          val present = certs.filter(c => wanted.contains(c.digest))
          if present.map(_.digest).toSet != wanted then
            Left(
              s"bft adoption: incomplete certs for interrupted generation " +
                s"(have ${present.size}/${wanted.size})")
          else
            adoptVerifiedCertificates(home, present).flatMap { _ =>
              node.writeChain(intent.remoteChain).flatMap(_ => clearAdoptionIntent(home))
            }
        }
    }

  /** Durable finalized ledger checkpoint — chain adoption must extend this block. */
  final case class FinalizedCheckpoint(
      block: Digest,
      height: Long,
      certificate: Digest,
      replicaSet: Digest,
  ):
    def canon: Canon = Canon.CTag("bft-checkpoint", Canon.cmap(
      "block" -> Canon.CStr(block.hex),
      "height" -> Canon.CInt(height),
      "certificate" -> Canon.CStr(certificate.hex),
      "replicaSet" -> Canon.CStr(replicaSet.hex)))

  object FinalizedCheckpoint:
    def fromCanon(c: Canon): Either[String, FinalizedCheckpoint] =
      c match
        case Canon.CTag("bft-checkpoint", m) =>
          try
            Right(FinalizedCheckpoint(
              Digest(m.field("block").asStr),
              m.field("height").asInt,
              Digest(m.field("certificate").asStr),
              Digest(m.field("replicaSet").asStr)))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not bft-checkpoint: $other")

    def fromCertificate(cert: FinalityCertificate): FinalizedCheckpoint =
      FinalizedCheckpoint(cert.blockDigest, cert.height, cert.digest, cert.replicaSet)

    /** Highest-height certificate wins; ties prefer larger certificate digest hex. */
    def bestOf(certs: List[FinalityCertificate]): Option[FinalizedCheckpoint] =
      certs.sortBy(c => (c.height, c.digest.hex)).lastOption.map(fromCertificate)

  def defaultCheckpointPath(home: java.nio.file.Path): java.nio.file.Path =
    home.resolve("finalized-checkpoint.canon")

  /** Decode only. Call [[loadVerifiedCheckpoint]] before trusting it for chain safety. */
  def loadCheckpoint(home: java.nio.file.Path): Either[String, Option[FinalizedCheckpoint]] =
    val path = defaultCheckpointPath(home)
    if !java.nio.file.Files.exists(path) then Right(None)
    else
      Canon.decode(java.nio.file.Files.readAllBytes(path))
        .flatMap(FinalizedCheckpoint.fromCanon).map(Some(_))

  /** Load and prove the durable checkpoint against its stored certificate,
    * replay-verified membership history, and current sealed chain. */
  def loadVerifiedCheckpoint(
      home: java.nio.file.Path,
      node: Node,
      ledgerAuth: Map[String, Vector[Byte]],
  ): Either[String, Option[FinalizedCheckpoint]] =
    loadCheckpoint(home).left.map(e => s"bft checkpoint corrupt: $e").flatMap {
      case None => Right(None)
      case Some(cp) =>
        loadCerts(home.resolve("bft-certs.canon")).left.map(e => s"bft checkpoint cert store corrupt: $e")
          .flatMap { certs =>
            certs.find(_.digest == cp.certificate).toRight(
              s"bft checkpoint corrupt: certificate ${cp.certificate.short} is missing").flatMap { cert =>
              if cert.blockDigest != cp.block || cert.height != cp.height || cert.replicaSet != cp.replicaSet then
                Left("bft checkpoint corrupt: checkpoint does not match certificate")
              else
                loadReplicaSetHistory(home).flatMap { hist =>
                  FinalityCertificate.verifyAgainstHistory(cert, hist, node, ledgerAuth)
                    .left.map(e => s"bft checkpoint corrupt: $e")
                    .map(_ => Some(cp))
                }
            }
          }
    }

  def saveCheckpoint(home: java.nio.file.Path, cp: FinalizedCheckpoint): Either[String, Unit] =
    DurableIo.writeConsensus(defaultCheckpointPath(home), Canon.encode(cp.canon))

  /** Advance durable checkpoint when `cert` is strictly higher (or first). */
  def advanceCheckpoint(
      home: java.nio.file.Path,
      cert: FinalityCertificate,
  ): Either[String, FinalizedCheckpoint] =
    val next = FinalizedCheckpoint.fromCertificate(cert)
    loadCheckpoint(home).flatMap {
      case None => saveCheckpoint(home, next).map(_ => next)
      case Some(cur) if next.height > cur.height =>
        saveCheckpoint(home, next).map(_ => next)
      case Some(cur) if next.height == cur.height && next.block == cur.block =>
        Right(cur)
      case Some(cur) if next.height == cur.height && next.block != cur.block =>
        Left(
          s"bft checkpoint: conflicting finality at height ${cur.height} " +
            s"(have ${cur.block.short}, got ${next.block.short})")
      case Some(cur) => Right(cur)
    }

  /** Proposed chain must contain the finalized block at its certified height. */
  def requireExtendsCheckpoint(
      chain: List[Digest],
      checkpoint: Option[FinalizedCheckpoint],
  ): Either[String, Unit] =
    checkpoint match
      case None => Right(())
      case Some(cp) =>
        if cp.height < 0 then Left(s"bft checkpoint: negative height ${cp.height}")
        else if chain.length <= cp.height then
          Left(
            s"bft checkpoint: chain length ${chain.length} does not reach " +
              s"finalized height ${cp.height} (${cp.block.short})")
        else if chain(cp.height.toInt) != cp.block then
          Left(
            s"bft checkpoint: chain[${cp.height}]=${chain(cp.height.toInt).short} " +
              s"!= finalized ${cp.block.short}")
        else Right(())

  /** Gossip/pull ranking: longer wins, tip-hex tie-break — but never below checkpoint. */
  def shouldAdoptChain(
      mine: List[Digest],
      theirs: List[Digest],
      checkpoint: Option[FinalizedCheckpoint],
  ): Boolean =
    requireExtendsCheckpoint(theirs, checkpoint).isRight &&
      (theirs.length > mine.length ||
        (theirs.length == mine.length && theirs != mine &&
          theirs.lastOption.map(_.hex).getOrElse("") <
            mine.lastOption.map(_.hex).getOrElse("~")))

  def loadReplicaSet(path: java.nio.file.Path): Either[String, ReplicaSetManifest] =
    if !java.nio.file.Files.exists(path) then Left(s"missing replica-set manifest at $path")
    else
      Canon.decode(java.nio.file.Files.readAllBytes(path)).flatMap(ReplicaSetManifest.fromCanon)
        .flatMap { m =>
          ReplicaSetManifest.verifySeals(m, Ed25519.verify).map(_ => m)
        }

  /** Tip of validated history when present; otherwise `replica-set.canon`. */
  def loadReplicaSetTip(home: java.nio.file.Path): Either[String, ReplicaSetManifest] =
    if java.nio.file.Files.exists(defaultReplicaSetHistoryPath(home)) then
      loadReplicaSetHistory(home).map(_.manifests.last)
    else loadReplicaSet(defaultReplicaSetPath(home))

  /** Membership that is live at `height` (not merely the latest configured tip). */
  def loadActiveReplicaSet(
      home: java.nio.file.Path,
      height: Long,
  ): Either[String, ReplicaSetManifest] =
    loadReplicaSetHistory(home).flatMap(_.activeAt(height))

  def defaultReplicaSetPath(home: java.nio.file.Path): java.nio.file.Path =
    home.resolve("replica-set.canon")

  def defaultReplicaSetHistoryPath(home: java.nio.file.Path): java.nio.file.Path =
    home.resolve("replica-set-history.canon")

  /** Load ordered historical manifests and replay-verify the transition chain. */
  def loadReplicaSetHistory(home: java.nio.file.Path): Either[String, ValidatedReplicaSetHistory] =
    val path = defaultReplicaSetHistoryPath(home)
    val raw: Either[String, List[ReplicaSetManifest]] =
      if !java.nio.file.Files.exists(path) then
        if java.nio.file.Files.exists(defaultReplicaSetPath(home)) then
          loadReplicaSet(defaultReplicaSetPath(home)).map(List(_))
        else Right(Nil)
      else
        Canon.decode(java.nio.file.Files.readAllBytes(path)).flatMap {
          case Canon.CList(xs) =>
            xs.foldLeft[Either[String, List[ReplicaSetManifest]]](Right(Nil)) { (acc, c) =>
              acc.flatMap(hs => ReplicaSetManifest.fromCanon(c).map(hs :+ _))
            }
          case other => Left(s"bad replica-set history: $other")
        }
    raw.flatMap(ValidatedReplicaSetHistory.verify(_, Ed25519.verify))

  /** Persist a sealed replica-set as current tip and append to durable history.
    * Amendments require predecessor quorum approvals ([[ReplicaSetManifest.allowsTransition]]).
    */
  def saveReplicaSet(path: java.nio.file.Path, m: ReplicaSetManifest): Either[String, Unit] =
    val home = Option(path.getParent).getOrElse(path.toAbsolutePath.getParent)
    for
      _ <- ReplicaSetManifest.verifySeals(m, Ed25519.verify)
      history <-
        if java.nio.file.Files.exists(defaultReplicaSetHistoryPath(home)) ||
            java.nio.file.Files.exists(defaultReplicaSetPath(home)) then
          loadReplicaSetHistory(home).map(_.manifests)
        else Right(Nil)
      live = history.lastOption
      certHighWater <- loadCerts(home.resolve("bft-certs.canon")).map(_.map(_.height).foldLeft(-1L)(_ max _))
      checkpointHighWater <- loadCheckpoint(home).map(_.map(_.height).getOrElse(-1L))
      minActivation = certHighWater max checkpointHighWater
      _ <- ReplicaSetManifest.allowsTransition(
        m, live, live.map(_.digest), Ed25519.verify, minActivation)
      newHistory = history.filterNot(_.digest == m.digest) :+ m
      _ <- ValidatedReplicaSetHistory.verify(newHistory, Ed25519.verify)
      _ <- DurableIo.writeConsensus(
        defaultReplicaSetHistoryPath(home),
        Canon.encode(Canon.CList(newHistory.map(_.canon))))
      // Derived cache of history tip (history file is authoritative).
      _ <- DurableIo.writeConsensus(path, Canon.encode(m.canon))
    yield ()

  /** Active replica-set at a ledger height from durable, replay-verified history. */
  def activeReplicaSetAt(home: java.nio.file.Path, height: Long): Either[String, ReplicaSetManifest] =
    loadReplicaSetHistory(home).flatMap(_.activeAt(height))

  /** Map a sealed block height to the BFT sequence number (monotonic with the ledger). */
  def seqForHeight(height: Long): Either[String, Int] =
    if height < 0 then Left(s"bft: negative height $height")
    else if height > Int.MaxValue then Left(s"bft: height $height exceeds Int.MaxValue")
    else Right(height.toInt)

  /** Encode durable protocol state (slots + seals + block meta + VC evidence). */
  def encodeReplicaState(
      state: BftQuorum.ReplicaState,
      commitSeals: Map[(Int, Int, String, String), Vector[Byte]],
      blockMeta: Map[Digest, (Long, Digest)],
      replicaSet: Digest,
      prepareSeals: Map[(Int, Int, String, String), Vector[Byte]] = Map.empty,
      viewChangeEvidence: List[ViewChangeEvidence] = Nil,
      prePrepareSeals: Map[(Int, Int, String), (ReplicaId, Vector[Byte])] = Map.empty,
  ): Canon =
    import BftQuorum.*
    val slots = state.slots.toList.map { case ((view, seq), slot) =>
      Canon.cmap(
        "view" -> Canon.CInt(view),
        "seq" -> Canon.CInt(seq),
        "prePrepare" -> slot.prePrepare.fold(Canon.CTag("none", Canon.CInt(0)))(pp =>
          Canon.CTag("some", msgCanon(pp))),
        "prepares" -> Canon.CList(slot.prepares.toList.map { (id, d) =>
          Canon.cmap("from" -> Canon.CStr(id.id), "digest" -> Canon.CStr(d.hex))
        }),
        "commits" -> Canon.CList(slot.commits.toList.map { (id, d) =>
          Canon.cmap("from" -> Canon.CStr(id.id), "digest" -> Canon.CStr(d.hex))
        }),
        "decided" -> slot.decided.fold(Canon.CTag("none", Canon.CInt(0)))(v =>
          Canon.CTag("some", Canon.CBytes(v.bytes))))
    }
    val seals = commitSeals.toList.map { case ((v, s, hex, rid), seal) =>
      Canon.cmap(
        "view" -> Canon.CInt(v), "seq" -> Canon.CInt(s),
        "digest" -> Canon.CStr(hex), "replica" -> Canon.CStr(rid),
        "seal" -> Canon.CBytes(seal))
    }
    val prepares = prepareSeals.toList.map { case ((v, s, hex, rid), seal) =>
      Canon.cmap(
        "view" -> Canon.CInt(v), "seq" -> Canon.CInt(s),
        "digest" -> Canon.CStr(hex), "replica" -> Canon.CStr(rid),
        "seal" -> Canon.CBytes(seal))
    }
    val meta = blockMeta.toList.map { case (d, hp) =>
      val (h, p) = hp
      Canon.cmap(
        "block" -> Canon.CStr(d.hex), "height" -> Canon.CInt(h), "parent" -> Canon.CStr(p.hex))
    }
    Canon.CTag("bft-replica-state", Canon.cmap(
      "id" -> Canon.CStr(state.id.id),
      "n" -> Canon.CInt(state.n),
      "view" -> Canon.CInt(state.view),
      "replicaSet" -> Canon.CStr(replicaSet.hex),
      "slots" -> Canon.CList(slots),
      "commitSeals" -> Canon.CList(seals),
      "prepareSeals" -> Canon.CList(prepares),
      "prePrepareSeals" -> Canon.CList(prePrepareSeals.toList.map {
        case ((v, s, hex), (rid, seal)) =>
          Canon.cmap(
            "view" -> Canon.CInt(v), "seq" -> Canon.CInt(s),
            "digest" -> Canon.CStr(hex), "replica" -> Canon.CStr(rid.id),
            "seal" -> Canon.CBytes(seal))
      }),
      "viewChangeEvidence" -> Canon.CList(viewChangeEvidence.map(viewChangeEvidenceCanon)),
      "blockMeta" -> Canon.CList(meta)))

  final case class DecodedReplicaState(
      state: BftQuorum.ReplicaState,
      commitSeals: Map[(Int, Int, String, String), Vector[Byte]],
      prepareSeals: Map[(Int, Int, String, String), Vector[Byte]],
      blockMeta: Map[Digest, (Long, Digest)],
      replicaSet: Digest,
      viewChangeEvidence: List[ViewChangeEvidence],
      prePrepareSeals: Map[(Int, Int, String), (ReplicaId, Vector[Byte])] = Map.empty,
  )

  def decodeReplicaStateWithReplicaSet(c: Canon): Either[String, DecodedReplicaState] =
    import Canon.*, BftQuorum.*
    c match
      case CTag("bft-replica-state", m) =>
        try
          val fields = m.asMap
          val id = ReplicaId(m.field("id").asStr)
          val n = m.field("n").asInt.toInt
          val view = fields.get("view").map(_.asInt.toInt).getOrElse(0)
          val replicaSet = Digest(m.field("replicaSet").asStr)
          val slots = m.field("slots").asList.map { row =>
            val view = row.field("view").asInt.toInt
            val seq = row.field("seq").asInt.toInt
            val pp = row.field("prePrepare") match
              case CTag("some", body) => parseMsg(body).toOption.collect { case p: Msg.PrePrepare => p }
              case _ => None
            val prepares = row.field("prepares").asList.map { r =>
              ReplicaId(r.field("from").asStr) -> Digest(r.field("digest").asStr)
            }.toMap
            val commits = row.field("commits").asList.map { r =>
              ReplicaId(r.field("from").asStr) -> Digest(r.field("digest").asStr)
            }.toMap
            val decided = row.field("decided") match
              case CTag("some", CBytes(bs)) => Some(Value(bs))
              case _ => None
            (view, seq) -> Slot(pp, prepares, commits, decided)
          }.toMap
          val seals = m.field("commitSeals").asList.map { row =>
            (row.field("view").asInt.toInt, row.field("seq").asInt.toInt,
              row.field("digest").asStr, row.field("replica").asStr) ->
              (row.field("seal") match
                case CBytes(bs) => bs
                case _ => throw CodecError("seal"))
          }.toMap
          val prepareSeals = fields.get("prepareSeals") match
            case Some(CList(xs)) =>
              xs.map { row =>
                (row.field("view").asInt.toInt, row.field("seq").asInt.toInt,
                  row.field("digest").asStr, row.field("replica").asStr) ->
                  (row.field("seal") match
                    case CBytes(bs) => bs
                    case _ => throw CodecError("prepare seal"))
              }.toMap
            case _ => Map.empty[(Int, Int, String, String), Vector[Byte]]
          val evidence = fields.get("viewChangeEvidence") match
            case Some(CList(xs)) => xs.map(parseViewChangeEvidence)
            case _               => Nil
          val prePrepareSeals = fields.get("prePrepareSeals") match
            case Some(CList(xs)) =>
              xs.map { row =>
                (row.field("view").asInt.toInt, row.field("seq").asInt.toInt, row.field("digest").asStr) ->
                  (ReplicaId(row.field("replica").asStr),
                    row.field("seal") match
                      case CBytes(bs) => bs
                      case _ => throw CodecError("pre-prepare seal"))
              }.toMap
            case _ => Map.empty[(Int, Int, String), (ReplicaId, Vector[Byte])]
          val viewChanges = evidence.foldLeft(Map.empty[Int, Map[ReplicaId, Msg.ViewChange]]) {
            (acc, ev) =>
              val forView = acc.getOrElse(ev.vc.newView, Map.empty) + (ev.vc.from -> ev.vc)
              acc + (ev.vc.newView -> forView)
          }
          val meta = m.field("blockMeta").asList.map { row =>
            Digest(row.field("block").asStr) ->
              (row.field("height").asInt, Digest(row.field("parent").asStr))
          }.toMap
          Right(DecodedReplicaState(
            ReplicaState(id, n, faulty = false, view = view, slots = slots, viewChanges = viewChanges),
            seals,
            prepareSeals,
            meta,
            replicaSet,
            evidence,
            prePrepareSeals))
        catch case e: CodecError => Left(e.getMessage)
      case other => Left(s"not bft-replica-state: $other")

  /** Legacy-compatible decoded state view. Durable restore must use
    * [[decodeReplicaStateWithReplicaSet]] and check the manifest binding. */
  def decodeReplicaState(c: Canon): Either[String, (BftQuorum.ReplicaState, Map[(Int, Int, String, String), Vector[Byte]], Map[Digest, (Long, Digest)])] =
    decodeReplicaStateWithReplicaSet(c).map(d => (d.state, d.commitSeals, d.blockMeta))

  def postMsg(baseUrl: String, sm: SignedMsg): Either[String, Unit] =
    try
      val resp = client.send(
        HttpRequest.newBuilder(URI.create(s"$baseUrl/bft/msg"))
          .header("Content-Type", "application/octet-stream")
          .POST(HttpRequest.BodyPublishers.ofByteArray(Canon.encode(sm.canon)))
          .build(),
        HttpResponse.BodyHandlers.ofByteArray())
      if resp.statusCode() == 200 then Right(())
      else Left(s"POST $baseUrl/bft/msg -> ${resp.statusCode()}: ${new String(resp.body())}")
    catch case e: Exception => Left(e.getMessage)

  def fetchCerts(baseUrl: String): Either[String, List[FinalityCertificate]] =
    try
      val resp = client.send(
        HttpRequest.newBuilder(URI.create(s"$baseUrl/bft/certs")).GET().build(),
        HttpResponse.BodyHandlers.ofByteArray())
      if resp.statusCode() != 200 then Left(s"GET $baseUrl/bft/certs -> ${resp.statusCode()}")
      else
        Canon.decode(resp.body()).flatMap {
          case Canon.CList(xs) =>
            xs.foldLeft[Either[String, List[FinalityCertificate]]](Right(Nil)) { (acc, c) =>
              acc.flatMap(cs => FinalityCertificate.fromCanon(c).map(cs :+ _))
            }
          case other => Left(s"bad certs payload: $other")
        }
    catch case e: Exception => Left(e.getMessage)

  /** Ask the designated primary to propose (initiator needs a replica key, not the primary's).
    * Sequence is derived from block height on the primary — callers do not pick slots.
    */
  def propose(
      primaryUrl: String,
      blockDigest: Digest,
      initiator: Signer,
      chainId: Digest,
      replicaSet: Digest,
      view: Int = 0,
  ): Either[String, Unit] =
    try
      val req = ProposeRequest.sign(initiator, blockDigest, chainId, replicaSet, view)
      val body = Canon.encode(req.canon)
      val resp = client.send(
        HttpRequest.newBuilder(URI.create(s"$primaryUrl/bft/propose"))
          .header("Content-Type", "application/octet-stream")
          .POST(HttpRequest.BodyPublishers.ofByteArray(body))
          .build(),
        HttpResponse.BodyHandlers.ofByteArray())
      if resp.statusCode() == 200 then Right(())
      else Left(s"POST $primaryUrl/bft/propose -> ${resp.statusCode()}: ${new String(resp.body())}")
    catch case e: Exception => Left(e.getMessage)

  /** @deprecated Prefer [[propose(String, Digest, Signer, Digest, Digest, Int)]] — seq is height-bound. */
  def propose(
      primaryUrl: String,
      view: Int,
      seq: Int,
      blockDigest: Digest,
      initiator: Signer,
      chainId: Digest,
      replicaSet: Digest,
  ): Either[String, Unit] =
    // Still accepted for labs; primary ignores seq and re-derives from height.
    propose(primaryUrl, blockDigest, initiator, chainId, replicaSet, view)

  /** Broadcast PrePrepare from a local primary keypair, then poll for a cert.
    * `seq` must equal the sealed block's ledger height.
    */
  def agreeNetwork(
      primary: Keypair,
      replicaUrls: Map[String, String],
      view: Int,
      seq: Int,
      blockDigest: Digest,
      replicaSet: Digest,
      chainId: Digest,
      polls: Int = 32,
      pollSleepMs: Long = 25,
  ): Either[String, FinalityCertificate] =
    val ids = replicaUrls.keys.toList
    for
      primaryId <- designatedPrimary(ids, view)
      _ <- Either.cond(primaryId.id == primary.name, (),
        s"bft: primary for view $view is ${primaryId.id}, not ${primary.name}")
      value = valueOfBlock(blockDigest)
      pp <- sign(primary, Msg.PrePrepare(view, seq, value, primaryId), replicaSet, chainId)
      _ <- fanoutQuorum(replicaUrls, { (name, url) =>
        postMsg(url, pp).left.map(e => s"$e")
      })
      cert <- pollCert(replicaUrls, blockDigest, polls, pollSleepMs)
    yield cert

  /** Deployable path: ask the primary to propose; on timeout, run view-change and retry. */
  def agreeNetworkRemote(
      replicaUrls: Map[String, String],
      blockDigest: Digest,
      initiator: Signer,
      chainId: Digest,
      replicaSet: Digest,
      view: Int = 0,
      polls: Int = 64,
      pollSleepMs: Long = 30,
      maxViews: Int = 4,
  ): Either[String, FinalityCertificate] =
    val ids = replicaUrls.keys.toList
    def attempt(v: Int, remaining: Int): Either[String, FinalityCertificate] =
      if remaining <= 0 then Left(s"bft network: no certificate after $maxViews views")
      else
        designatedPrimary(ids, v).flatMap { primaryId =>
          replicaUrls.get(primaryId.id).toRight(s"bft: no URL for primary ${primaryId.id}").flatMap {
            primaryUrl =>
              propose(primaryUrl, blockDigest, initiator, chainId, replicaSet, v) match
                case Left(_) =>
                  // Primary unreachable — accept a cert already minted by backups, else view-change.
                  pollCert(replicaUrls, blockDigest, Math.max(4, polls / 8), pollSleepMs) match
                    case Right(c) => Right(c)
                    case Left(_) =>
                      requestNetworkViewChange(replicaUrls, v + 1, initiator, chainId, replicaSet).flatMap { _ =>
                        Thread.sleep(pollSleepMs * 2)
                        attempt(v + 1, remaining - 1)
                      }
                case Right(()) =>
                  pollCert(replicaUrls, blockDigest, polls, pollSleepMs) match
                    case Right(c) => Right(c)
                    case Left(_) =>
                      requestNetworkViewChange(replicaUrls, v + 1, initiator, chainId, replicaSet).flatMap { _ =>
                        Thread.sleep(pollSleepMs * 2)
                        attempt(v + 1, remaining - 1)
                      }
          }
        }
    attempt(view, maxViews)

  /** Fan-out a request to replica URLs; succeed once at least a BFT quorum accepts.
    * Up to `f` unreachable replicas (including a dead primary) are tolerated.
    */
  def fanoutQuorum(
      replicaUrls: Map[String, String],
      post: (String, String) => Either[String, Unit],
  ): Either[String, Unit] =
    val n = replicaUrls.size
    if !BftQuorum.validReplicaCount(n) then
      Left(s"bft: n=$n is not a valid 3f+1 size")
    else
      val q = BftQuorum.quorumSize(n)
      val results = replicaUrls.toList.map { (name, url) =>
        name -> post(name, url)
      }
      val ok = results.count(_._2.isRight)
      if ok >= q then Right(())
      else
        val errs = results.collect { case (name, Left(e)) => s"$name: $e" }
        Left(s"bft: quorum fan-out reached $ok/$n (need $q): ${errs.mkString("; ")}")

  /** Broadcast a view-change request; quorum acceptance is enough (dead primary OK). */
  def requestNetworkViewChange(
      replicaUrls: Map[String, String],
      newView: Int,
      initiator: Signer,
      chainId: Digest,
      replicaSet: Digest,
  ): Either[String, Unit] =
    try
      val req = ViewChangeRequest.sign(initiator, newView, chainId, replicaSet)
      val body = Canon.encode(req.canon)
      fanoutQuorum(replicaUrls, { (name, url) =>
        try
          val resp = client.send(
            HttpRequest.newBuilder(URI.create(s"$url/bft/view-change"))
              .header("Content-Type", "application/octet-stream")
              .POST(HttpRequest.BodyPublishers.ofByteArray(body))
              .build(),
            HttpResponse.BodyHandlers.ofByteArray())
          if resp.statusCode() == 200 then Right(())
          else Left(s"POST $url/bft/view-change -> ${resp.statusCode()}: ${new String(resp.body())}")
        catch case e: Exception => Left(e.getMessage)
      })
    catch case e: Exception => Left(e.getMessage)

  /** @deprecated Prefer the epoch-bound [[agreeNetworkRemote]] overload. */
  def agreeNetworkRemote(
      replicaUrls: Map[String, String],
      view: Int,
      seq: Int,
      blockDigest: Digest,
      initiator: Signer,
      polls: Int,
      pollSleepMs: Long,
  ): Either[String, FinalityCertificate] =
    Left("bft: agreeNetworkRemote requires chainId and replicaSet epoch binding")

  private def pollCert(
      replicaUrls: Map[String, String],
      blockDigest: Digest,
      polls: Int,
      pollSleepMs: Long,
  ): Either[String, FinalityCertificate] =
    def loop(n: Int): Either[String, FinalityCertificate] =
      if n <= 0 then Left("bft network: no certificate minted")
      else
        val found = replicaUrls.values.toList.view.flatMap { url =>
          fetchCerts(url).toOption.toList.flatten
        }.find(_.blockDigest == blockDigest)
        found match
          case Some(c) => Right(c)
          case None =>
            Thread.sleep(pollSleepMs)
            loop(n - 1)
    loop(polls)

/** Per-process BFT replica state machine backed by [[BftQuorum.deliver]].
  *
  * Authorities always come from a seal-verified [[ReplicaSetManifest]] — never
  * an ad-hoc public-key map. Prefer [[BftReplica.certified]].
  *
  * Vote persistence is part of the protocol transition: state is written with
  * [[DurableIo.writeConsensus]] **before** outbound signatures are exposed.
  * Any I/O failure enters a permanent fail-closed [[ioError]] state.
  */
final class BftReplica private (
    val keypair: Keypair,
    private var manifest: ReplicaSetManifest,
    private var history: ValidatedReplicaSetHistory,
    val node: Option[Node],
    val ledgerAuth: Map[String, Vector[Byte]],
    certStore: Option[java.nio.file.Path],
    stateStore: Option[java.nio.file.Path],
    home: Option[java.nio.file.Path],
    val chainId: Digest,
):
  import BftQuorum.*
  import BftFinality.*

  def authorities: Map[String, Vector[Byte]] = manifest.authorities
  def replicaIds: List[String] = manifest.ids
  private def n: Int = replicaIds.length
  private var state: ReplicaState = ReplicaState(ReplicaId(keypair.name), n, faulty = false)
  private val outbound = scala.collection.mutable.ListBuffer.empty[SignedMsg]
  /** Keyed by (view, seq, valueDigestHex, replicaId) — never overwrite peers. */
  private val commitSeals =
    scala.collection.mutable.Map.empty[(Int, Int, String, String), Vector[Byte]]
  /** Prepare seals keyed like commit seals — evidence for PreparedCert proofs. */
  private val prepareSeals =
    scala.collection.mutable.Map.empty[(Int, Int, String, String), Vector[Byte]]
  /** Signed PrePrepare seals keyed by (view, seq, valueDigestHex). */
  private val prePrepareSeals =
    scala.collection.mutable.Map.empty[(Int, Int, String), (ReplicaId, Vector[Byte])]
  /** Sealed ViewChange envelopes keyed by (newView, replicaId). */
  private val viewChangeEvidence =
    scala.collection.mutable.Map.empty[(Int, String), ViewChangeEvidence]
  private var certificates: List[FinalityCertificate] = Nil
  private var blockMeta: Map[Digest, (Long, Digest)] = Map.empty
  /** Highest ledger height for which this replica has minted a finality cert. */
  private var finalizedHighWater: Long = -1L
  /** Non-empty when restore failed or a later durable write failed. */
  private var ioError: Option[String] = None
  private var historyMtime: Long = home.map(BftReplica.historyFileMtime).getOrElse(0L)
  /** Tip height last used for membership adoption (mtime-independent). */
  private var membershipTipHeight: Long = -1L

  certStore.foreach { path =>
    if java.nio.file.Files.exists(path) then
      BftFinality.loadCerts(path) match
        case Right(cs) =>
          certificates = cs
          finalizedHighWater = cs.map(_.height).foldLeft(-1L)(_ max _)
          home.foreach { h =>
            cs.sortBy(_.height).foreach { cert =>
              advanceCheckpoint(h, cert) match
                case Left(e) => ioError = Some(s"bft-checkpoint restore failed: $e")
                case Right(_) => ()
            }
          }
        case Left(e) => ioError = Some(s"bft-certs restore failed: $e")
  }

  home.foreach { h =>
    if ioError.isEmpty then
      node match
        case Some(n) =>
          BftFinality.resumeFollowerAdoption(h, n) match
            case Left(e) => ioError = Some(s"bft-adoption resume failed: $e")
            case Right(()) => ()
        case None => ()
  }

  stateStore.foreach { path =>
    if java.nio.file.Files.exists(path) && ioError.isEmpty then
      Canon.decode(java.nio.file.Files.readAllBytes(path)).flatMap(
        BftFinality.decodeReplicaStateWithReplicaSet) match
        case Right(decoded)
            if decoded.state.id.id == keypair.name && decoded.state.n == n &&
              decoded.replicaSet == manifest.replicaSetDigest =>
          state = decoded.state
          commitSeals ++= decoded.commitSeals
          prepareSeals ++= decoded.prepareSeals
          prePrepareSeals ++= decoded.prePrepareSeals
          decoded.viewChangeEvidence.foreach { ev =>
            viewChangeEvidence((ev.vc.newView, ev.vc.from.id)) = ev
          }
          blockMeta = decoded.blockMeta
          // High-water is cert-mint only — noted blocks must not lock slots.
        case Right(decoded) =>
          ioError = Some(
            s"bft-state identity mismatch: file has ${decoded.state.id.id}/n=${decoded.state.n}, " +
              s"replica is ${keypair.name}/n=$n")
        case Left(e) =>
          ioError = Some(s"bft-state restore failed: $e")
  }

  def id: ReplicaId = ReplicaId(keypair.name)
  def setDigest: Digest = manifest.replicaSetDigest
  def drainOutbound(): List[SignedMsg] =
    val xs = outbound.toList
    outbound.clear()
    xs
  def finalityCerts: List[FinalityCertificate] = certificates

  private def refuseIfCorrupt[A](op: => Either[String, A]): Either[String, A] =
    ioError match
      case Some(e) => Left(s"bft: refusing to operate after durable I/O failure ($e)")
      case None    => op

  private def failClosed(err: String): Unit =
    ioError = Some(err)

  /** Atomically refresh membership when history file changes **or** tip height moves
    * across an activation boundary (mtime alone is not enough for staged sets).
    */
  def refreshHistory(): Either[String, Unit] =
    home match
      case None => Right(())
      case Some(h) =>
        val tipHeight =
          node match
            case Some(n) =>
              val digs = n.chainDigests
              if digs.isEmpty then 0L else (digs.length - 1).toLong
            case None =>
              finalizedHighWater.max(0L)
        val mt = BftReplica.historyFileMtime(h)
        val fileChanged = mt != historyMtime
        val heightMoved = tipHeight != membershipTipHeight
        if !fileChanged && !heightMoved then Right(())
        else
          val loaded =
            if fileChanged then
              BftFinality.loadReplicaSetHistory(h).map { vh =>
                history = vh
                historyMtime = mt
                vh
              }
            else Right(history)
          loaded.flatMap { vh =>
            membershipTipHeight = tipHeight
            vh.activeAt(tipHeight) match
              case Left(_) => Right(())
              case Right(active) =>
                if manifest.activationHeight > tipHeight then
                  // Staged future set — keep it; requireLocalActiveAt refuses early heights.
                  Right(())
                else
                  active.authorities.get(keypair.name) match
                    case Some(pk) if pk == keypair.publicBytes &&
                        active.replicaSetDigest != manifest.replicaSetDigest =>
                      // Clean epoch handoff — drop predecessor view/slots/seals.
                      state = ReplicaState(ReplicaId(keypair.name), active.n, faulty = false)
                      commitSeals.clear()
                      prepareSeals.clear()
                      prePrepareSeals.clear()
                      viewChangeEvidence.clear()
                      manifest = active
                      persistState().map(_ => ())
                    case _ => Right(())
          }

  /** Local set must be the active membership at `height` (checked every transition). */
  private def requireLocalActiveAt(height: Long): Either[String, ReplicaSetManifest] =
    refreshHistory().flatMap { _ =>
      if manifest.activationHeight > height then
        Left(
          s"bft: replica-set not yet active at height $height " +
            s"(activates at ${manifest.activationHeight})")
      else
        history.deactivationHeight(manifest) match
          case Some(d) if height >= d =>
            Left(
              s"bft: local set ${manifest.replicaSetDigest.short} deactivated at height $d " +
                s"(refusing operate at height $height)")
          case _ =>
            history.activeAt(height).flatMap { active =>
              if active.replicaSetDigest != manifest.replicaSetDigest then
                Left(
                  s"bft: local set ${manifest.replicaSetDigest.short} is not active at height $height " +
                    s"(active=${active.replicaSetDigest.short})")
              else Right(active)
            }
    }

  private def heightForValueDigest(d: Digest): Option[Long] =
    blockMeta.collectFirst {
      case (blockDig, (h, _)) if valueOfBlock(blockDig).digest == d => h
    }

  /** Bind ledger evidence for a block before / while agreeing. */
  def noteSealedBlock(blockDigest: Digest, height: Long, parent: Digest): Either[String, Unit] =
    refuseIfCorrupt {
      blockMeta = blockMeta + (blockDigest -> (height, parent))
      persistState()
    }

  /** Primary propose: sequence number is the sealed block's ledger height. */
  def proposeBlock(view: Int, blockDigest: Digest): Either[String, List[SignedMsg]] =
    refuseIfCorrupt {
      for
        _ <- Either.cond(view == state.view, (),
          s"bft: propose view $view != current view ${state.view}")
        primary <- BftFinality.designatedPrimary(replicaIds, view)
        _ <- Either.cond(primary.id == keypair.name, (),
          s"bft: this replica ${keypair.name} is not primary for view $view (${primary.id})")
        heightParent <- (node, ledgerAuth.nonEmpty) match
          case (Some(n), true) =>
            requireSealedBlock(n, ledgerAuth, blockDigest).map((b, h) => (h, b.parent))
          case _ =>
            blockMeta.get(blockDigest).toRight(s"bft: block ${blockDigest.short} not noted as sealed")
        (height, parent) = heightParent
        _ <- requireLocalActiveAt(height)
        seq <- seqForHeight(height)
        _ <- Either.cond(
          height > finalizedHighWater, (),
          s"bft: height $height already at/below finalized high-water $finalizedHighWater")
        _ <- noteSealedBlock(blockDigest, height, parent)
        pp <- sign(keypair, Msg.PrePrepare(view, seq, valueOfBlock(blockDigest), primary), setDigest, chainId)
        out <- receive(pp)
      yield pp :: out
    }

  /** Primary-only propose with explicit seq — must equal block height. */
  def propose(view: Int, seq: Int, blockDigest: Digest): Either[String, List[SignedMsg]] =
    refuseIfCorrupt {
      for
        heightParent <- (node, ledgerAuth.nonEmpty) match
          case (Some(n), true) =>
            requireSealedBlock(n, ledgerAuth, blockDigest).map((b, h) => (h, b.parent))
          case _ =>
            blockMeta.get(blockDigest).toRight(s"bft: block ${blockDigest.short} not noted as sealed")
        (height, _) = heightParent
        expected <- seqForHeight(height)
        _ <- Either.cond(seq == expected, (),
          s"bft: seq $seq must equal block height $height")
        out <- proposeBlock(view, blockDigest)
      yield out
    }

  def currentView: Int = state.view

  /** Prepared claims with mandatory signed PrePrepare + prepare-quorum seals + value.
    * Slots that cannot be proved are omitted (not asserted bare).
    */
  private def preparedWithProofs: List[PreparedCert] =
    preparedFromSlots(state, minSeqExclusive = finalizedHighWater.toInt).flatMap { pc =>
      val votes = prepareSeals.collect {
        case ((v, s, hex, rid), seal)
            if v == pc.preparedView && s == pc.seq && hex == pc.valueDigest.hex =>
          ReplicaId(rid) -> seal
      }.toList
      val pp = prePrepareSeals.get((pc.preparedView, pc.seq, pc.valueDigest.hex))
      if pc.value.isEmpty then None
      else if pp.isEmpty then None
      else if votes.map(_._1.id).distinct.length < quorumSize(n) then None
      else
        Some(pc.copy(
          prepareVotes = votes,
          prePrepareSeal = pp.map(_._2),
          prePrepareFrom = pp.map(_._1)))
    }

  /** Verify mandatory prepare-quorum evidence on a prepared claim. */
  private def verifyPreparedCert(pc: PreparedCert): Either[String, Unit] =
    if pc.value.isEmpty then
      Left(s"bft: prepared seq ${pc.seq} missing value")
    else if pc.value.exists(_.digest != pc.valueDigest) then
      Left(s"bft: prepared value digest mismatch at seq ${pc.seq}")
    else if pc.prePrepareSeal.isEmpty || pc.prePrepareFrom.isEmpty then
      Left(s"bft: prepared seq ${pc.seq} missing signed PrePrepare evidence")
    else if pc.prepareVotes.isEmpty then
      Left(s"bft: prepared seq ${pc.seq} missing prepare-quorum evidence")
    else
      val ids = pc.prepareVotes.map(_._1.id)
      if ids.length != ids.distinct.length then
        Left(s"bft: duplicate prepare votes for seq ${pc.seq}")
      else if ids.exists(id => !authorities.contains(id)) then
        Left(s"bft: unknown prepare voter for seq ${pc.seq}")
      else if ids.distinct.length < quorumSize(n) then
        Left(s"bft: prepare votes ${ids.distinct.length} < quorum ${quorumSize(n)}")
      else
        val from = pc.prePrepareFrom.get
        val pp = Msg.PrePrepare(pc.preparedView, pc.seq, pc.value.get, from)
        BftFinality.verify(
          authorities,
          SignedMsg(pp, from, pc.prePrepareSeal.get, setDigest, chainId),
          Some(setDigest),
          Some(chainId)).flatMap { _ =>
          pc.prepareVotes.foldLeft[Either[String, Unit]](Right(())) { case (acc, (rid, seal)) =>
            acc.flatMap { _ =>
              val prep = Msg.Prepare(pc.preparedView, pc.seq, pc.valueDigest, rid)
              BftFinality.verify(
                authorities,
                SignedMsg(prep, rid, seal, setDigest, chainId),
                Some(setDigest),
                Some(chainId))
            }
          }
        }

  /** Verify NewView as a real certificate of sealed ViewChange envelopes. */
  private def verifyNewViewCertificate(nv: Msg.NewView): Either[String, Unit] =
    if nv.evidence.isEmpty then
      Left("bft: NewView missing sealed ViewChange evidence")
    else if nv.evidence.exists(_.seal.isEmpty) then
      Left("bft: NewView ViewChange evidence missing seals")
    else
      val senders = nv.evidence.map(_.vc.from.id)
      if senders.length != senders.distinct.length then
        Left("bft: NewView duplicate ViewChange senders")
      else if senders.exists(id => !authorities.contains(id)) then
        Left("bft: NewView ViewChange from unknown replica")
      else if nv.evidence.exists(_.vc.newView != nv.newView) then
        Left(s"bft: NewView evidence targets wrong view (expected ${nv.newView})")
      else if nv.evidence.exists(ev => ev.replicaSet != setDigest || ev.chainId != chainId) then
        Left("bft: NewView evidence epoch (chainId/replicaSet) mismatch")
      else if nv.evidence.size < quorumSize(n) then
        Left(s"bft: NewView evidence ${nv.evidence.size} < quorum ${quorumSize(n)}")
      else
        val selected = selectPrepared(nv.evidence.map(_.vc))
        val selectedKeys = selected.map(p => (p.seq, p.valueDigest, p.preparedView))
        val claimedKeys = nv.prepared.map(p => (p.seq, p.valueDigest, p.preparedView))
        if selectedKeys != claimedKeys then
          Left("bft: NewView prepared does not match recomputed selectPrepared")
        else
          nv.evidence.foldLeft[Either[String, Unit]](Right(())) { (acc, ev) =>
            acc.flatMap { _ =>
              BftFinality.verify(
                authorities,
                SignedMsg(ev.vc, ev.vc.from, ev.seal, ev.replicaSet, ev.chainId),
                Some(setDigest),
                Some(chainId)).flatMap { _ =>
                ev.vc.prepared.foldLeft[Either[String, Unit]](Right(())) { (a, pc) =>
                  a.flatMap(_ => verifyPreparedCert(pc))
                }
              }
            }
          }.flatMap { _ =>
            nv.prepared.foldLeft[Either[String, Unit]](Right(())) { (a, pc) =>
              a.flatMap(_ => verifyPreparedCert(pc))
            }
          }

  /** Start a view-change toward `newView` (must be strictly greater than current).
    * Returns the local signed ViewChange plus any follow-on messages (e.g. NewView).
    */
  def requestViewChange(newView: Int): Either[String, List[SignedMsg]] =
    refuseIfCorrupt {
      if newView <= state.view then
        Left(s"bft: newView $newView must exceed current view ${state.view}")
      else
        sign(keypair, Msg.ViewChange(newView, preparedWithProofs, id), setDigest, chainId).flatMap { vc =>
          receive(vc).map(out => vc :: out)
        }
    }

  def receive(sm: SignedMsg): Either[String, List[SignedMsg]] =
    refuseIfCorrupt {
      val priorState = state
      val priorSeals = commitSeals.toMap
      val priorPrepares = prepareSeals.toMap
      val priorPp = prePrepareSeals.toMap
      val priorVc = viewChangeEvidence.toMap
      val priorMeta = blockMeta
      val priorCerts = certificates
      val priorHw = finalizedHighWater
      def rollback(): Unit =
        state = priorState
        commitSeals.clear()
        commitSeals ++= priorSeals
        prepareSeals.clear()
        prepareSeals ++= priorPrepares
        prePrepareSeals.clear()
        prePrepareSeals ++= priorPp
        viewChangeEvidence.clear()
        viewChangeEvidence ++= priorVc
        blockMeta = priorMeta
        certificates = priorCerts
        finalizedHighWater = priorHw

      /** Apply one verified message; return newly signed follow-ons (not `sm` itself). */
      def step(sm: SignedMsg): Either[String, List[SignedMsg]] =
        val ids = replicaIds.map(ReplicaId(_))
        val primaryOk = sm.msg match
          case Msg.PrePrepare(view, _, _, from) =>
            BftFinality.designatedPrimary(replicaIds, view).exists(_ == from)
          case Msg.NewView(newView, _, from, _) =>
            BftFinality.designatedPrimary(replicaIds, newView).exists(_ == from)
          case _ => true
        if !primaryOk then Left(s"bft: message from non-primary ${msgFrom(sm.msg).id}")
        else BftFinality.verify(authorities, sm, Some(setDigest), Some(chainId)).flatMap { _ =>
          val bind: Either[String, Unit] = sm.msg match
            case Msg.PrePrepare(_, seq, value, _) =>
              Digest.parse(new String(value.bytes.toArray, StandardCharsets.US_ASCII)) match
                case Left(e) => Left(e)
                case Right(blockDig) =>
                  preparedLock(state, seq, state.view) match
                    case Some(locked) if locked != value.digest =>
                      Left(s"bft: PrePrepare conflicts with prepared lock ${locked.short}")
                    case _ =>
                      (node, ledgerAuth.nonEmpty) match
                        case (Some(n), true) =>
                          requireSealedBlock(n, ledgerAuth, blockDig).flatMap { (b, h) =>
                            seqForHeight(h).flatMap { expected =>
                              if seq != expected then
                                Left(s"bft: PrePrepare seq $seq != block height $h")
                              else
                                requireLocalActiveAt(h).flatMap { _ =>
                                  noteSealedBlock(blockDig, h, b.parent).map(_ => ())
                                }
                            }
                          }
                        case _ =>
                          blockMeta.get(blockDig) match
                            case Some((h, _)) =>
                              if seq.toLong != h then
                                Left(s"bft: PrePrepare seq $seq != noted height $h")
                              else requireLocalActiveAt(h).map(_ => ())
                            case None =>
                              Left(s"bft: block ${blockDig.short} not noted as sealed")
            case Msg.Prepare(_, seq, _, _) =>
              requireLocalActiveAt(seq.toLong).map(_ => ())
            case Msg.Commit(_, seq, _, _) =>
              requireLocalActiveAt(seq.toLong).map(_ => ())
            case Msg.ViewChange(_, prepared, _) =>
              prepared.foldLeft[Either[String, Unit]](Right(())) { (acc, pc) =>
                acc.flatMap(_ => verifyPreparedCert(pc))
              }
            case nv: Msg.NewView =>
              verifyNewViewCertificate(nv)
          bind.flatMap { _ =>
            sm.msg match
              case Msg.Commit(view, seq, d, from) =>
                commitSeals((view, seq, d.hex, from.id)) = sm.seal
              case Msg.Prepare(view, seq, d, from) =>
                prepareSeals((view, seq, d.hex, from.id)) = sm.seal
              case Msg.PrePrepare(view, seq, value, from) =>
                prePrepareSeals((view, seq, value.digest.hex)) = (from, sm.seal)
              case vc: Msg.ViewChange =>
                viewChangeEvidence((vc.newView, vc.from.id)) =
                  ViewChangeEvidence(vc, sm.seal, sm.replicaSet, sm.chainId)
              case _ => ()
            val (st2, out) = sm.msg match
              case vc: Msg.ViewChange => deliverViewChange(state, vc, ids)
              case nv: Msg.NewView    => deliverNewView(state, nv, ids)
              case other              => deliver(state, other)
            state = st2
            out.foldLeft[Either[String, List[SignedMsg]]](Right(Nil)) { (acc, m) =>
              acc.flatMap { xs =>
                val sealedMsg: Either[String, Msg] = m match
                  case nv: Msg.NewView =>
                    val evidence = viewChangeEvidence.values.filter(_.vc.newView == nv.newView).toList
                    if evidence.size < quorumSize(n) then
                      Left(s"bft: cannot mint NewView without ${quorumSize(n)} sealed ViewChanges")
                    else if evidence.exists(_.seal.isEmpty) then
                      Left("bft: cannot mint NewView with unsigned ViewChange evidence")
                    else
                      val selected = selectPrepared(evidence.map(_.vc))
                      Right(nv.copy(evidence = evidence, prepared = selected))
                  case other => Right(other)
                sealedMsg.flatMap { m2 =>
                  val activeOk: Either[String, Unit] = m2 match
                    case Msg.Prepare(_, seq, d, _) =>
                      heightForValueDigest(d).orElse(Some(seq.toLong))
                        .toRight("bft: cannot sign Prepare for unknown value")
                        .flatMap(h => requireLocalActiveAt(h).map(_ => ()))
                    case Msg.Commit(_, seq, d, _) =>
                      heightForValueDigest(d).orElse(Some(seq.toLong))
                        .toRight("bft: cannot sign Commit for unknown value")
                        .flatMap(h => requireLocalActiveAt(h).map(_ => ()))
                    case Msg.PrePrepare(_, _, value, _) =>
                      Digest.parse(new String(value.bytes.toArray, StandardCharsets.US_ASCII))
                        .flatMap(d => blockMeta.get(d).map(_._1).toRight("bft: unknown PrePrepare block"))
                        .flatMap(h => requireLocalActiveAt(h).map(_ => ()))
                    case Msg.ViewChange(_, _, _) | Msg.NewView(_, _, _, _) =>
                      Right(())
                  activeOk.flatMap { _ =>
                    BftFinality.sign(keypair, m2, setDigest, chainId).map { s =>
                      m2 match
                        case Msg.Commit(view, seq, d, from) =>
                          commitSeals((view, seq, d.hex, from.id)) = s.seal
                        case Msg.Prepare(view, seq, d, from) =>
                          prepareSeals((view, seq, d.hex, from.id)) = s.seal
                        case Msg.PrePrepare(view, seq, value, from) =>
                          prePrepareSeals((view, seq, value.digest.hex)) = (from, s.seal)
                        case vc: Msg.ViewChange =>
                          viewChangeEvidence((vc.newView, vc.from.id)) =
                            ViewChangeEvidence(vc, s.seal, setDigest, chainId)
                        case _ => ()
                      xs :+ s
                    }
                  }
                }
              }
            }
          }
        }

      /** Deliver locally generated messages so NewView/PrePrepare close under self-delivery. */
      def pump(pending: List[SignedMsg], acc: List[SignedMsg]): Either[String, List[SignedMsg]] =
        pending match
          case Nil =>
            persistState().flatMap { _ =>
              tryMintCertificates().map { _ =>
                outbound ++= acc
                acc
              }
            }
          case h :: t =>
            step(h) match
              case Left(e) => Left(e)
              case Right(more) => pump(t ++ more, acc ++ more)

      step(sm).flatMap(first => pump(first, first)) match
        case Left(e) =>
          rollback()
          Left(e)
        case Right(out) => Right(out)
    }

  private def compactInMemory(): Unit =
    val (compacted, seals, prepares, meta, vcs, pps) = BftFinality.compactFinalized(
      state,
      commitSeals.toMap,
      prepareSeals.toMap,
      blockMeta,
      certificates,
      finalizedHighWater,
      viewChangeEvidence.toMap,
      prePrepareSeals.toMap)
    state = compacted
    commitSeals.clear()
    commitSeals ++= seals
    prepareSeals.clear()
    prepareSeals ++= prepares
    prePrepareSeals.clear()
    prePrepareSeals ++= pps
    viewChangeEvidence.clear()
    viewChangeEvidence ++= vcs
    blockMeta = meta

  private def persistState(): Either[String, Unit] =
    compactInMemory()
    stateStore match
      case None => Right(())
      case Some(path) =>
        val c = BftFinality.encodeReplicaState(
          state,
          commitSeals.toMap,
          blockMeta,
          manifest.replicaSetDigest,
          prepareSeals.toMap,
          viewChangeEvidence.values.toList,
          prePrepareSeals.toMap)
        DurableIo.writeConsensus(path, Canon.encode(c)) match
          case Left(e) =>
            failClosed(s"bft-state write failed: $e")
            Left(s"bft: durable state write failed ($e)")
          case Right(()) => Right(())

  private def tryMintCertificates(): Either[String, Unit] =
    val hwBefore = finalizedHighWater
    val minted = state.slots.foldLeft[Either[String, Unit]](Right(())) { case (acc, ((view, seq), slot)) =>
      acc.flatMap { _ =>
        slot.decided match
          case None => Right(())
          case Some(v) =>
            val blockDig = Digest.parse(new String(v.bytes.toArray, StandardCharsets.US_ASCII))
              .getOrElse(v.digest)
            if certificates.exists(_.blockDigest == blockDig) then Right(())
            else blockMeta.get(blockDig) match
              case None => Right(())
              case Some((h, p)) =>
                requireLocalActiveAt(h).flatMap { active =>
                  val seals = commitSeals.collect {
                    case ((vv, ss, hex, rid), seal)
                        if vv == view && ss == seq && hex == v.digest.hex =>
                      ReplicaId(rid) -> seal
                  }.toList
                  val distinct = seals.map(_._1.id).distinct
                  if distinct.length < quorumSize(active.n) then Right(())
                  else
                    val cert = FinalityCertificate(
                      blockDig, view, seq, seals, active.replicaSetDigest, h, p, chainId)
                    val ok = (node, ledgerAuth.nonEmpty) match
                      case (Some(n), true) =>
                        FinalityCertificate.verifyAgainstHistory(cert, history, n, ledgerAuth)
                      case _ =>
                        FinalityCertificate.verify(cert, active)
                    ok.flatMap { _ =>
                      certificates = cert :: certificates
                      finalizedHighWater = finalizedHighWater max h
                      val persisted =
                        certStore match
                          case None => Right(())
                          case Some(path) =>
                            BftFinality.saveCerts(path, certificates) match
                              case Left(e) =>
                                failClosed(s"bft-certs write failed: $e")
                                Left(s"bft: durable cert write failed ($e)")
                              case Right(()) => Right(())
                      persisted.flatMap { _ =>
                        home match
                          case None => Right(())
                          case Some(h) =>
                            advanceCheckpoint(h, cert) match
                              case Left(e) =>
                                failClosed(s"bft-checkpoint write failed: $e")
                                Left(s"bft: durable checkpoint write failed ($e)")
                              case Right(_) => Right(())
                      }
                    }
                }
      }
    }
    minted.flatMap { _ =>
      if finalizedHighWater > hwBefore then persistState()
      else Right(())
    }

object BftReplica:
  /** Construct only from a seal-verified manifest whose entry matches `keypair`.
    *
    * Pass `home` so the replica loads and replay-verifies `replica-set-history.canon`
    * (required for activation / deactivation of rotated membership).
    */
  def certified(
      keypair: Keypair,
      manifest: ReplicaSetManifest,
      node: Option[Node] = None,
      ledgerAuth: Map[String, Vector[Byte]] = Map.empty,
      certStore: Option[java.nio.file.Path] = None,
      stateStore: Option[java.nio.file.Path] = None,
      home: Option[java.nio.file.Path] = None,
  ): Either[String, BftReplica] =
    for
      _ <- ReplicaSetManifest.verifySeals(manifest, Ed25519.verify)
      expected <- manifest.authorities.get(keypair.name).toRight(
        s"bft: replica '${keypair.name}' not in replica-set manifest")
      _ <- Either.cond(
        expected == keypair.publicBytes, (),
        s"bft: local key for '${keypair.name}' does not match replica-set manifest")
      history <- home match
        case Some(h) =>
          BftFinality.loadReplicaSetHistory(h).flatMap { vh =>
            if vh.manifests.exists(_.digest == manifest.digest) then Right(vh)
            else ValidatedReplicaSetHistory.verify(vh.manifests :+ manifest, Ed25519.verify)
          }
        case None =>
          ValidatedReplicaSetHistory.verify(List(manifest), Ed25519.verify)
      cid <- node match
        case Some(n) => BftFinality.chainId(n)
        case None    => Right(Digest.of(Canon.CStr("")))
    yield new BftReplica(
      keypair, manifest, history, node, ledgerAuth, certStore, stateStore, home, cid)

  private[systemhandler] def historyFileMtime(home: java.nio.file.Path): Long =
    val p = BftFinality.defaultReplicaSetHistoryPath(home)
    if java.nio.file.Files.exists(p) then java.nio.file.Files.getLastModifiedTime(p).toMillis
    else 0L
