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

  /** Canonical identity of a replica set: digest of sorted replica names. */
  def replicaSetDigest(replicaIds: List[String]): Digest =
    Digest.of(Canon.cstrs(replicaIds.sorted))

  def designatedPrimary(replicaIds: List[String], view: Int): Either[String, ReplicaId] =
    val sorted = replicaIds.sorted
    if sorted.isEmpty then Left("bft: empty replica set")
    else if sorted.length != sorted.distinct.length then Left("bft: duplicate replica ids")
    else
      val n = sorted.length
      if n > 1 && n < 4 then Left(s"bft: n=$n is not a valid BFT size (need 1 or ≥4)")
      else Right(ReplicaId(sorted(Math.floorMod(view, n))))

  def msgFrom(msg: Msg): ReplicaId = msg match
    case Msg.PrePrepare(_, _, _, from) => from
    case Msg.Prepare(_, _, _, from)    => from
    case Msg.Commit(_, _, _, from)     => from

  final case class SignedMsg(msg: Msg, signer: ReplicaId, seal: Vector[Byte]):
    def payload: Array[Byte] = Canon.encode(msgCanon(msg))
    def canon: Canon = Canon.CTag("bft-signed", Canon.cmap(
      "msg" -> msgCanon(msg),
      "signer" -> Canon.CStr(signer.id),
      "seal" -> Canon.CBytes(seal)))

  object SignedMsg:
    def fromCanon(c: Canon): Either[String, SignedMsg] =
      import Canon.*
      c match
        case CTag("bft-signed", m) =>
          parseMsg(m.field("msg")).flatMap { msg =>
            m.field("seal") match
              case CBytes(bs) =>
                Right(SignedMsg(msg, ReplicaId(m.field("signer").asStr), bs))
              case other => Left(s"bad seal: $other")
          }
        case other => Left(s"not a bft-signed message: $other")

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
      case other => Left(s"unknown bft msg: $other")
    catch case e: CodecError => Left(e.getMessage)

  def sign(kp: Keypair, msg: Msg): Either[String, SignedMsg] =
    val rid = ReplicaId(kp.name)
    if rid != msgFrom(msg) then Left(s"bft: cannot sign msg.from=${msgFrom(msg).id} as ${rid.id}")
    else
      val seal = Ed25519.sign(kp.privateKey, Canon.encode(msgCanon(msg)))
      Right(SignedMsg(msg, rid, seal))

  /** Verify signature AND `signer == msg.from`, and that signer is known. */
  def verify(authorities: Map[String, Vector[Byte]], sm: SignedMsg): Either[String, Unit] =
    if sm.signer != msgFrom(sm.msg) then
      Left(s"bft: signer '${sm.signer.id}' != msg.from '${msgFrom(sm.msg).id}'")
    else authorities.get(sm.signer.id) match
      case None => Left(s"unknown bft replica '${sm.signer.id}'")
      case Some(pk) =>
        if Ed25519.verify(pk, sm.payload, sm.seal) then Right(())
        else Left(s"bad bft seal from ${sm.signer.id}")

  /** Certificate that a sealed PoA block reached 2f+1 **distinct** commits. */
  final case class FinalityCertificate(
      blockDigest: Digest,
      view: Int,
      seq: Int,
      commits: List[(ReplicaId, Vector[Byte])],
      replicaSet: Digest,
      height: Long,
      parent: Digest,
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
      "parent" -> Canon.CStr(parent.hex)))
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
            Right(FinalityCertificate(
              Digest(m.field("block").asStr),
              m.field("view").asInt.toInt,
              m.field("seq").asInt.toInt,
              commits,
              Digest(m.field("replicaSet").asStr),
              m.field("height").asInt,
              Digest(m.field("parent").asStr)))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not bft-finality: $other")

    /** Quorum certificate check: distinct known replicas, matching replica-set
      * digest, valid Commit seals for the encoded block value.
      */
    def verify(
        cert: FinalityCertificate,
        authorities: Map[String, Vector[Byte]],
        expectedReplicaSet: Digest,
    ): Either[String, Unit] =
      val n = authorities.size
      val q = quorumSize(n)
      val ids = cert.commits.map(_._1.id)
      if cert.replicaSet != expectedReplicaSet then
        Left(s"bft finality: replicaSet ${cert.replicaSet.short} != expected ${expectedReplicaSet.short}")
      else if replicaSetDigest(authorities.keys.toList) != expectedReplicaSet then
        Left("bft finality: authorities do not match certificate replicaSet")
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
            BftFinality.verify(authorities, SignedMsg(commit, id, seal))
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

  /** In-process agreement bound to a replay-valid sealed block on `node`. */
  def agreeForSealedBlock(
      node: Node,
      ledgerAuth: Map[String, Vector[Byte]],
      replicas: List[Keypair],
      view: Int,
      seq: Int,
      blockDigest: Digest,
  ): Either[String, FinalityCertificate] =
    requireSealedBlock(node, ledgerAuth, blockDigest).flatMap { (block, height) =>
      agreeLocalProven(replicas, view, seq, blockDigest, height, block.parent)
    }

  /** Agree when the caller already proved the block (height/parent known). */
  def agreeLocalProven(
      replicas: List[Keypair],
      view: Int,
      seq: Int,
      blockDigest: Digest,
      height: Long,
      parent: Digest,
      maxRounds: Int = 16,
  ): Either[String, FinalityCertificate] =
    val ids = replicas.map(_.name)
    for
      primaryId <- designatedPrimary(ids, view)
      primary <- replicas.find(_.name == primaryId.id).toRight(s"bft: missing primary ${primaryId.id}")
      cert <- runAgreement(replicas, primary, view, seq, blockDigest, height, parent, maxRounds)
    yield cert

  private def runAgreement(
      replicas: List[Keypair],
      primary: Keypair,
      view: Int,
      seq: Int,
      blockDigest: Digest,
      height: Long,
      parent: Digest,
      maxRounds: Int,
  ): Either[String, FinalityCertificate] =
    val ids = replicas.map(k => ReplicaId(k.name))
    val auth = replicas.map(k => k.name -> k.publicBytes).toMap
    val setDig = replicaSetDigest(replicas.map(_.name))
    val value = valueOfBlock(blockDigest)
    val primaryId = ReplicaId(primary.name)
    var states: Map[ReplicaId, ReplicaState] =
      ids.map(id => id -> ReplicaState(id, ids.length, faulty = false)).toMap
    sign(primary, Msg.PrePrepare(view, seq, value, primaryId)).flatMap { pp =>
      var inbox: List[SignedMsg] = List(pp)
      var round = 0
      var commitSeals: Map[ReplicaId, Vector[Byte]] = Map.empty
      while inbox.nonEmpty && round < maxRounds do
        val batch = inbox
        inbox = Nil
        batch.foreach { sm =>
          verify(auth, sm) match
            case Left(_) => ()
            case Right(()) =>
              ids.foreach { rid =>
                val (st2, out) = deliver(states(rid), sm.msg)
                states = states + (rid -> st2)
                out.foreach { m =>
                  val kp = replicas.find(_.name == rid.id).get
                  sign(kp, m).foreach { signed =>
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
          val cert = FinalityCertificate(blockDigest, view, seq, commits, setDig, height, parent)
          FinalityCertificate.verify(cert, auth, setDig).map(_ => cert)
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
    try
      java.nio.file.Files.createDirectories(path.getParent)
      java.nio.file.Files.write(path, Canon.encode(Canon.CList(certs.map(_.canon))))
      Right(())
    catch case e: Exception => Left(e.getMessage)

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

  /** Broadcast PrePrepare and poll replicas until one mints a certificate. */
  def agreeNetwork(
      primary: Keypair,
      replicaUrls: Map[String, String],
      view: Int,
      seq: Int,
      blockDigest: Digest,
      polls: Int = 32,
      pollSleepMs: Long = 25,
  ): Either[String, FinalityCertificate] =
    val ids = replicaUrls.keys.toList
    for
      primaryId <- designatedPrimary(ids, view)
      _ <- Either.cond(primaryId.id == primary.name, (),
        s"bft: primary for view $view is ${primaryId.id}, not ${primary.name}")
      value = valueOfBlock(blockDigest)
      pp <- sign(primary, Msg.PrePrepare(view, seq, value, primaryId))
      _ <- replicaUrls.toList.foldLeft[Either[String, Unit]](Right(())) { case (acc, (name, url)) =>
        acc.flatMap(_ => postMsg(url, pp).left.map(e => s"$name: $e"))
      }
      cert <- {
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
                // Re-fan-out: ask each replica to process empty by re-posting is
                // unnecessary — /bft/msg already fans out follow-ons. Just poll.
                loop(n - 1)
        loop(polls)
      }
    yield cert

/** Per-process BFT replica state machine backed by [[BftQuorum.deliver]]. */
final class BftReplica(
    val keypair: Keypair,
    val authorities: Map[String, Vector[Byte]],
    val replicaIds: List[String],
    val node: Option[Node] = None,
    val ledgerAuth: Map[String, Vector[Byte]] = Map.empty,
    certStore: Option[java.nio.file.Path] = None,
):
  import BftQuorum.*
  import BftFinality.*

  private val n = replicaIds.length
  private var state: ReplicaState = ReplicaState(ReplicaId(keypair.name), n, faulty = false)
  private val outbound = scala.collection.mutable.ListBuffer.empty[SignedMsg]
  /** Keyed by (view, seq, valueDigestHex, replicaId) — never overwrite peers. */
  private val commitSeals =
    scala.collection.mutable.Map.empty[(Int, Int, String, String), Vector[Byte]]
  private var certificates: List[FinalityCertificate] =
    certStore.flatMap(BftFinality.loadCerts(_).toOption).getOrElse(Nil)
  private var blockMeta: Map[Digest, (Long, Digest)] = Map.empty

  def id: ReplicaId = ReplicaId(keypair.name)
  def setDigest: Digest = replicaSetDigest(replicaIds)
  def drainOutbound(): List[SignedMsg] =
    val xs = outbound.toList
    outbound.clear()
    xs
  def finalityCerts: List[FinalityCertificate] = certificates

  /** Bind ledger evidence for a block before / while agreeing. */
  def noteSealedBlock(blockDigest: Digest, height: Long, parent: Digest): Unit =
    blockMeta = blockMeta + (blockDigest -> (height, parent))

  def receive(sm: SignedMsg): Either[String, List[SignedMsg]] =
    val primaryOk = sm.msg match
      case Msg.PrePrepare(view, _, _, from) =>
        designatedPrimary(replicaIds, view).exists(_ == from)
      case _ => true
    if !primaryOk then Left(s"bft: PrePrepare from non-primary ${msgFrom(sm.msg).id}")
    else BftFinality.verify(authorities, sm).flatMap { _ =>
      val bind: Either[String, Unit] = sm.msg match
        case Msg.PrePrepare(_, _, value, _) =>
          Digest.parse(new String(value.bytes.toArray, StandardCharsets.US_ASCII)) match
            case Left(e) => Left(e)
            case Right(blockDig) =>
              (node, ledgerAuth.nonEmpty) match
                case (Some(n), true) =>
                  requireSealedBlock(n, ledgerAuth, blockDig).map { (b, h) =>
                    noteSealedBlock(blockDig, h, b.parent)
                  }
                case _ =>
                  if blockMeta.contains(blockDig) then Right(())
                  else Left(s"bft: block ${blockDig.short} not noted as sealed")
        case _ => Right(())
      bind.map { _ =>
        sm.msg match
          case Msg.Commit(view, seq, d, from) =>
            commitSeals((view, seq, d.hex, from.id)) = sm.seal
          case _ => ()
        val (st2, out) = deliver(state, sm.msg)
        state = st2
        val signedOut = out.flatMap { m =>
          BftFinality.sign(keypair, m).toOption.toList.map { s =>
            m match
              case Msg.Commit(view, seq, d, from) =>
                commitSeals((view, seq, d.hex, from.id)) = s.seal
              case _ => ()
            s
          }
        }
        outbound ++= signedOut
        tryMintCertificates()
        signedOut
      }
    }

  private def tryMintCertificates(): Unit =
    state.slots.foreach { case ((view, seq), slot) =>
      slot.decided.foreach { v =>
        val blockDig = Digest.parse(new String(v.bytes.toArray, StandardCharsets.US_ASCII))
          .getOrElse(v.digest)
        if certificates.forall(_.blockDigest != blockDig) then
          blockMeta.get(blockDig) match
            case None => () // refuse certificate without ledger binding
            case Some((h, p)) =>
              val seals = commitSeals.collect {
                case ((vv, ss, hex, rid), seal) if vv == view && ss == seq && hex == v.digest.hex =>
                  ReplicaId(rid) -> seal
              }.toList
              val distinct = seals.map(_._1.id).distinct
              if distinct.length >= quorumSize(n) then
                val cert = FinalityCertificate(
                  blockDig, view, seq, seals, setDigest, h, p)
                if FinalityCertificate.verify(cert, authorities, setDigest).isRight then
                  certificates = cert :: certificates
                  certStore.foreach(BftFinality.saveCerts(_, certificates))
      }
    }
