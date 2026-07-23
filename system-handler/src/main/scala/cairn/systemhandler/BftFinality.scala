package cairn.systemhandler

import cairn.kernel.*
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets

/** Network BFT finality over sealed PoA block digests.
  *
  * Elevates [[BftQuorum]] from an in-process research sim to a **deployable
  * certificate protocol**: authenticated replicas exchange signed
  * PrePrepare/Prepare/Commit messages over HTTP, then mint a
  * [[FinalityCertificate]] once `2f+1` commits agree.
  *
  * Honesty bounds (still apply):
  * - Replica set is static and taken from [[PeerRegistry]] (`role=replica`).
  * Safety under `f < n/3`; channels are HTTPS-or-localhost + Ed25519 seals.
  * Does not replace PoA sealing — it certifies that a **already-sealed**
  * block digest reached BFT agreement among replicas.
  */
object BftFinality:
  import BftQuorum.*

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

  def sign(kp: Keypair, msg: Msg): SignedMsg =
    val rid = ReplicaId(kp.name)
    val seal = Ed25519.sign(kp.privateKey, Canon.encode(msgCanon(msg)))
    SignedMsg(msg, rid, seal)

  def verify(authorities: Map[String, Vector[Byte]], sm: SignedMsg): Either[String, Unit] =
    authorities.get(sm.signer.id) match
      case None => Left(s"unknown bft replica '${sm.signer.id}'")
      case Some(pk) =>
        if Ed25519.verify(pk, sm.payload, sm.seal) then Right(())
        else Left(s"bad bft seal from ${sm.signer.id}")

  /** Certificate that `blockDigest` reached `2f+1` commits. */
  final case class FinalityCertificate(
      blockDigest: Digest,
      view: Int,
      seq: Int,
      commits: List[(ReplicaId, Vector[Byte])],
  ):
    def canon: Canon = Canon.CTag("bft-finality", Canon.cmap(
      "block" -> Canon.CStr(blockDigest.hex),
      "view" -> Canon.CInt(view),
      "seq" -> Canon.CInt(seq),
      "commits" -> Canon.CList(commits.map { (id, seal) =>
        Canon.cmap("replica" -> Canon.CStr(id.id), "seal" -> Canon.CBytes(seal))
      })))
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
              commits))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not bft-finality: $other")

    /** Check commit count ≥ `2f+1` and each seal matches Commit(view,seq,valueDigest). */
    def verify(
        cert: FinalityCertificate,
        authorities: Map[String, Vector[Byte]],
        n: Int,
    ): Either[String, Unit] =
      val q = quorumSize(n)
      if cert.commits.size < q then
        Left(s"bft finality: ${cert.commits.size} commits < quorum $q")
      else
        val valueDigest = valueOfBlock(cert.blockDigest).digest
        cert.commits.foldLeft[Either[String, Unit]](Right(())) { case (acc, (id, seal)) =>
          acc.flatMap { _ =>
            val commit = Msg.Commit(cert.view, cert.seq, valueDigest, id)
            BftFinality.verify(authorities, SignedMsg(commit, id, seal))
          }
        }

  /** Encode a PoA block digest as the BFT agreement value. */
  def valueOfBlock(blockDigest: Digest): Value =
    Value(blockDigest.hex.getBytes(StandardCharsets.US_ASCII).toVector)

  /** In-process multi-replica agreement with signed messages (unit-testable
    * without HTTP). Returns a certificate when honest replicas decide.
    */
  def agreeLocal(
      replicas: List[Keypair],
      primary: Keypair,
      view: Int,
      seq: Int,
      blockDigest: Digest,
      maxRounds: Int = 16,
  ): Either[String, FinalityCertificate] =
    val ids = replicas.map(k => ReplicaId(k.name))
    val auth = replicas.map(k => k.name -> k.publicBytes).toMap
    val value = valueOfBlock(blockDigest)
    val primaryId = ReplicaId(primary.name)
    var states: Map[ReplicaId, ReplicaState] =
      ids.map(id => id -> ReplicaState(id, ids.length, faulty = false)).toMap
    var inbox: List[SignedMsg] = List(sign(primary, Msg.PrePrepare(view, seq, value, primaryId)))
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
                val signed = sign(kp, m)
                m match
                  case Msg.Commit(_, _, _, _) => commitSeals = commitSeals + (rid -> signed.seal)
                  case _ => ()
                inbox = inbox :+ signed
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
      if commits.size < q then Left(s"bft: only ${commits.size} commit seals, need $q")
      else Right(FinalityCertificate(blockDigest, view, seq, commits))

  private val client = HttpClient.newHttpClient()

  /** Post a signed message to a replica's `/bft/msg` endpoint. */
  def postMsg(baseUrl: String, sm: SignedMsg): Either[String, Unit] =
    try
      val resp = client.send(
        HttpRequest.newBuilder(URI.create(s"$baseUrl/bft/msg"))
          .header("Content-Type", "application/octet-stream")
          .POST(HttpRequest.BodyPublishers.ofByteArray(Canon.encode(sm.canon)))
          .build(),
        HttpResponse.BodyHandlers.ofByteArray())
      if resp.statusCode() == 200 then Right(())
      else Left(s"POST $baseUrl/bft/msg -> ${resp.statusCode()}")
    catch case e: Exception => Left(e.getMessage)

  /** Network agreement: primary broadcasts PrePrepare; replicas respond via HTTP.
    * Each replica process must run [[BftReplica]] (wired into [[HttpNode]]).
    */
  def agreeNetwork(
      primary: Keypair,
      replicaUrls: Map[String, String],
      view: Int,
      seq: Int,
      blockDigest: Digest,
  ): Either[String, Unit] =
    val value = valueOfBlock(blockDigest)
    val pp = sign(primary, Msg.PrePrepare(view, seq, value, ReplicaId(primary.name)))
    replicaUrls.toList.foldLeft[Either[String, Unit]](Right(())) { case (acc, (name, url)) =>
      acc.flatMap(_ => postMsg(url, pp).left.map(e => s"$name: $e"))
    }

/** Per-process BFT replica state machine backed by [[BftQuorum.deliver]]. */
final class BftReplica(
    val keypair: Keypair,
    val authorities: Map[String, Vector[Byte]],
    val n: Int,
):
  import BftQuorum.*
  import BftFinality.*

  private var state: ReplicaState = ReplicaState(ReplicaId(keypair.name), n, faulty = false)
  private val outbound = scala.collection.mutable.ListBuffer.empty[SignedMsg]
  private val commitSeals = scala.collection.mutable.Map.empty[(Int, Int, String), (ReplicaId, Vector[Byte])]
  private var certificates = List.empty[FinalityCertificate]

  def id: ReplicaId = ReplicaId(keypair.name)
  def drainOutbound(): List[SignedMsg] =
    val xs = outbound.toList
    outbound.clear()
    xs
  def finalityCerts: List[FinalityCertificate] = certificates

  def receive(sm: SignedMsg): Either[String, List[SignedMsg]] =
    BftFinality.verify(authorities, sm).map { _ =>
      // Record remote commit seals for certificate assembly
      sm.msg match
        case Msg.Commit(view, seq, d, from) =>
          commitSeals((view, seq, d.hex)) = from -> sm.seal
        case _ => ()
      val (st2, out) = deliver(state, sm.msg)
      state = st2
      val signedOut = out.map { m =>
        val s = BftFinality.sign(keypair, m)
        m match
          case Msg.Commit(view, seq, d, from) =>
            commitSeals((view, seq, d.hex)) = from -> s.seal
          case _ => ()
        s
      }
      outbound ++= signedOut
      // Attempt certificate when this replica has decided
      state.slots.foreach { case ((view, seq), slot) =>
        slot.decided.foreach { v =>
          val blockDig = Digest.parse(new String(v.bytes.toArray, StandardCharsets.US_ASCII))
            .getOrElse(v.digest)
          if certificates.forall(_.blockDigest != blockDig) then
            val seals = commitSeals.collect {
              case ((vv, ss, hex), pair) if vv == view && ss == seq && hex == v.digest.hex => pair
            }.toList
            if seals.size >= quorumSize(n) then
              certificates = FinalityCertificate(blockDig, view, seq, seals) :: certificates
        }
      }
      signedOut
    }
