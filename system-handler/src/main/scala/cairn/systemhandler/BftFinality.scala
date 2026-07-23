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

  def sign(kp: Signer, msg: Msg): Either[String, SignedMsg] =
    val rid = ReplicaId(kp.name)
    if rid != msgFrom(msg) then Left(s"bft: cannot sign msg.from=${msgFrom(msg).id} as ${rid.id}")
    else
      val seal = kp.sign(Canon.encode(msgCanon(msg)))
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
              BftFinality.verify(authorities, SignedMsg(commit, id, seal))
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
        requireSealedBlock(node, ledgerAuth, cert.blockDigest).flatMap { (block, height) =>
          if cert.height != height then
            Left(s"bft finality: height ${cert.height} != chain height $height")
          else if cert.parent != block.parent then
            Left(
              s"bft finality: parent ${cert.parent.short} != block parent ${block.parent.short}")
          else Right(())
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
    sealReplicaSet(replicas).flatMap { manifest =>
      val auth = manifest.authorities
      val setDig = manifest.replicaSetDigest
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

  def loadReplicaSet(path: java.nio.file.Path): Either[String, ReplicaSetManifest] =
    if !java.nio.file.Files.exists(path) then Left(s"missing replica-set manifest at $path")
    else
      Canon.decode(java.nio.file.Files.readAllBytes(path)).flatMap(ReplicaSetManifest.fromCanon)
        .flatMap { m =>
          ReplicaSetManifest.verifySeals(m, Ed25519.verify).map(_ => m)
        }

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
      _ <- ReplicaSetManifest.allowsTransition(
        m, live, live.map(_.digest), Ed25519.verify)
      newHistory = history.filterNot(_.digest == m.digest) :+ m
      _ <- ValidatedReplicaSetHistory.verify(newHistory, Ed25519.verify)
      _ <- DurableIo.writeConsensus(
        defaultReplicaSetHistoryPath(home),
        Canon.encode(Canon.CList(newHistory.map(_.canon))))
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

  /** Encode durable protocol state (slots + commit seals + block meta). */
  def encodeReplicaState(
      state: BftQuorum.ReplicaState,
      commitSeals: Map[(Int, Int, String, String), Vector[Byte]],
      blockMeta: Map[Digest, (Long, Digest)],
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
    val meta = blockMeta.toList.map { case (d, hp) =>
      val (h, p) = hp
      Canon.cmap(
        "block" -> Canon.CStr(d.hex), "height" -> Canon.CInt(h), "parent" -> Canon.CStr(p.hex))
    }
    Canon.CTag("bft-replica-state", Canon.cmap(
      "id" -> Canon.CStr(state.id.id),
      "n" -> Canon.CInt(state.n),
      "slots" -> Canon.CList(slots),
      "commitSeals" -> Canon.CList(seals),
      "blockMeta" -> Canon.CList(meta)))

  def decodeReplicaState(c: Canon): Either[String, (BftQuorum.ReplicaState, Map[(Int, Int, String, String), Vector[Byte]], Map[Digest, (Long, Digest)])] =
    import Canon.*, BftQuorum.*
    c match
      case CTag("bft-replica-state", m) =>
        try
          val id = ReplicaId(m.field("id").asStr)
          val n = m.field("n").asInt.toInt
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
          val meta = m.field("blockMeta").asList.map { row =>
            Digest(row.field("block").asStr) ->
              (row.field("height").asInt, Digest(row.field("parent").asStr))
          }.toMap
          Right((ReplicaState(id, n, faulty = false, slots), seals, meta))
        catch case e: CodecError => Left(e.getMessage)
      case other => Left(s"not bft-replica-state: $other")

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

  /** Ask the designated primary to propose (initiator needs no primary private key).
    * Sequence is derived from block height on the primary — callers do not pick slots.
    */
  def propose(
      primaryUrl: String,
      blockDigest: Digest,
      view: Int = 0,
  ): Either[String, Unit] =
    try
      val body = Canon.encode(Canon.cmap(
        "view" -> Canon.CInt(view),
        "block" -> Canon.CStr(blockDigest.hex)))
      val resp = client.send(
        HttpRequest.newBuilder(URI.create(s"$primaryUrl/bft/propose"))
          .header("Content-Type", "application/octet-stream")
          .POST(HttpRequest.BodyPublishers.ofByteArray(body))
          .build(),
        HttpResponse.BodyHandlers.ofByteArray())
      if resp.statusCode() == 200 then Right(())
      else Left(s"POST $primaryUrl/bft/propose -> ${resp.statusCode()}: ${new String(resp.body())}")
    catch case e: Exception => Left(e.getMessage)

  /** @deprecated Prefer [[propose(String, Digest, Int)]] — seq is height-bound. */
  def propose(
      primaryUrl: String,
      view: Int,
      seq: Int,
      blockDigest: Digest,
  ): Either[String, Unit] =
    // Still accepted for labs; primary ignores seq and re-derives from height.
    propose(primaryUrl, blockDigest, view)

  /** Broadcast PrePrepare from a local primary keypair, then poll for a cert.
    * `seq` must equal the sealed block's ledger height.
    */
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
      cert <- pollCert(replicaUrls, blockDigest, polls, pollSleepMs)
    yield cert

  /** Deployable path: ask the primary to propose (seq = height); poll for the cert. */
  def agreeNetworkRemote(
      replicaUrls: Map[String, String],
      blockDigest: Digest,
      view: Int = 0,
      polls: Int = 64,
      pollSleepMs: Long = 30,
  ): Either[String, FinalityCertificate] =
    val ids = replicaUrls.keys.toList
    for
      primaryId <- designatedPrimary(ids, view)
      primaryUrl <- replicaUrls.get(primaryId.id).toRight(s"bft: no URL for primary ${primaryId.id}")
      _ <- propose(primaryUrl, blockDigest, view)
      cert <- pollCert(replicaUrls, blockDigest, polls, pollSleepMs)
    yield cert

  /** @deprecated Prefer [[agreeNetworkRemote(Map, Digest, Int, Int, Long)]]. */
  def agreeNetworkRemote(
      replicaUrls: Map[String, String],
      view: Int,
      seq: Int,
      blockDigest: Digest,
      polls: Int,
      pollSleepMs: Long,
  ): Either[String, FinalityCertificate] =
    agreeNetworkRemote(replicaUrls, blockDigest, view, polls, pollSleepMs)

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
    val manifest: ReplicaSetManifest,
    val history: ValidatedReplicaSetHistory,
    val node: Option[Node],
    val ledgerAuth: Map[String, Vector[Byte]],
    certStore: Option[java.nio.file.Path],
    stateStore: Option[java.nio.file.Path],
):
  import BftQuorum.*
  import BftFinality.*

  val authorities: Map[String, Vector[Byte]] = manifest.authorities
  val replicaIds: List[String] = manifest.ids
  private val n = replicaIds.length
  private var state: ReplicaState = ReplicaState(ReplicaId(keypair.name), n, faulty = false)
  private val outbound = scala.collection.mutable.ListBuffer.empty[SignedMsg]
  /** Keyed by (view, seq, valueDigestHex, replicaId) — never overwrite peers. */
  private val commitSeals =
    scala.collection.mutable.Map.empty[(Int, Int, String, String), Vector[Byte]]
  private var certificates: List[FinalityCertificate] = Nil
  private var blockMeta: Map[Digest, (Long, Digest)] = Map.empty
  /** Highest ledger height for which this replica has minted a finality cert. */
  private var finalizedHighWater: Long = -1L
  /** Non-empty when restore failed or a later durable write failed. */
  private var ioError: Option[String] = None

  certStore.foreach { path =>
    if java.nio.file.Files.exists(path) then
      BftFinality.loadCerts(path) match
        case Right(cs) =>
          certificates = cs
          finalizedHighWater = cs.map(_.height).foldLeft(-1L)(_ max _)
        case Left(e) => ioError = Some(s"bft-certs restore failed: $e")
  }

  stateStore.foreach { path =>
    if java.nio.file.Files.exists(path) && ioError.isEmpty then
      Canon.decode(java.nio.file.Files.readAllBytes(path)).flatMap(BftFinality.decodeReplicaState) match
        case Right((st, seals, meta)) if st.id.id == keypair.name && st.n == n =>
          state = st
          commitSeals ++= seals
          blockMeta = meta
          // High-water is cert-mint only — noted blocks must not lock slots.
        case Right((st, _, _)) =>
          ioError = Some(
            s"bft-state identity mismatch: file has ${st.id.id}/n=${st.n}, " +
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

  /** Local set must be the active membership at `height` (checked every transition). */
  private def requireLocalActiveAt(height: Long): Either[String, ReplicaSetManifest] =
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
        primary <- designatedPrimary(replicaIds, view)
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
        pp <- sign(keypair, Msg.PrePrepare(view, seq, valueOfBlock(blockDigest), primary))
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

  def receive(sm: SignedMsg): Either[String, List[SignedMsg]] =
    refuseIfCorrupt {
      val primaryOk = sm.msg match
        case Msg.PrePrepare(view, _, _, from) =>
          designatedPrimary(replicaIds, view).exists(_ == from)
        case _ => true
      if !primaryOk then Left(s"bft: PrePrepare from non-primary ${msgFrom(sm.msg).id}")
      else BftFinality.verify(authorities, sm).flatMap { _ =>
        val bind: Either[String, Long] = sm.msg match
          case Msg.PrePrepare(_, seq, value, _) =>
            Digest.parse(new String(value.bytes.toArray, StandardCharsets.US_ASCII)) match
              case Left(e) => Left(e)
              case Right(blockDig) =>
                (node, ledgerAuth.nonEmpty) match
                  case (Some(n), true) =>
                    requireSealedBlock(n, ledgerAuth, blockDig).flatMap { (b, h) =>
                      seqForHeight(h).flatMap { expected =>
                        if seq != expected then
                          Left(s"bft: PrePrepare seq $seq != block height $h")
                        else
                          requireLocalActiveAt(h).flatMap { _ =>
                            noteSealedBlock(blockDig, h, b.parent).map(_ => h)
                          }
                      }
                    }
                  case _ =>
                    blockMeta.get(blockDig) match
                      case Some((h, _)) =>
                        if seq.toLong != h then
                          Left(s"bft: PrePrepare seq $seq != noted height $h")
                        else requireLocalActiveAt(h).map(_ => h)
                      case None =>
                        Left(s"bft: block ${blockDig.short} not noted as sealed")
          case Msg.Prepare(_, seq, _, _) =>
            // Prepares may race ahead of PrePrepare on the wire; seq == height.
            requireLocalActiveAt(seq.toLong).map(_ => seq.toLong)
          case Msg.Commit(_, seq, _, _) =>
            requireLocalActiveAt(seq.toLong).map(_ => seq.toLong)
        bind.flatMap { _ =>
          val priorState = state
          val priorSeals = commitSeals.toMap
          sm.msg match
            case Msg.Commit(view, seq, d, from) =>
              commitSeals((view, seq, d.hex, from.id)) = sm.seal
            case _ => ()
          val (st2, out) = deliver(state, sm.msg)
          state = st2
          // Do not sign outbound until we confirm we are still active for each msg height.
          val signedOutE: Either[String, List[SignedMsg]] =
            out.foldLeft[Either[String, List[SignedMsg]]](Right(Nil)) { (acc, m) =>
              acc.flatMap { xs =>
                val hE = m match
                  case Msg.Prepare(_, seq, d, _) =>
                    heightForValueDigest(d).orElse(Some(seq.toLong))
                      .toRight("bft: cannot sign Prepare for unknown value")
                  case Msg.Commit(_, seq, d, _) =>
                    heightForValueDigest(d).orElse(Some(seq.toLong))
                      .toRight("bft: cannot sign Commit for unknown value")
                  case Msg.PrePrepare(_, _, value, _) =>
                    Digest.parse(new String(value.bytes.toArray, StandardCharsets.US_ASCII))
                      .flatMap(d => blockMeta.get(d).map(_._1).toRight("bft: unknown PrePrepare block"))
                hE.flatMap { h =>
                  requireLocalActiveAt(h).flatMap { _ =>
                    BftFinality.sign(keypair, m).map { s =>
                      m match
                        case Msg.Commit(view, seq, d, from) =>
                          commitSeals((view, seq, d.hex, from.id)) = s.seal
                        case _ => ()
                      xs :+ s
                    }
                  }
                }
              }
            }
          signedOutE.flatMap { signedOut =>
            persistState() match
              case Left(e) =>
                state = priorState
                commitSeals.clear()
                commitSeals ++= priorSeals
                Left(e)
              case Right(()) =>
                tryMintCertificates() match
                  case Left(e) => Left(e)
                  case Right(()) =>
                    outbound ++= signedOut
                    Right(signedOut)
          }
        }
      }
    }

  private def persistState(): Either[String, Unit] =
    stateStore match
      case None => Right(())
      case Some(path) =>
        val c = BftFinality.encodeReplicaState(state, commitSeals.toMap, blockMeta)
        DurableIo.writeConsensus(path, Canon.encode(c)) match
          case Left(e) =>
            failClosed(s"bft-state write failed: $e")
            Left(s"bft: durable state write failed ($e)")
          case Right(()) => Right(())

  private def tryMintCertificates(): Either[String, Unit] =
    state.slots.foldLeft[Either[String, Unit]](Right(())) { case (acc, ((view, seq), slot)) =>
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
                      blockDig, view, seq, seals, active.replicaSetDigest, h, p)
                    val ok = (node, ledgerAuth.nonEmpty) match
                      case (Some(n), true) =>
                        FinalityCertificate.verifyAgainstHistory(cert, history, n, ledgerAuth)
                      case _ =>
                        FinalityCertificate.verify(cert, active)
                    ok.flatMap { _ =>
                      certificates = cert :: certificates
                      finalizedHighWater = finalizedHighWater max h
                      certStore match
                        case None => Right(())
                        case Some(path) =>
                          BftFinality.saveCerts(path, certificates) match
                            case Left(e) =>
                              failClosed(s"bft-certs write failed: $e")
                              Left(s"bft: durable cert write failed ($e)")
                            case Right(()) => Right(())
                    }
                }
      }
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
    yield new BftReplica(keypair, manifest, history, node, ledgerAuth, certStore, stateStore)
