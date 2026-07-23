package cairn.systemhandler

import cairn.kernel.*
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

/** HTTP node surface + want/have pull sync (Phase 3 http/network families).
  * Moved from `ledger.Distribution`. Chain-file writes go through
  * [[Filesystem]] on the consumer node (same as [[Sync.pull]]).
  *
  * Optional [[peersRoot]] enables `GET/POST /peers` discovery.
  * Optional [[bft]] enables `POST /bft/msg` for [[BftFinality]] replicas.
  */
final class HttpNode(
    node: Node,
    authorities: Map[String, Vector[Byte]],
    peersRoot: Option[java.nio.file.Path] = None,
    bft: Option[BftReplica] = None,
):
  private var server: HttpServer | Null = null
  private var executor: java.util.concurrent.ExecutorService | Null = null

  def start(port: Int = 0): Int =
    val s = HttpServer.create(InetSocketAddress(port), 0)
    val pool = java.util.concurrent.Executors.newCachedThreadPool()
    s.setExecutor(pool)
    executor = pool
    def reply(ex: HttpExchange, code: Int, body: Array[Byte]): Unit =
      ex.sendResponseHeaders(code, body.length)
      ex.getResponseBody.write(body)
      ex.close()
    def readBody(ex: HttpExchange): Array[Byte] = ex.getRequestBody.readAllBytes()
    s.createContext("/chain", ex =>
      reply(ex, 200, node.chainDigests.map(_.hex).mkString("\n").getBytes))
    s.createContext("/blob/", ex =>
      val hex = ex.getRequestURI.getPath.stripPrefix("/blob/")
      Digest.parse(hex).flatMap(d => CasEffects.getBytes(node.cas, d, node.ctx).left.map(_.toString)) match
        case Right(bs) => reply(ex, 200, bs)
        case Left(e)   => reply(ex, 404, e.getBytes))
    s.createContext("/heads", ex =>
      node.state(authorities) match
        case Right(st) => reply(ex, 200,
          st.heads.toList.sortBy(_._1).map((b, k) => s"$b ${k.render}").mkString("\n").getBytes)
        case Left(e) => reply(ex, 500, e.getBytes))
    peersRoot.foreach { root =>
      s.createContext("/peers", ex =>
        ex.getRequestMethod match
          case "GET" =>
            PeerRegistry.load(root) match
              case Right(dir) => reply(ex, 200, Canon.encode(dir.canon))
              case Left(e)    => reply(ex, 500, e.getBytes)
          case "POST" =>
            Canon.decode(readBody(ex)).flatMap(PeerRegistry.Peer.fromCanon) match
              case Left(e) => reply(ex, 400, e.getBytes)
              case Right(peer) =>
                PeerRegistry.load(root).flatMap { d =>
                  val next = d.upsert(peer)
                  PeerRegistry.save(root, next).map(_ => next)
                } match
                  case Right(dir) => reply(ex, 200, Canon.encode(dir.canon))
                  case Left(e)    => reply(ex, 500, e.getBytes)
          case other => reply(ex, 405, s"method $other".getBytes))
    }
    bft.foreach { replica =>
      def replicaPeers(root: java.nio.file.Path): List[PeerRegistry.Peer] =
        PeerRegistry.load(root).toOption.toList.flatMap(_.replicas).filter { peer =>
          replica.authorities.get(peer.name).contains(peer.publicKey.getOrElse(Vector.empty)) &&
            PeerRegistry.Peer.verifyBinding(peer).isRight
        }
      s.createContext("/bft/msg", ex =>
        if ex.getRequestMethod != "POST" then reply(ex, 405, "POST only".getBytes)
        else
          Canon.decode(readBody(ex)).flatMap(BftFinality.SignedMsg.fromCanon) match
            case Left(e) => reply(ex, 400, e.getBytes)
            case Right(sm) =>
              replica.receive(sm) match
                case Left(e) => reply(ex, 400, e.getBytes)
                case Right(out) =>
                  reply(ex, 200, Canon.encode(Canon.CList(out.map(_.canon))))
                  peersRoot.foreach { root =>
                    replicaPeers(root).filterNot(_.name == replica.keypair.name).foreach { p =>
                      out.foreach(m => BftFinality.postMsg(p.baseUrl, m))
                    }
                  }
      )
      s.createContext("/bft/propose", ex =>
        if ex.getRequestMethod != "POST" then reply(ex, 405, "POST only".getBytes)
        else
          Canon.decode(readBody(ex)).flatMap(BftFinality.ProposeRequest.fromCanon) match
            case Left(e) => reply(ex, 400, e.getBytes)
            case Right(req) =>
              BftFinality.verifyProposeRequest(
                replica.authorities, req, Some(replica.chainId), Some(replica.setDigest)).flatMap { _ =>
                // Sequence is derived from sealed block height on the primary.
                replica.proposeBlock(req.view, req.block)
              } match
                case Left(e) => reply(ex, 400, e.getBytes)
                case Right(out) =>
                  reply(ex, 200, Canon.encode(Canon.CList(out.map(_.canon))))
                  peersRoot.foreach { root =>
                    replicaPeers(root).filterNot(_.name == replica.keypair.name).foreach { p =>
                      out.foreach(m => BftFinality.postMsg(p.baseUrl, m))
                    }
                  }
      )
      s.createContext("/bft/certs", ex =>
        reply(ex, 200, Canon.encode(Canon.CList(replica.finalityCerts.map(_.canon)))))
      s.createContext("/bft/view-change", ex =>
        if ex.getRequestMethod != "POST" then reply(ex, 405, "POST only".getBytes)
        else
          Canon.decode(readBody(ex)).flatMap(BftFinality.ViewChangeRequest.fromCanon) match
            case Left(e) => reply(ex, 400, e.getBytes)
            case Right(req) =>
              BftFinality.verifyViewChangeRequest(
                replica.authorities, req, Some(replica.chainId), Some(replica.setDigest)).flatMap { _ =>
                replica.requestViewChange(req.newView)
              } match
                case Left(e) => reply(ex, 400, e.getBytes)
                case Right(out) =>
                  reply(ex, 200, Canon.encode(Canon.CList(out.map(_.canon))))
                  peersRoot.foreach { root =>
                    replicaPeers(root).filterNot(_.name == replica.keypair.name).foreach { p =>
                      out.foreach(m => BftFinality.postMsg(p.baseUrl, m))
                    }
                  }
      )
    }
    s.start()
    server = s
    s.getAddress.getPort

  def stop(): Unit =
    Option(server).foreach(_.stop(0))
    Option(executor).foreach(_.shutdownNow())
    server = null
    executor = null

object HttpSync:
  private val client = HttpClient.newHttpClient()

  private def get(base: String, path: String): Either[String, Array[Byte]] =
    val resp = client.send(
      HttpRequest.newBuilder(URI.create(s"$base$path")).GET().build(),
      HttpResponse.BodyHandlers.ofByteArray())
    if resp.statusCode() == 200 then Right(resp.body()) else Left(s"GET $path -> ${resp.statusCode()}")

  final case class PullReport(fetchedBlocks: Int, fetchedBlobs: Int, alreadyHad: Int)

  private def casErr(e: cairn.systeminterface.Cas.Error): String = e match
    case cairn.systeminterface.Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
    case cairn.systeminterface.Cas.Error.Io(m)      => m

  /** Fetch and materialize ONE artifact by digest alone — first-class
    * share-by-hash. Unlike [[pull]] (which always walks a whole ledger
    * chain), this needs no chain, branch, or publish state at all: any
    * digest the remote's `/blob/` endpoint can serve is fetchable directly,
    * the same way a Unison user shares a single definition by hash. A no-op
    * if already local; never trusts the remote — verifies the served bytes
    * actually hash to `d` before accepting them.
    */
  def fetchByHash(baseUrl: String, to: Node, d: Digest): Either[String, Digest] =
    CasEffects.contains(to.cas, d, to.ctx).left.map(casErr).flatMap {
      case true => Right(d)
      case false =>
        get(baseUrl, s"/blob/${d.hex}").flatMap { bs =>
          CasEffects.putBytes(to.cas, bs, to.ctx).left.map(casErr).flatMap { actual =>
            if actual == d then Right(actual)
            else Left(s"remote served wrong bytes for ${d.short}")
          }
        }
    }

  /** Fetch `d` when absent, as part of a [[pull]] walk. Authorized `contains`
    * failures abort — never treat denial as "missing" via `Either.forall`.
    */
  private def fetchIfMissing(
      baseUrl: String, to: Node, d: Digest, fetched: Int
  ): Either[String, Int] =
    CasEffects.contains(to.cas, d, to.ctx).left.map(casErr).flatMap {
      case true  => Right(fetched)
      case false => fetchByHash(baseUrl, to, d).map(_ => fetched + 1)
    }

  def pull(
      baseUrl: String,
      to: Node,
      authorities: Map[String, Vector[Byte]],
      checkpoint: Option[BftFinality.FinalizedCheckpoint] = None,
      checkpointHome: Option[java.nio.file.Path] = None,
  ): Either[String, PullReport] =
    for
      chainTxt <- get(baseUrl, "/chain")
      remoteChain = new String(chainTxt).linesIterator.filter(_.nonEmpty).map(Digest(_)).toList
      _ <- BftFinality.requireExtendsCheckpoint(remoteChain, checkpoint)
      fetched <- remoteChain.foldLeft[Either[String, Int]](Right(0)) { (acc, d) =>
        acc.flatMap(n => fetchIfMissing(baseUrl, to, d, n))
      }
      blocks <- remoteChain.foldLeft[Either[String, List[Block]]](Right(Nil)) { (acc, d) =>
        acc.flatMap(bs =>
          CasEffects.get(to.cas, d, to.ctx).left.map(casErr).map(a => bs :+ Block.fromCanon(a.body))) }
      st <- LedgerKernel.replay(authorities, blocks, Ed25519.verify)
      publishedDigests = st.published.toList.flatMap(_.split(":") match
        case Array(_, value, _) => Digest.parse(value).toOption
        case _                  => None)
      fetchedBlobs <- publishedDigests.foldLeft[Either[String, Int]](Right(0)) { (acc, d) =>
        acc.flatMap(n => fetchIfMissing(baseUrl, to, d, n))
      }
      // Adopt remote finality certs only after proving them against the candidate
      // chain; never consult `to.chainDigests` before this chain is committed.
      verifiedCerts <- checkpointHome match
        case None => Right(Nil)
        case Some(home) =>
          BftFinality.fetchCerts(baseUrl) match
            // `/bft/certs` is optional for ordinary non-BFT HTTP peers.
            case Left(e) if e.contains("/bft/certs ->") => Right(Nil)
            case Left(e) => Left(s"bft cert fetch failed: $e")
            case Right(remoteCerts) =>
              BftFinality.loadReplicaSetHistory(home).flatMap { hist =>
                remoteCerts.sortBy(_.height).foldLeft[Either[String, List[BftFinality.FinalityCertificate]]](
                  Right(Nil)) { (acc, cert) =>
                  acc.flatMap { ok =>
                    if !blocks.exists(_.digest == cert.blockDigest) then Right(ok)
                    else
                      BftFinality.FinalityCertificate.verifyAgainstBlocks(
                        cert, hist, blocks, authorities).map(_ => ok :+ cert)
                  }
                }
              }
      _ <- (checkpointHome, verifiedCerts) match
        case (Some(home), cs) if cs.nonEmpty || remoteChain != to.chainDigests =>
          BftFinality.adoptFollowerGeneration(home, to, remoteChain, cs)
        case (Some(home), _) =>
          // No new certs and same chain tip: still require checkpoint extension.
          BftFinality.loadCheckpoint(home).flatMap { durable =>
            BftFinality.requireExtendsCheckpoint(remoteChain, durable.orElse(checkpoint))
              .flatMap(_ => to.writeChain(remoteChain))
          }
        case (None, _) =>
          BftFinality.requireExtendsCheckpoint(remoteChain, checkpoint)
            .flatMap(_ => to.writeChain(remoteChain))
    yield PullReport(fetched, fetchedBlobs, remoteChain.size - fetched)

object IdentityResolvers:
  /** Resolve identities from a node's replayed ledger, falling back to bootstrap. */
  def fromNode(node: Node, bootstrap: Map[String, Vector[Byte]]): Either[String, IdentityResolver] =
    node.state(bootstrap).map(st => IdentityResolver.of(bootstrap, st))

object Gossip:
  final case class Reorg(node: String, fromHead: Option[Digest], toHead: Digest, forkPoint: Int)
  final case class Peer(name: String, node: Node)

  def round(
      peers: List[Peer],
      authorities: Map[String, Vector[Byte]],
      checkpoint: Option[BftFinality.FinalizedCheckpoint] = None,
  ): Either[String, List[Reorg]] =
    val candidates: List[(Peer, List[Digest])] = peers.map(p => (p, p.node.chainDigests))
    val reorgs = List.newBuilder[Reorg]
    val result = peers.foldLeft[Either[String, Unit]](Right(())) { (acc, me) =>
      acc.flatMap { _ =>
        val mine = me.node.chainDigests
        val ranked = candidates
          .filter((p, chain) => BftFinality.shouldAdoptChain(mine, chain, checkpoint))
          .sortBy((p, chain) => (-chain.length, chain.lastOption.map(_.hex).getOrElse("")))
        ranked.headOption match
          case None => Right(())
          case Some((from, theirChain)) =>
            Sync.pull(from.node, me.node, authorities, checkpoint) match
              case Left(err) => Left(s"gossip: ${me.name} <- ${from.name}: $err")
              case Right(_) =>
                val forkPoint = mine.zip(theirChain).takeWhile(_ == _).length
                reorgs += Reorg(me.name, mine.lastOption, theirChain.last, forkPoint)
                Right(())
      }
    }
    result.map(_ => reorgs.result())

  def converge(peers: List[Peer], authorities: Map[String, Vector[Byte]], maxRounds: Int = 8): Either[String, List[Reorg]] =
    def loop(n: Int, acc: List[Reorg]): Either[String, List[Reorg]] =
      if n <= 0 then Right(acc)
      else round(peers, authorities).flatMap { rs =>
        if rs.isEmpty then Right(acc) else loop(n - 1, acc ++ rs) }
    loop(maxRounds, Nil)

object Provenance:
  final case class Record(output: Digest, inputs: List[Digest], tool: String):
    def canon: Canon = Canon.CTag("provenance", Canon.cmap(
      "output" -> Canon.CStr(output.hex),
      "inputs" -> Canon.cstrs(inputs.map(_.hex)),
      "tool" -> Canon.CStr(tool)))
    def artifact: Artifact = Artifact(ArtifactKind.Provenance, canon)

  def record(
      cas: cairn.systeminterface.Cas,
      output: Digest,
      inputs: List[Digest],
      tool: String,
      ctx: EffectContext,
  ): Either[cairn.systeminterface.Cas.Error, Digest] =
    CasEffects.put(cas, Record(output, inputs, tool).artifact, ctx).map(_.valueHash)

  def fromArtifact(a: Artifact): Option[Record] =
    if a.kind != ArtifactKind.Provenance then None
    else
      import Canon.*
      a.body match
        case CTag("provenance", m) => Some(Record(
          Digest(m.field("output").asStr),
          m.field("inputs").asList.map(c => Digest(c.asStr)),
          m.field("tool").asStr))
        case _ => None

  /** Inventory provenance records under a CAS root. Authorizes CAS `stats`
    * on the root (same inventory right as [[CasAdminEffects.stats]]), then
    * walks object files — no ungated public FS entry.
    */
  def index(root: java.nio.file.Path, ctx: EffectContext): Either[String, Map[String, Record]] =
    val abs = root.toAbsolutePath.normalize.toString
    ctx.authorize(
      ctx.registry.require(Effects.Family.Cas).actionKey("stats"),
      ctx.registry.require(Effects.Family.Cas).resource.at(abs)) match
      case Left(err) => Left(s"denied: $err")
      case Right(_) =>
        Right(
          CasAdmin.objectFiles(root)
            .flatMap(p => Artifact.decode(java.nio.file.Files.readAllBytes(p)).toOption)
            .flatMap(fromArtifact)
            .map(r => r.output.hex -> r)
            .toMap)

  final case class Hop(record: Record, depth: Int)

  /** Walk provenance edges to `target`. Authorizes via [[index]]. */
  def why(root: java.nio.file.Path, target: Digest, ctx: EffectContext): Either[String, List[Hop]] =
    index(root, ctx).map { idx =>
      def walk(d: Digest, depth: Int, seen: Set[String]): List[Hop] =
        if seen.contains(d.hex) then Nil
        else idx.get(d.hex) match
          case None => Nil
          case Some(r) =>
            Hop(r, depth) :: r.inputs.flatMap(i => walk(i, depth + 1, seen + d.hex))
      walk(target, 0, Set.empty)
    }
