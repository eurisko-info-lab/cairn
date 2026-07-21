package cairn.systemhandler

import cairn.kernel.*
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

/** HTTP node surface + want/have pull sync (Phase 3 http/network families).
  * Moved from `ledger.Distribution`. Chain-file writes go through
  * [[Filesystem]] on the consumer node (same as [[Sync.pull]]).
  */
final class HttpNode(node: Node, authorities: Map[String, Vector[Byte]]):
  private var server: HttpServer | Null = null

  def start(): Int =
    val s = HttpServer.create(InetSocketAddress(0), 0)
    def reply(ex: HttpExchange, code: Int, body: Array[Byte]): Unit =
      ex.sendResponseHeaders(code, body.length)
      ex.getResponseBody.write(body)
      ex.close()
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
    s.start()
    server = s
    s.getAddress.getPort

  def stop(): Unit = Option(server).foreach(_.stop(0))

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

  /** Fetch `d` when absent. Authorized `contains` / `putBytes` failures abort
    * — never treat denial as "missing" via `Either.forall`.
    */
  private def fetchIfMissing(
      baseUrl: String, to: Node, d: Digest, fetched: Int
  ): Either[String, Int] =
    CasEffects.contains(to.cas, d, to.ctx).left.map(casErr).flatMap {
      case true => Right(fetched)
      case false =>
        get(baseUrl, s"/blob/${d.hex}").flatMap { bs =>
          CasEffects.putBytes(to.cas, bs, to.ctx).left.map(casErr).flatMap { actual =>
            if actual == d then Right(fetched + 1)
            else Left(s"remote served wrong bytes for ${d.short}")
          }
        }
    }

  def pull(baseUrl: String, to: Node, authorities: Map[String, Vector[Byte]]): Either[String, PullReport] =
    for
      chainTxt <- get(baseUrl, "/chain")
      remoteChain = new String(chainTxt).linesIterator.filter(_.nonEmpty).map(Digest(_)).toList
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
      _ <- to.writeChain(remoteChain)
    yield PullReport(fetched, fetchedBlobs, remoteChain.size - fetched)

object Gossip:
  final case class Reorg(node: String, fromHead: Option[Digest], toHead: Digest, forkPoint: Int)
  final case class Peer(name: String, node: Node)

  def round(peers: List[Peer], authorities: Map[String, Vector[Byte]]): Either[String, List[Reorg]] =
    val candidates: List[(Peer, List[Digest])] = peers.map(p => (p, p.node.chainDigests))
    val reorgs = List.newBuilder[Reorg]
    val result = peers.foldLeft[Either[String, Unit]](Right(())) { (acc, me) =>
      acc.flatMap { _ =>
        val mine = me.node.chainDigests
        val ranked = candidates
          .filter((p, chain) => chain.length > mine.length ||
            (chain.length == mine.length && chain != mine &&
             chain.lastOption.map(_.hex).getOrElse("") < mine.lastOption.map(_.hex).getOrElse("~")))
          .sortBy((p, chain) => (-chain.length, chain.lastOption.map(_.hex).getOrElse("")))
        ranked.headOption match
          case None => Right(())
          case Some((from, theirChain)) =>
            Sync.pull(from.node, me.node, authorities) match
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
    ctx.authorize(EffectMeta.cas.actionKey("stats"), EffectMeta.cas.resource.at(abs)) match
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
