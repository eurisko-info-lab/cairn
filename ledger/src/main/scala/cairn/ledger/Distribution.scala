package cairn.ledger

import cairn.kernel.*
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files

/** M38: minimal HTTP node surface + want/have pull sync. Everything fetched
  * is verified on arrival (blocks replay through the pure kernel, blobs
  * re-hash in the CAS); interruption is safe because every step is an
  * idempotent content-addressed write — re-running resumes.
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
      Digest.parse(hex).flatMap(node.cas.getBytes) match
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

  /** Want/have negotiation: fetch the remote chain listing, diff against what
    * we already hold, fetch only the missing digests, verify, then adopt.
    */
  def pull(baseUrl: String, to: Node, authorities: Map[String, Vector[Byte]]): Either[String, PullReport] =
    for
      chainTxt <- get(baseUrl, "/chain")
      remoteChain = new String(chainTxt).linesIterator.filter(_.nonEmpty).map(Digest(_)).toList
      // have/want on blocks
      wantBlocks = remoteChain.filterNot(to.cas.contains)
      fetched <- wantBlocks.foldLeft[Either[String, Int]](Right(0)) { (acc, d) =>
        acc.flatMap { n =>
          get(baseUrl, s"/blob/${d.hex}").flatMap { bs =>
            val actual = to.cas.putBytes(bs) // putBytes re-hashes: verified on arrival
            if actual == d then Right(n + 1) else Left(s"remote served wrong bytes for ${d.short}")
          }
        }
      }
      blocks <- remoteChain.foldLeft[Either[String, List[Block]]](Right(Nil)) { (acc, d) =>
        acc.flatMap(bs => to.cas.getByDigest(d).map(a => bs :+ Block.fromCanon(a.body))) }
      // kernel verification BEFORE adopting the chain
      st <- LedgerKernel.replay(authorities, blocks, Ed25519.verify)
      // want/have on published artifact bodies
      publishedDigests = st.published.toList.flatMap(_.split(":") match
        case Array(_, value, _) => Digest.parse(value).toOption
        case _                  => None)
      wantBlobs = publishedDigests.filterNot(to.cas.contains)
      fetchedBlobs <- wantBlobs.foldLeft[Either[String, Int]](Right(0)) { (acc, d) =>
        acc.flatMap { n =>
          get(baseUrl, s"/blob/${d.hex}").flatMap { bs =>
            if to.cas.putBytes(bs) == d then Right(n + 1) else Left(s"remote served wrong bytes for ${d.short}") }
        }
      }
    yield
      Files.writeString(to.root.resolve("chain"), remoteChain.map(_.hex).mkString("", "\n", "\n"))
      PullReport(fetched, fetchedBlobs, remoteChain.size - fetched)

/** M39: gossip + deterministic fork choice over a set of local nodes.
  * Fork rule: longest valid chain wins; ties break on lexicographically
  * smallest head digest. Reorgs are EXPLICIT events; nothing merges silently.
  */
object Gossip:
  final case class Reorg(node: String, fromHead: Option[Digest], toHead: Digest, forkPoint: Int)

  final case class Peer(name: String, node: Node)

  /** One gossip round: every peer offers its chain; every peer adopts the
    * best valid chain it heard. Returns reorg events.
    */
  def round(peers: List[Peer], authorities: Map[String, Vector[Byte]]): Either[String, List[Reorg]] =
    // collect all candidate chains with their blocks (validated lazily below)
    val candidates: List[(Peer, List[Digest])] = peers.map(p => (p, p.node.chainDigests))
    val reorgs = List.newBuilder[Reorg]
    val result = peers.foldLeft[Either[String, Unit]](Right(())) { (acc, me) =>
      acc.flatMap { _ =>
        val mine = me.node.chainDigests
        // pick the best candidate: longest valid; tie-break on smallest head hex
        val ranked = candidates
          .filter((p, chain) => chain.length > mine.length ||
            (chain.length == mine.length && chain != mine &&
             chain.lastOption.map(_.hex).getOrElse("") < mine.lastOption.map(_.hex).getOrElse("~")))
          .sortBy((p, chain) => (-chain.length, chain.lastOption.map(_.hex).getOrElse("")))
        ranked.headOption match
          case None => Right(())
          case Some((from, theirChain)) =>
            // validate before adopting (Sync.pull replays the kernel)
            Sync.pull(from.node, me.node, authorities) match
              case Left(err) => Left(s"gossip: ${me.name} <- ${from.name}: $err")
              case Right(_) =>
                val forkPoint = mine.zip(theirChain).takeWhile(_ == _).length
                reorgs += Reorg(me.name, mine.lastOption, theirChain.last, forkPoint)
                Right(())
      }
    }
    result.map(_ => reorgs.result())

  /** Run rounds until no peer changes (or maxRounds). */
  def converge(peers: List[Peer], authorities: Map[String, Vector[Byte]], maxRounds: Int = 8): Either[String, List[Reorg]] =
    def loop(n: Int, acc: List[Reorg]): Either[String, List[Reorg]] =
      if n <= 0 then Right(acc)
      else round(peers, authorities).flatMap { rs =>
        if rs.isEmpty then Right(acc) else loop(n - 1, acc ++ rs) }
    loop(maxRounds, Nil)

/** M40: provenance records — origin and derivation of artifacts as artifacts.
  * `why` walks the provenance DAG backwards from any digest.
  */
object Provenance:
  final case class Record(output: Digest, inputs: List[Digest], tool: String):
    def canon: Canon = Canon.CTag("provenance", Canon.cmap(
      "output" -> Canon.CStr(output.hex),
      "inputs" -> Canon.cstrs(inputs.map(_.hex)),
      "tool" -> Canon.CStr(tool)))
    def artifact: Artifact = Artifact(ArtifactKind.Provenance, canon)

  def record(cas: cairn.systeminterface.Cas, output: Digest, inputs: List[Digest], tool: String): Digest =
    cas.put(Record(output, inputs, tool).artifact).valueHash

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

  /** All provenance records in a disk CAS, indexed by output digest. */
  def index(root: java.nio.file.Path): Map[String, Record] =
    val objs = root.resolve("objects")
    if !Files.exists(objs) then Map.empty
    else
      import scala.jdk.CollectionConverters.*
      Files.walk(objs).iterator.asScala
        .filter(p => Files.isRegularFile(p) && !p.toString.endsWith(".corrupt"))
        .flatMap(p => Artifact.decode(Files.readAllBytes(p)).toOption)
        .flatMap(fromArtifact)
        .map(r => r.output.hex -> r)
        .toMap

  final case class Hop(record: Record, depth: Int)

  /** Walk backwards: why does this digest exist? */
  def why(root: java.nio.file.Path, target: Digest): List[Hop] =
    val idx = index(root)
    def walk(d: Digest, depth: Int, seen: Set[String]): List[Hop] =
      if seen.contains(d.hex) then Nil
      else idx.get(d.hex) match
        case None => Nil
        case Some(r) =>
          Hop(r, depth) :: r.inputs.flatMap(i => walk(i, depth + 1, seen + d.hex))
    walk(target, 0, Set.empty)
