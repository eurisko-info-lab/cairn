package cairn.systemhandler

import cairn.kernel.*
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.concurrent.{Executors, ScheduledFuture, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

/** Multi-peer gossip over [[HttpSync]] — the network form of [[Gossip]].
  *
  * Fork choice: prefer longer chains; ties break on smallest tip digest —
  * but never adopt a chain that excludes the local [[BftFinality.FinalizedCheckpoint]].
  */
object HttpGossip:
  final case class Reorg(localName: String, fromHead: Option[Digest], toHead: Digest, via: String, forkPoint: Int)
  final case class RoundReport(reorgs: List[Reorg], pulled: List[String], errors: List[String])

  private val client = HttpClient.newHttpClient()

  private def getChain(baseUrl: String): Either[String, List[Digest]] =
    try
      val resp = client.send(
        HttpRequest.newBuilder(URI.create(s"$baseUrl/chain")).GET().build(),
        HttpResponse.BodyHandlers.ofString())
      if resp.statusCode() != 200 then Left(s"GET $baseUrl/chain -> ${resp.statusCode()}")
      else Right(resp.body().linesIterator.filter(_.nonEmpty).map(Digest(_)).toList)
    catch case e: Exception => Left(e.getMessage)

  /** Rank whether `theirs` should replace `mine` under the gossip fork rule. */
  def shouldAdopt(
      mine: List[Digest],
      theirs: List[Digest],
      checkpoint: Option[BftFinality.FinalizedCheckpoint] = None,
  ): Boolean =
    BftFinality.shouldAdoptChain(mine, theirs, checkpoint)

  /** One gossip round: pick the best remote chain among `peers` and pull if it wins. */
  def round(
      localName: String,
      local: Node,
      peers: List[PeerRegistry.Peer],
      authorities: Map[String, Vector[Byte]],
      checkpoint: Option[BftFinality.FinalizedCheckpoint] = None,
      checkpointHome: Option[java.nio.file.Path] = None,
  ): RoundReport =
    val mine = local.chainDigests
    val errors = List.newBuilder[String]
    val ranked = peers.flatMap { p =>
      getChain(p.baseUrl) match
        case Left(e) =>
          errors += s"${p.name}: $e"
          None
        case Right(chain) if shouldAdopt(mine, chain, checkpoint) =>
          Some((p, chain))
        case Right(chain) =>
          BftFinality.requireExtendsCheckpoint(chain, checkpoint) match
            case Left(e) => errors += s"${p.name}: $e"
            case Right(_) => ()
          None
    }.sortBy((p, chain) => (-chain.length, chain.lastOption.map(_.hex).getOrElse("")))

    ranked.headOption match
      case None => RoundReport(Nil, Nil, errors.result())
      case Some((from, theirChain)) =>
        HttpSync.pull(from.baseUrl, local, authorities, checkpoint, checkpointHome) match
          case Left(e) =>
            RoundReport(Nil, Nil, errors.result() :+ s"pull ${from.name}: $e")
          case Right(_) =>
            val forkPoint = mine.zip(theirChain).takeWhile(_ == _).length
            RoundReport(
              List(Reorg(localName, mine.lastOption, theirChain.last, from.name, forkPoint)),
              List(from.name),
              errors.result())

  /** Fetch remote peer directories and merge into the local registry. */
  def discover(localRoot: java.nio.file.Path, fromUrls: List[String]): Either[String, PeerRegistry.Directory] =
    def fetchPeers(url: String): Either[String, PeerRegistry.Directory] =
      try
        val resp = client.send(
          HttpRequest.newBuilder(URI.create(s"$url/peers")).GET().build(),
          HttpResponse.BodyHandlers.ofByteArray())
        if resp.statusCode() != 200 then Left(s"GET $url/peers -> ${resp.statusCode()}")
        else Canon.decode(resp.body()).flatMap(PeerRegistry.Directory.fromCanon)
      catch case e: Exception => Left(e.getMessage)

    PeerRegistry.load(localRoot).flatMap { local =>
      val merged = fromUrls.foldLeft(local) { (acc, url) =>
        fetchPeers(url) match
          case Right(remote) => PeerRegistry.merge(acc, remote)
          case Left(_)       => acc
      }
      PeerRegistry.save(localRoot, merged).map(_ => merged)
    }

/** Timed multi-peer gossip daemon: runs [[HttpGossip.round]] on an interval. */
final class GossipDaemon(
    localName: String,
    local: Node,
    peersRoot: java.nio.file.Path,
    authorities: Map[String, Vector[Byte]],
    intervalMs: Long = 2000L,
):
  private val scheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, s"cairn-gossip-$localName")
    t.setDaemon(true)
    t
  }
  private val running = AtomicBoolean(false)
  private var handle: ScheduledFuture[?] | Null = null
  private val lastReport = new java.util.concurrent.atomic.AtomicReference[HttpGossip.RoundReport](
    HttpGossip.RoundReport(Nil, Nil, Nil))

  def last: HttpGossip.RoundReport = lastReport.get()

  def tick(): HttpGossip.RoundReport =
    val peers = PeerRegistry.load(peersRoot).toOption.toList.flatMap(_.gossipPeers)
    val checkpoint = BftFinality.loadCheckpoint(peersRoot).toOption.flatten
    val report = HttpGossip.round(
      localName, local, peers, authorities,
      checkpoint = checkpoint, checkpointHome = Some(peersRoot))
    lastReport.set(report)
    report

  def start(): Unit =
    if running.compareAndSet(false, true) then
      handle = scheduler.scheduleAtFixedRate(
        () => try tick() catch case _: Throwable => (),
        0L,
        intervalMs,
        TimeUnit.MILLISECONDS)

  def stop(): Unit =
    running.set(false)
    Option(handle).foreach(_.cancel(false))
    scheduler.shutdownNow()
