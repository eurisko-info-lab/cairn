package cairn.systemhandler

import cairn.kernel.*
import java.nio.file.{Files, Path}
import java.time.Instant

/** Static + announced peer directory for gossip / BFT replica URLs.
  *
  * Discovery here is **directory-based**, not DHT: peers announce themselves
  * (or an operator plants entries), and [[HttpNode]] serves `GET /peers` so
  * others can pull the list. Membership for BFT replicas is a tagged subset
  * (`role = replica`); gossip sync peers use `role = gossip` (default).
  */
object PeerRegistry:
  enum Role:
    case Gossip, Replica
    def name: String = this match
      case Gossip  => "gossip"
      case Replica => "replica"
  object Role:
    def parse(s: String): Either[String, Role] = s match
      case "gossip"  => Right(Gossip)
      case "replica" => Right(Replica)
      case other     => Left(s"unknown peer role '$other'")

  final case class Peer(
      name: String,
      baseUrl: String,
      role: Role = Role.Gossip,
      publicKey: Option[Vector[Byte]] = None,
      lastSeenEpochMs: Long = Instant.now().toEpochMilli,
  ):
    def canon: Canon = Canon.cmap(
      "name" -> Canon.CStr(name),
      "baseUrl" -> Canon.CStr(baseUrl),
      "role" -> Canon.CStr(role.name),
      "publicKey" -> publicKey.fold(Canon.CTag("none", Canon.CInt(0)))(pk =>
        Canon.CTag("some", Canon.CBytes(pk))),
      "lastSeen" -> Canon.CInt(lastSeenEpochMs))

  object Peer:
    def fromCanon(c: Canon): Either[String, Peer] =
      import Canon.*
      try
        Role.parse(c.field("role").asStr).map { role =>
          val pk = c.field("publicKey") match
            case CTag("some", CBytes(bs)) => Some(bs)
            case _                        => None
          Peer(
            c.field("name").asStr,
            c.field("baseUrl").asStr,
            role,
            pk,
            c.field("lastSeen").asInt)
        }
      catch
        case e: CodecError => Left(e.getMessage)

  final case class Directory(peers: List[Peer]):
    def canon: Canon = Canon.CTag("peer-directory", Canon.CList(peers.map(_.canon)))
    def byName(n: String): Option[Peer] = peers.find(_.name == n)
    def gossipPeers: List[Peer] = peers.filter(_.role == Role.Gossip)
    def replicas: List[Peer] = peers.filter(_.role == Role.Replica)
    def upsert(p: Peer): Directory =
      Directory(peers.filterNot(_.name == p.name) :+ p.copy(lastSeenEpochMs = Instant.now().toEpochMilli))
    def remove(name: String): Directory = Directory(peers.filterNot(_.name == name))

  object Directory:
    def empty: Directory = Directory(Nil)
    def fromCanon(c: Canon): Either[String, Directory] =
      import Canon.*
      c match
        case CTag("peer-directory", CList(xs)) =>
          xs.foldLeft[Either[String, List[Peer]]](Right(Nil)) { (acc, row) =>
            acc.flatMap(ps => Peer.fromCanon(row).map(ps :+ _))
          }.map(Directory(_))
        case other => Left(s"not a peer-directory: $other")

  /** Persist under `$root/peers.canon` (raw Canon bytes). */
  def path(root: Path): Path = root.resolve("peers.canon")

  def load(root: Path): Either[String, Directory] =
    val p = path(root)
    if !Files.exists(p) then Right(Directory.empty)
    else
      Canon.decode(Files.readAllBytes(p)).flatMap(Directory.fromCanon)

  def save(root: Path, dir: Directory): Either[String, Unit] =
    try
      Files.createDirectories(root)
      Files.write(path(root), Canon.encode(dir.canon))
      Right(())
    catch case e: Exception => Left(e.getMessage)

  def add(
      root: Path,
      name: String,
      baseUrl: String,
      role: Role = Role.Gossip,
      publicKey: Option[Vector[Byte]] = None,
  ): Either[String, Directory] =
    load(root).flatMap { d =>
      val next = d.upsert(Peer(name, baseUrl, role, publicKey))
      save(root, next).map(_ => next)
    }

  def remove(root: Path, name: String): Either[String, Directory] =
    load(root).flatMap { d =>
      val next = d.remove(name)
      save(root, next).map(_ => next)
    }

  /** Merge a remote directory into local (upsert by name; prefer newer lastSeen). */
  def merge(local: Directory, remote: Directory): Directory =
    remote.peers.foldLeft(local) { (acc, rp) =>
      acc.byName(rp.name) match
        case Some(lp) if lp.lastSeenEpochMs >= rp.lastSeenEpochMs => acc
        case _ => acc.upsert(rp)
    }
