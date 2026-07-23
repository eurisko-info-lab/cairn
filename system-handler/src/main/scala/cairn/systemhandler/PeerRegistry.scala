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
  *
  * When a `publicKey` is present, `urlSeal` must bind `(name, baseUrl, role)`.
  * Replica-role entries always require a verified binding. Unsigned gossip
  * plants remain allowed for operator-local directories.
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

  def bindingBytes(name: String, baseUrl: String, role: Role): Array[Byte] =
    Canon.encode(Canon.cmap(
      "name" -> Canon.CStr(name),
      "baseUrl" -> Canon.CStr(baseUrl),
      "role" -> Canon.CStr(role.name)))

  final case class Peer(
      name: String,
      baseUrl: String,
      role: Role = Role.Gossip,
      publicKey: Option[Vector[Byte]] = None,
      lastSeenEpochMs: Long = Instant.now().toEpochMilli,
      urlSeal: Option[Vector[Byte]] = None,
  ):
    def canon: Canon = Canon.cmap(
      "name" -> Canon.CStr(name),
      "baseUrl" -> Canon.CStr(baseUrl),
      "role" -> Canon.CStr(role.name),
      "publicKey" -> publicKey.fold(Canon.CTag("none", Canon.CInt(0)))(pk =>
        Canon.CTag("some", Canon.CBytes(pk))),
      "lastSeen" -> Canon.CInt(lastSeenEpochMs),
      "urlSeal" -> urlSeal.fold(Canon.CTag("none", Canon.CInt(0)))(s =>
        Canon.CTag("some", Canon.CBytes(s))))

  object Peer:
    def fromCanon(c: Canon): Either[String, Peer] =
      import Canon.*
      try
        Role.parse(c.field("role").asStr).flatMap { role =>
          val fields = c.asMap
          val pk = fields.getOrElse("publicKey", CTag("none", CInt(0))) match
            case CTag("some", CBytes(bs)) => Some(bs)
            case _                        => None
          val seal = fields.getOrElse("urlSeal", CTag("none", CInt(0))) match
            case CTag("some", CBytes(bs)) => Some(bs)
            case _                        => None
          val peer = Peer(
            c.field("name").asStr,
            c.field("baseUrl").asStr,
            role,
            pk,
            c.field("lastSeen").asInt,
            seal)
          verifyBinding(peer).map(_ => peer)
        }
      catch
        case e: CodecError => Left(e.getMessage)

    /** Build a peer whose URL is Ed25519-bound to `signer`. */
    def bound(signer: Signer, baseUrl: String, role: Role = Role.Gossip): Peer =
      val seal = signer.sign(bindingBytes(signer.name, baseUrl, role))
      Peer(
        signer.name,
        baseUrl,
        role,
        publicKey = Some(signer.publicBytes),
        urlSeal = Some(seal))

    /** Verify optional URL binding. Replica role always requires a valid seal. */
    def verifyBinding(p: Peer): Either[String, Unit] =
      (p.publicKey, p.urlSeal, p.role) match
        case (None, None, Role.Gossip) => Right(())
        case (None, None, Role.Replica) =>
          Left(s"peer '${p.name}': replica role requires signed URL binding")
        case (None, Some(_), _) =>
          Left(s"peer '${p.name}': urlSeal without publicKey")
        case (Some(_), None, _) =>
          Left(s"peer '${p.name}': publicKey requires urlSeal")
        case (Some(pk), Some(seal), _) =>
          if Ed25519.verify(pk, bindingBytes(p.name, p.baseUrl, p.role), seal) then Right(())
          else Left(s"peer '${p.name}': bad urlSeal")

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
      urlSeal: Option[Vector[Byte]] = None,
  ): Either[String, Directory] =
    val peer = Peer(name, baseUrl, role, publicKey, urlSeal = urlSeal)
    Peer.verifyBinding(peer).flatMap { _ =>
      load(root).flatMap { d =>
        val next = d.upsert(peer)
        save(root, next).map(_ => next)
      }
    }

  /** Add a peer with URL binding signed by `signer` (name must match). */
  def addBound(
      root: Path,
      signer: Signer,
      baseUrl: String,
      role: Role = Role.Gossip,
  ): Either[String, Directory] =
    val peer = Peer.bound(signer, baseUrl, role)
    load(root).flatMap { d =>
      val next = d.upsert(peer)
      save(root, next).map(_ => next)
    }

  def remove(root: Path, name: String): Either[String, Directory] =
    load(root).flatMap { d =>
      val next = d.remove(name)
      save(root, next).map(_ => next)
    }

  /** Merge a remote directory into local (upsert by name; prefer newer lastSeen).
    * Peers that fail URL-binding verification are skipped.
    */
  def merge(local: Directory, remote: Directory): Directory =
    remote.peers.foldLeft(local) { (acc, rp) =>
      Peer.verifyBinding(rp) match
        case Left(_) => acc
        case Right(()) =>
          acc.byName(rp.name) match
            case Some(lp) if lp.lastSeenEpochMs >= rp.lastSeenEpochMs => acc
            case _ => acc.upsert(rp)
    }
