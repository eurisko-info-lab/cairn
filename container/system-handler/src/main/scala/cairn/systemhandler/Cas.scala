package cairn.systemhandler

import cairn.kernel.*
import cairn.systeminterface.Cas
import java.nio.file.{Files, Path}

/** MIGRATION-PLAN.md Phase 1: the System Handler half of the old
  * `workbench.Cas.scala` — concrete, effectful `Cas` implementations plus
  * the filesystem-backed branch-ref store. The pure `Cas` contract lives in
  * `system-interface`; `BranchManifest` (pure data, its validity is a
  * Kernel concern) lives in `kernel`.
  */
final class MemCas extends Cas:
  private val store = scala.collection.mutable.Map[String, Array[Byte]]()
  def putBytes(bs: Array[Byte]): Digest =
    val d = Digest.ofBytes(bs); store(d.hex) = bs; d
  def getBytes(d: Digest): Either[String, Array[Byte]] =
    store.get(d.hex).toRight(s"blob ${d.short} not in CAS")
  def contains(d: Digest): Boolean = store.contains(d.hex)

/** Disk CAS: `<root>/objects/ab/cdef...`; digest re-verified on read (corruption check). */
final class DiskCas(root: Path) extends Cas:
  private def pathOf(d: Digest): Path = root.resolve("objects").resolve(d.hex.take(2)).resolve(d.hex.drop(2))
  def putBytes(bs: Array[Byte]): Digest =
    val d = Digest.ofBytes(bs)
    val p = pathOf(d)
    if !Files.exists(p) then
      Files.createDirectories(p.getParent)
      Files.write(p, bs)
    d
  def getBytes(d: Digest): Either[String, Array[Byte]] =
    val p = pathOf(d)
    if !Files.exists(p) then Left(s"blob ${d.short} not in CAS at $root")
    else
      val bs = Files.readAllBytes(p)
      val actual = Digest.ofBytes(bs)
      if actual == d then Right(bs)
      else Left(s"CAS corruption: blob ${d.short} hashes to ${actual.short}")
  def contains(d: Digest): Boolean = Files.exists(pathOf(d))

  // -- digest agility (M4): non-default algorithms live in sibling stores --
  private def pathOfAlgo(algo: String, hex: String): Path =
    root.resolve(s"objects-$algo").resolve(hex.take(2)).resolve(hex.drop(2))
  /** Store under an explicit algorithm; returns the self-describing key. */
  def putBytesAlgo(algo: String, bs: Array[Byte]): String =
    if algo == "sha256" then HashAlgo.render(algo, putBytes(bs).hex)
    else
      val hex = HashAlgo.hash(algo, bs)
      val p = pathOfAlgo(algo, hex)
      if !Files.exists(p) then { Files.createDirectories(p.getParent); Files.write(p, bs) }
      HashAlgo.render(algo, hex)
  /** Read by self-describing key (`algo:hex`), verifying under that algorithm. */
  def getBytesKey(key: String): Either[String, Array[Byte]] =
    HashAlgo.parse(key).flatMap { (algo, hex) =>
      if algo == "sha256" then Digest.parse(hex).flatMap(getBytes)
      else
        val p = pathOfAlgo(algo, hex)
        if !Files.exists(p) then Left(s"blob $key not in CAS at $root")
        else
          val bs = Files.readAllBytes(p)
          if HashAlgo.hash(algo, bs) == hex then Right(bs)
          else Left(s"CAS corruption on $key") }

/** Authorized CAS put/get/contains over a store — same authorize →
  * [[AuthorizedEffect]] → perform spine as Filesystem / Workspace.
  * Admin ops (fsck/gc/stats) live on [[CasAdminEffects]] (path-rooted).
  */
object CasEffects:
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.Cas)

  private def ctorName(req: Cas.Request): String = req match
    case Cas.Request.Put(_)       => "put"
    case Cas.Request.Get(_)       => "get"
    case Cas.Request.Contains(_)  => "contains"
    case Cas.Request.Fsck(_)      => "fsck"
    case Cas.Request.Gc(_, _)     => "gc"
    case Cas.Request.Stats(_)     => "stats"

  private def resourcePath(req: Cas.Request): String = req match
    case Cas.Request.Put(a)       => a.digest.hex
    case Cas.Request.Get(d)       => d.hex
    case Cas.Request.Contains(d)  => d.hex
    case Cas.Request.Fsck(root)   => root
    case Cas.Request.Gc(root, _)  => root
    case Cas.Request.Stats(root)  => root

  def intent(req: Cas.Request, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    val i = iface(registry)
    (i.keyFor(ctorName(req)).get, i.resource.at(resourcePath(req)))

  /** Store-backed requests only (`put` / `get` / `contains`). Admin requests
    * must go through [[CasAdminEffects]]. */
  def run(store: Cas, req: Cas.Request, ctx: EffectContext): Either[Cas.Error, Cas.Response] =
    req match
      case _: Cas.Request.Fsck | _: Cas.Request.Gc | _: Cas.Request.Stats =>
        Left(Cas.Error.Io(s"admin request ${ctorName(req)} requires CasAdminEffects"))
      case _ =>
        val (action, resource) = intent(req, ctx.registry)
        ctx.authorize(action, resource) match
          case Left(err)   => Left(Cas.Error.Io(s"denied: $err"))
          case Right(auth) => perform(store, req, auth, ctx.registry)

  def perform(store: Cas, req: Cas.Request, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): Either[Cas.Error, Cas.Response] =
    val (action, resource) = intent(req, registry)
    if !auth.covers(action, resource) then Left(Cas.Error.Io("authorized effect does not cover request"))
    else
      try req match
        case Cas.Request.Put(artifact) =>
          Right(Cas.Response.Key(store.put(artifact)))
        case Cas.Request.Get(digest) =>
          store.getByDigest(digest) match
            case Right(a) => Right(Cas.Response.Stored(a))
            case Left(_)  => Left(Cas.Error.Missing(digest))
        case Cas.Request.Contains(digest) =>
          Right(Cas.Response.Present(store.contains(digest)))
        case Cas.Request.Fsck(_) | Cas.Request.Gc(_, _) | Cas.Request.Stats(_) =>
          Left(Cas.Error.Io(s"admin request ${ctorName(req)} requires CasAdminEffects"))
      catch case e: Exception => Left(Cas.Error.Io(e.getMessage))

  /** Convenience: authorize + put an artifact; returns its typed key. */
  def put(store: Cas, artifact: Artifact, ctx: EffectContext): Either[Cas.Error, TypedKey] =
    run(store, Cas.Request.Put(artifact), ctx).flatMap {
      case Cas.Response.Key(k) => Right(k)
      case other               => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  /** Convenience: authorize + get an artifact by digest. */
  def get(store: Cas, digest: Digest, ctx: EffectContext): Either[Cas.Error, Artifact] =
    run(store, Cas.Request.Get(digest), ctx).flatMap {
      case Cas.Response.Stored(a) => Right(a)
      case other                  => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  /** Authorize get-class existence check at `digest`. */
  def contains(store: Cas, digest: Digest, ctx: EffectContext): Either[Cas.Error, Boolean] =
    run(store, Cas.Request.Contains(digest), ctx).flatMap {
      case Cas.Response.Present(p) => Right(p)
      case other                   => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  /** Authorize put at the content digest, then store raw bytes (Sync path). */
  def putBytes(store: Cas, bs: Array[Byte], ctx: EffectContext): Either[Cas.Error, Digest] =
    val d = Digest.ofBytes(bs)
    val i = iface(ctx.registry)
    ctx.authorize(i.actionKey("put"), i.resource.at(d.hex)) match
      case Left(err) => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) =>
        if !auth.covers(i.actionKey("put"), i.resource.at(d.hex)) then
          Left(Cas.Error.Io("authorized effect does not cover request"))
        else
          try Right(store.putBytes(bs))
          catch case e: Exception => Left(Cas.Error.Io(e.getMessage))

  /** Authorize get at `digest`, then read raw bytes (Sync path). */
  def getBytes(store: Cas, digest: Digest, ctx: EffectContext): Either[Cas.Error, Array[Byte]] =
    val i = iface(ctx.registry)
    ctx.authorize(i.actionKey("get"), i.resource.at(digest.hex)) match
      case Left(err) => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) =>
        if !auth.covers(i.actionKey("get"), i.resource.at(digest.hex)) then
          Left(Cas.Error.Io("authorized effect does not cover request"))
        else store.getBytes(digest).left.map(_ => Cas.Error.Missing(digest))
