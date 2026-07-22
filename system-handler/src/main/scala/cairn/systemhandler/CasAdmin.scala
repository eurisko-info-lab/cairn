package cairn.systemhandler

import cairn.kernel.*
import cairn.systeminterface.Cas
import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** CAS maintenance (M3): fsck, root-tracked mark/sweep GC, stats. MIGRATION-
  * PLAN.md Phase 1: moved here wholesale alongside `DiskCas` (`Cas.scala`,
  * same module) rather than split further — `DiskCas.putBytesAlgo` already
  * depends on this file's `HashAlgo`, so they can't live in different
  * modules, and the plan's own Phase 1 text doesn't name this file at all.
  * `HashAlgo`/`DigestMigration` are themselves pure; not worth a second split
  * this phase didn't ask for.
  *
  * Public entry is [[CasAdminEffects]] (authorize → perform). The helpers
  * below are the privileged implementation used only after authorization.
  */
object CasAdmin:
  final case class FsckReport(checked: Int, corrupt: List[Digest])
  final case class GcReport(kept: Int, swept: Int)
  final case class Stats(objects: Int, bytes: Long, byKind: Map[String, Int])

  private[systemhandler] def objectFiles(root: Path): List[Path] =
    val objs = root.resolve("objects")
    if !Files.exists(objs) then Nil
    else Files.walk(objs).iterator.asScala.filter(p => Files.isRegularFile(p) && !p.toString.endsWith(".corrupt")).toList

  private def digestOf(root: Path, p: Path): Digest =
    Digest(p.getParent.getFileName.toString + p.getFileName.toString)

  /** Re-hash every object; corrupted objects are quarantined (renamed `.corrupt`),
    * never silently served or deleted. Caller must have authorized `fsck`.
    */
  private[systemhandler] def fsck(root: Path): FsckReport =
    var checked = 0
    val corrupt = List.newBuilder[Digest]
    for p <- objectFiles(root) do
      checked += 1
      val claimed = digestOf(root, p)
      val actual = Digest.ofBytes(Files.readAllBytes(p))
      if actual != claimed then
        corrupt += claimed
        Files.move(p, p.resolveSibling(p.getFileName.toString + ".corrupt"))
    FsckReport(checked, corrupt.result())

  /** Digests referenced by an artifact: every 64-hex string in its canonical
    * body plus chunk manifests. Conservative (may over-approximate), which is
    * exactly what a GC marker must be.
    */
  def references(bs: Array[Byte]): Set[Digest] =
    def walk(c: Canon): Set[Digest] = c match
      case Canon.CStr(s) if s.length == 64 => Digest.parse(s).toOption.toSet
      case Canon.CList(xs)                 => xs.flatMap(walk).toSet
      case Canon.CMap(es)                  => es.flatMap((_, v) => walk(v)).toSet
      case Canon.CTag(_, v)                => walk(v)
      case _                               => Set.empty
    Canon.decode(bs).map(walk).getOrElse(Set.empty)

  /** Mark/sweep from roots. Never collects a reachable blob. Caller must have
    * authorized `gc`.
    */
  private[systemhandler] def gc(root: Path, roots: Set[Digest]): GcReport =
    val cas = DiskCas(root)
    val marked = scala.collection.mutable.Set[String]()
    def mark(d: Digest): Unit =
      if !marked.contains(d.hex) then
        marked += d.hex
        cas.getBytes(d).foreach(bs => references(bs).foreach(mark))
    roots.foreach(mark)
    var swept = 0
    for p <- objectFiles(root) do
      if !marked.contains(digestOf(root, p).hex) then
        Files.delete(p); swept += 1
    GcReport(marked.size, swept)

  private[systemhandler] def stats(root: Path): Stats =
    val files = objectFiles(root)
    val byKind = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    var bytes = 0L
    for p <- files do
      val bs = Files.readAllBytes(p)
      bytes += bs.length
      Artifact.decode(bs).foreach(a => byKind(a.kind.name) += 1)
    Stats(files.size, bytes, byKind.toMap)

/** Authorized CAS admin (fsck/gc/stats) — path-rooted, same authorize →
  * [[AuthorizedEffect]] → perform spine as [[CasEffects]].
  */
object CasAdminEffects:
  private def rootPath(root: Path): String = root.toAbsolutePath.toString

  def run(req: Cas.Request, ctx: EffectContext): Either[Cas.Error, Cas.Response] =
    val (action, resource) = CasEffects.intent(req, ctx.registry)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) => perform(req, auth, ctx.registry)

  def perform(req: Cas.Request, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): Either[Cas.Error, Cas.Response] =
    val (action, resource) = CasEffects.intent(req, registry)
    if !auth.covers(action, resource) then Left(Cas.Error.Io("authorized effect does not cover request"))
    else
      try req match
        case Cas.Request.Fsck(root) =>
          val r = CasAdmin.fsck(Path.of(root))
          Right(Cas.Response.FsckReport(r.checked, r.corrupt))
        case Cas.Request.Gc(root, roots) =>
          val r = CasAdmin.gc(Path.of(root), roots.toSet)
          Right(Cas.Response.GcReport(r.kept, r.swept))
        case Cas.Request.Stats(root) =>
          val r = CasAdmin.stats(Path.of(root))
          Right(Cas.Response.StatsReport(r.objects, r.bytes, r.byKind))
        case other =>
          Left(Cas.Error.Io(s"store request $other requires CasEffects"))
      catch case e: Exception => Left(Cas.Error.Io(Option(e.getMessage).getOrElse(e.toString)))

  def fsck(root: Path, ctx: EffectContext): Either[Cas.Error, CasAdmin.FsckReport] =
    run(Cas.Request.Fsck(rootPath(root)), ctx).flatMap {
      case Cas.Response.FsckReport(checked, corrupt) => Right(CasAdmin.FsckReport(checked, corrupt))
      case other => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  def gc(root: Path, roots: Set[Digest], ctx: EffectContext): Either[Cas.Error, CasAdmin.GcReport] =
    run(Cas.Request.Gc(rootPath(root), roots.toList), ctx).flatMap {
      case Cas.Response.GcReport(kept, swept) => Right(CasAdmin.GcReport(kept, swept))
      case other => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  def stats(root: Path, ctx: EffectContext): Either[Cas.Error, CasAdmin.Stats] =
    run(Cas.Request.Stats(rootPath(root)), ctx).flatMap {
      case Cas.Response.StatsReport(objects, bytes, byKind) =>
        Right(CasAdmin.Stats(objects, bytes, byKind))
      case other => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  /** Inventory decoded artifacts under a CAS root. Authorizes `stats` then
    * walks object files (same gate as [[cairn.systemhandler.Provenance.index]]).
    */
  def artifacts(root: Path, ctx: EffectContext): Either[Cas.Error, List[Artifact]] =
    val abs = rootPath(root)
    val i = CasEffects.intent(Cas.Request.Stats(abs), ctx.registry)
    ctx.authorize(i._1, i._2) match
      case Left(err) => Left(Cas.Error.Io(s"denied: $err"))
      case Right(_) =>
        Right(
          CasAdmin.objectFiles(root)
            .flatMap(p => Artifact.decode(Files.readAllBytes(p)).toOption))

/** Digest agility (M4): self-describing algorithm-prefixed digests alongside
  * the default sha-256, plus kernel-validated migration artifacts mapping
  * old-algorithm keys to new-algorithm keys for the same bytes.
  */
object HashAlgo:
  val supported: List[String] = List("sha256", "sha512")
  private def md(algo: String): MessageDigest = algo match
    case "sha256" => MessageDigest.getInstance("SHA-256")
    case "sha512" => MessageDigest.getInstance("SHA-512")
    case other    => throw IllegalArgumentException(s"unsupported hash algorithm '$other'")
  def hash(algo: String, bs: Array[Byte]): String =
    md(algo).digest(bs).map(b => f"${b & 0xff}%02x").mkString
  /** Self-describing render, e.g. `sha256:ab12…`. */
  def render(algo: String, hex: String): String = s"$algo:$hex"
  def parse(s: String): Either[String, (String, String)] = s.split(":", 2) match
    case Array(algo, hex) if supported.contains(algo) => Right((algo, hex))
    case _ => Left(s"not an algorithm-prefixed digest: '$s'")

object DigestMigration:
  /** Migration artifact: entries certify that one byte string carries both keys. */
  def build(entries: List[(Array[Byte])]): Artifact =
    Artifact(ArtifactKind.Migration, Canon.CList(entries.map { bs =>
      Canon.cmap(
        "sha256" -> Canon.CStr(HashAlgo.hash("sha256", bs)),
        "sha512" -> Canon.CStr(HashAlgo.hash("sha512", bs))) }))

  /** Kernel gate: every mapping entry must re-hash correctly under BOTH
    * algorithms against the actual bytes (fetched by the old key).
    */
  def validate(migration: Artifact, fetch: String => Either[String, Array[Byte]]): Either[String, Int] =
    if migration.kind != ArtifactKind.Migration then Left("not a migration artifact")
    else
      import Canon.*
      val entries = migration.body.asList
      entries.zipWithIndex.foldLeft[Either[String, Int]](Right(0)) { case (acc, (e, i)) =>
        acc.flatMap { n =>
          val h256 = e.field("sha256").asStr
          val h512 = e.field("sha512").asStr
          fetch(h256).flatMap { bs =>
            if HashAlgo.hash("sha256", bs) != h256 then Left(s"entry $i: sha256 mismatch")
            else if HashAlgo.hash("sha512", bs) != h512 then Left(s"entry $i: sha512 mismatch")
            else Right(n + 1) } } }

/** Content-defined chunking for large blobs (M5): rolling-sum boundaries,
  * bounded-memory streaming, chunk-level dedup, Merkle-list manifests.
  * Puts/gets authorize through [[CasEffects]].
  */
object Chunker:
  val MinChunk = 64 * 1024
  val MaxChunk = 4 * 1024 * 1024
  val BoundaryMask = (1 << 20) - 1 // ~1 MiB average

  /** Stream `in` into the CAS as chunks; returns the manifest artifact digest. */
  def putStream(cas: Cas, in: InputStream, ctx: EffectContext): Either[String, Digest] =
    val chunks = List.newBuilder[(Digest, Long)]
    val buf = new java.io.ByteArrayOutputStream(MaxChunk)
    var rolling = 0
    var b = in.read()
    def flush(): Either[String, Unit] =
      if buf.size == 0 then Right(())
      else
        val bs = buf.toByteArray
        CasEffects.putBytes(cas, bs, ctx).left.map(_.toString).map { d =>
          chunks += ((d, bs.length.toLong))
          buf.reset(); rolling = 0
        }
    def loop(): Either[String, Unit] =
      if b < 0 then flush()
      else
        buf.write(b)
        rolling = (rolling << 1) + (b & 0xff) + (rolling >>> 24)
        b = in.read()
        if buf.size >= MaxChunk || (buf.size >= MinChunk && (rolling & BoundaryMask) == 0) then
          flush().flatMap(_ => loop())
        else loop()
    loop().flatMap { _ =>
      val manifest = Artifact(ArtifactKind.ChunkedBlob, Canon.CList(chunks.result().map { (d, n) =>
        Canon.cmap("chunk" -> Canon.CStr(d.hex), "size" -> Canon.CInt(n)) }))
      CasEffects.put(cas, manifest, ctx).left.map(_.toString).map(_.valueHash)
    }

  /** Stream a chunked blob back out; memory bounded by max chunk size. */
  def getStream(cas: Cas, manifest: Digest, out: OutputStream, ctx: EffectContext): Either[String, Long] =
    CasEffects.get(cas, manifest, ctx).left.map(_.toString).flatMap { a =>
      if a.kind != ArtifactKind.ChunkedBlob then Left(s"${manifest.short} is not a chunked blob")
      else
        import Canon.*
        a.body.asList.foldLeft[Either[String, Long]](Right(0L)) { (acc, e) =>
          acc.flatMap { total =>
            CasEffects.getBytes(cas, Digest(e.field("chunk").asStr), ctx).left.map(_.toString).map { bs =>
              out.write(bs); total + bs.length
            }
          }
        }
    }

  def chunkCount(cas: Cas, manifest: Digest, ctx: EffectContext): Either[String, Int] =
    CasEffects.get(cas, manifest, ctx).left.map(_.toString).map(_.body.asList.size)
