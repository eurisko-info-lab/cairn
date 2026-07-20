package cairn.workbench

import cairn.kernel.*
import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** CAS maintenance (M3): fsck, root-tracked mark/sweep GC, stats. */
object CasAdmin:
  final case class FsckReport(checked: Int, corrupt: List[Digest])
  final case class GcReport(kept: Int, swept: Int)
  final case class Stats(objects: Int, bytes: Long, byKind: Map[String, Int])

  private def objectFiles(root: Path): List[Path] =
    val objs = root.resolve("objects")
    if !Files.exists(objs) then Nil
    else Files.walk(objs).iterator.asScala.filter(p => Files.isRegularFile(p) && !p.toString.endsWith(".corrupt")).toList

  private def digestOf(root: Path, p: Path): Digest =
    Digest(p.getParent.getFileName.toString + p.getFileName.toString)

  /** Re-hash every object; corrupted objects are quarantined (renamed `.corrupt`),
    * never silently served or deleted.
    */
  def fsck(root: Path): FsckReport =
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

  /** Mark/sweep from roots. Never collects a reachable blob. */
  def gc(root: Path, roots: Set[Digest]): GcReport =
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

  def stats(root: Path): Stats =
    val files = objectFiles(root)
    val byKind = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    var bytes = 0L
    for p <- files do
      val bs = Files.readAllBytes(p)
      bytes += bs.length
      Artifact.decode(bs).foreach(a => byKind(a.kind.name) += 1)
    Stats(files.size, bytes, byKind.toMap)

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
  */
object Chunker:
  val MinChunk = 64 * 1024
  val MaxChunk = 4 * 1024 * 1024
  val BoundaryMask = (1 << 20) - 1 // ~1 MiB average

  /** Stream `in` into the CAS as chunks; returns the manifest artifact digest. */
  def putStream(cas: DiskCas, in: InputStream): Digest =
    val chunks = List.newBuilder[(Digest, Long)]
    val buf = new java.io.ByteArrayOutputStream(MaxChunk)
    var rolling = 0
    var b = in.read()
    def flush(): Unit =
      if buf.size > 0 then
        val bs = buf.toByteArray
        chunks += ((cas.putBytes(bs), bs.length.toLong))
        buf.reset(); rolling = 0
    while b >= 0 do
      buf.write(b)
      rolling = (rolling << 1) + (b & 0xff) + (rolling >>> 24) // cheap rolling sum
      if buf.size >= MaxChunk || (buf.size >= MinChunk && (rolling & BoundaryMask) == 0) then flush()
      b = in.read()
    flush()
    val manifest = Artifact(ArtifactKind.ChunkedBlob, Canon.CList(chunks.result().map { (d, n) =>
      Canon.cmap("chunk" -> Canon.CStr(d.hex), "size" -> Canon.CInt(n)) }))
    cas.put(manifest).valueHash

  /** Stream a chunked blob back out; memory bounded by max chunk size. */
  def getStream(cas: DiskCas, manifest: Digest, out: OutputStream): Either[String, Long] =
    cas.getByDigest(manifest).flatMap { a =>
      if a.kind != ArtifactKind.ChunkedBlob then Left(s"${manifest.short} is not a chunked blob")
      else
        import Canon.*
        a.body.asList.foldLeft[Either[String, Long]](Right(0L)) { (acc, e) =>
          acc.flatMap { total =>
            cas.getBytes(Digest(e.field("chunk").asStr)).map { bs => out.write(bs); total + bs.length } } }
    }

  def chunkCount(cas: DiskCas, manifest: Digest): Either[String, Int] =
    cas.getByDigest(manifest).map(_.body.asList.size)
