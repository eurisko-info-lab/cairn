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

/** Named branch refs over a CAS; ref file stores the manifest digest. */
final class Branches(cas: Cas, refsDir: Path):
  private def refPath(branch: String): Path =
    require(branch.nonEmpty && branch.forall(c => c.isLetterOrDigit || c == '-' || c == '_'), s"bad branch name '$branch'")
    refsDir.resolve(branch)

  def load(branch: String): BranchManifest =
    val p = refPath(branch)
    if !Files.exists(p) then BranchManifest(branch, None, Nil)
    else
      val d = Digest(Files.readString(p).trim)
      cas.getByDigest(d).map(a => BranchManifest.fromCanon(a.body)).fold(e => throw RuntimeException(e), identity)

  /** Append: new head goes to history head; manifest itself stored in CAS. */
  def advance(branch: String, newHead: TypedKey): BranchManifest =
    val cur = load(branch)
    val next = BranchManifest(branch, Some(newHead), cur.head.toList ++ cur.history)
    val key = cas.put(next.artifact)
    Files.createDirectories(refsDir)
    Files.writeString(refPath(branch), key.valueHash.hex)
    next

  def list(): List[String] =
    if !Files.exists(refsDir) then Nil
    else Files.list(refsDir).map(_.getFileName.toString).toArray.toList.map(_.toString).sorted
