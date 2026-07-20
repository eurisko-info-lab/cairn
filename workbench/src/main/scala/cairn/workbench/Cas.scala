package cairn.workbench

import cairn.kernel.*
import java.nio.file.{Files, Path}

/** Local content-addressed store (S4). Bodies are canonical artifact bytes;
  * keys are content digests. Local-first: the working store, not the ledger (§4.9).
  */
trait Cas:
  def putBytes(bs: Array[Byte]): Digest
  def getBytes(d: Digest): Either[String, Array[Byte]]
  def contains(d: Digest): Boolean

  def put(a: Artifact): TypedKey =
    putBytes(Canon.encode(a.canon)); a.key
  def get(key: TypedKey): Either[String, Artifact] =
    for
      bs <- getBytes(key.valueHash)
      a <- Artifact.decode(bs)
      _ <- TypedKey.check(key, a.key)
    yield a
  def getByDigest(d: Digest): Either[String, Artifact] =
    getBytes(d).flatMap(Artifact.decode)

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

/** Branch manifests + append-only history (S18). Heads are stable typed keys
  * stored as named refs; every head update appends, never overwrites history.
  */
final case class BranchManifest(branch: String, head: Option[TypedKey], history: List[TypedKey]):
  def canon: Canon = Canon.cmap(
    "branch" -> Canon.CStr(branch),
    "head" -> head.fold(Canon.CTag("none", Canon.CInt(0)))(k => Canon.CTag("some", keyCanon(k))),
    "history" -> Canon.CList(history.map(keyCanon)))
  def artifact: Artifact = Artifact(ArtifactKind.BranchManifest, canon)
  private def keyCanon(k: TypedKey): Canon = Canon.cmap(
    "kind" -> Canon.CStr(k.kind.name),
    "value" -> Canon.CStr(k.valueHash.hex),
    "type" -> Canon.CStr(k.typeHash.hex))

object BranchManifest:
  def fromCanon(c: Canon): BranchManifest =
    import Canon.*
    def key(k: Canon): TypedKey = TypedKey(
      ArtifactKind.parse(k.field("kind").asStr).toOption.get,
      Digest(k.field("value").asStr), Digest(k.field("type").asStr))
    BranchManifest(
      c.field("branch").asStr,
      c.field("head") match
        case CTag("some", k) => Some(key(k))
        case _               => None,
      c.field("history").asList.map(key))

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
