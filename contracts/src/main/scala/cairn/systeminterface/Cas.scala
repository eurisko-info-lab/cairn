package cairn.systeminterface

import cairn.kernel.*

/** Local content-addressed store (S4): the pure effect contract. Bodies are
  * canonical artifact bytes; keys are content digests. Local-first: the
  * working store, not the ledger (§4.9). MIGRATION-PLAN.md Phase 1: this is
  * the System Interface half of the old `workbench.Cas.scala` — the
  * concrete, effectful implementations (`MemCas`/`DiskCas`) live in
  * `system-handler` instead.
  *
  * [[Request]] / [[Response]] / [[Error]] mirror [[cairn.kernel.EffectMeta.cas]]
  * so handlers can authorize → [[cairn.systemhandler.AuthorizedEffect]] →
  * perform, same spine as Filesystem / Workspace.
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

object Cas:
  enum Request:
    case Put(artifact: Artifact)
    case Get(digest: Digest)
    case Contains(digest: Digest)
    case Fsck(root: String)
    case Gc(root: String, roots: List[Digest])
    case Stats(root: String)

  /** Response ctors mirror Meta (`typedKey` / `artifact` / `bool` / admin
    * reports) by role; Scala names avoid shadowing [[cairn.kernel.TypedKey]] /
    * [[Artifact]]. Admin report payloads are opaque host values. */
  enum Response:
    case Key(key: TypedKey)
    case Stored(artifact: Artifact)
    case Present(present: Boolean)
    case FsckReport(checked: Int, corrupt: List[Digest])
    case GcReport(kept: Int, swept: Int)
    case StatsReport(objects: Int, bytes: Long, byKind: Map[String, Int])

  enum Error:
    case Missing(digest: Digest)
    case Io(message: String)
