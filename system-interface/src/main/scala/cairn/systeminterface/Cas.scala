package cairn.systeminterface

import cairn.kernel.*

/** Local content-addressed store (S4): the pure effect contract. Bodies are
  * canonical artifact bytes; keys are content digests. Local-first: the
  * working store, not the ledger (§4.9). MIGRATION-PLAN.md Phase 1: this is
  * the System Interface half of the old `workbench.Cas.scala` — the
  * concrete, effectful implementations (`MemCas`/`DiskCas`) live in
  * `system-handler` instead.
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
