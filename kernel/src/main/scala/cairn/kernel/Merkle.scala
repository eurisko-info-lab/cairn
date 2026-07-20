package cairn.kernel

/** M35: Merkle-tree commitments with inclusion proofs. Leaves are sorted
  * (key, value) pairs; inner nodes hash left||right; odd nodes promote.
  * Light clients verify "key K has value V under root R" without the state.
  */
object Merkle:
  final case class ProofStep(sibling: Digest, siblingOnLeft: Boolean)
  final case class Proof(leafKey: String, leafValue: Canon, steps: List[ProofStep])

  def leafHash(key: String, value: Canon): Digest =
    Digest.of(Canon.cmap("k" -> Canon.CStr(key), "v" -> value))

  private def combine(a: Digest, b: Digest): Digest =
    Digest.of(Canon.cmap("l" -> Canon.CStr(a.hex), "r" -> Canon.CStr(b.hex)))

  val emptyRoot: Digest = Digest.of(Canon.CStr("merkle-empty"))

  def root(entries: List[(String, Canon)]): Digest =
    val sorted = entries.sortBy(_._1)
    if sorted.isEmpty then emptyRoot
    else
      var level = sorted.map((k, v) => leafHash(k, v))
      while level.length > 1 do
        level = level.grouped(2).map {
          case List(a, b) => combine(a, b)
          case List(a)    => a
          case g          => sys.error(s"grouped(2) yielded ${g.size}")
        }.toList
      level.head

  def prove(entries: List[(String, Canon)], key: String): Either[String, Proof] =
    val sorted = entries.sortBy(_._1)
    val idx = sorted.indexWhere(_._1 == key)
    if idx < 0 then Left(s"key '$key' not present")
    else
      var level = sorted.map((k, v) => leafHash(k, v))
      var pos = idx
      val steps = List.newBuilder[ProofStep]
      while level.length > 1 do
        val sibling = if pos % 2 == 0 then pos + 1 else pos - 1
        if sibling < level.length then
          steps += ProofStep(level(sibling), siblingOnLeft = sibling < pos)
        level = level.grouped(2).map {
          case List(a, b) => combine(a, b)
          case List(a)    => a
          case g          => sys.error(s"grouped(2) yielded ${g.size}")
        }.toList
        pos = pos / 2
      Right(Proof(key, sorted(idx)._2, steps.result()))

  def verify(expectedRoot: Digest, proof: Proof): Boolean =
    val start = leafHash(proof.leafKey, proof.leafValue)
    val computed = proof.steps.foldLeft(start) { (acc, step) =>
      if step.siblingOnLeft then combine(step.sibling, acc) else combine(acc, step.sibling) }
    computed == expectedRoot
