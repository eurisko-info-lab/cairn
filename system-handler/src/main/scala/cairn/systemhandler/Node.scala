package cairn.systemhandler

import cairn.kernel.*
import cairn.systeminterface.Cas
import java.nio.file.{Files, Path}

/** Single-node PoA ledger over a CAS directory (Phase 3 ledger-transport
  * family). Moved from `ledger.Node`. All validation delegates to the pure
  * [[LedgerKernel]].
  *
  * Constructed with an [[EffectContext]] for the gate; ledger append still
  * authenticates as the signing authority (`Subject(authority.name)`), not
  * the process-local subject — that identity is intrinsic to the seal.
  */
final class Node(val root: Path, ctx: EffectContext):
  val cas: Cas = DiskCas(root)
  private val chainFile = root.resolve("chain")

  def chainDigests: List[Digest] =
    if !Files.exists(chainFile) then Nil
    else Files.readAllLines(chainFile).toArray.toList.map(l => Digest(l.toString.trim)).filter(_ => true)

  def blocks: Either[String, List[Block]] =
    chainDigests.foldLeft[Either[String, List[Block]]](Right(Nil)) { (acc, d) =>
      acc.flatMap(bs => cas.getByDigest(d).map(a => bs :+ Block.fromCanon(a.body))) }

  def state(authorities: Map[String, Vector[Byte]]): Either[String, LedgerState] =
    blocks.flatMap(LedgerKernel.replay(authorities, _, Ed25519.verify))

  /** Seal and append a block of txs. Atomic under kernel validation.
    * Phase 5: ledger append is authorized via [[EffectContext.authorize]]
    * (seal identity = `Subject(authority.name)`, not the process-local subject).
    */
  def append(authority: Keypair, authorities: Map[String, Vector[Byte]], txs: List[SignedTx]): Either[String, Block] =
    val req = Authority.EffectRequest(
      Authority.Subject(authority.name),
      Effects.Action.LedgerAppend,
      Authority.Resource("ledger", root.toString))
    ctx.authorize(req).flatMap { _ =>
      for
        bs <- blocks
        st <- LedgerKernel.replay(authorities, bs, Ed25519.verify)
        parent = bs.lastOption.map(_.digest).getOrElse(LedgerKernel.genesisParent)
        height = bs.length.toLong
        after <- txs.foldLeft[Either[String, LedgerState]](Right(st)) { (acc, stx) =>
          acc.flatMap(LedgerKernel.applyTx(_, stx, Ed25519.verify)) }
        unsealed = Block(height, parent, txs, after.root, authority.name, Vector.empty)
        seal = Ed25519.sign(authority.privateKey, Canon.encode(unsealed.unsealedCanon))
        block = unsealed.copy(seal = seal)
        _ <- LedgerKernel.applyBlock(st, parent, height, authorities, block, Ed25519.verify)
      yield
        cas.put(block.artifact)
        Files.createDirectories(root)
        Files.writeString(chainFile, (chainDigests :+ block.digest).map(_.hex).mkString("", "\n", "\n"))
        block
    }

object Sync:
  /** Pull-based blob sync: copy blocks + published artifact bodies the
    * consumer is missing, by digest. Returns fetched digests.
    */
  def pull(from: Node, to: Node, authorities: Map[String, Vector[Byte]]): Either[String, List[Digest]] =
    for
      theirBlocks <- from.blocks
      _ <- LedgerKernel.replay(authorities, theirBlocks, Ed25519.verify)
      st <- from.state(authorities)
    yield
      val fetched = List.newBuilder[Digest]
      def fetch(d: Digest): Unit =
        if !to.cas.contains(d) then
          from.cas.getBytes(d).foreach { bs => to.cas.putBytes(bs); fetched += d }
      theirBlocks.foreach(b => fetch(b.digest))
      st.published.foreach { render =>
        render.split(":") match
          case Array(_, value, _) => Digest.parse(value).foreach(fetch)
          case _                  => () }
      Files.writeString(to.root.resolve("chain"), theirBlocks.map(_.digest.hex).mkString("", "\n", "\n"))
      fetched.result()

  enum Comparison:
    case Same
    case Ahead(by: Int)
    case Behind(by: Int)
    case Diverged(atHeight: Int, mineHead: Digest, otherHead: Digest)

  def compare(mine: List[Digest], other: List[Digest]): Comparison =
    val common = mine.zip(other).takeWhile(_ == _).length
    if common == mine.length && common == other.length then Comparison.Same
    else if common == mine.length then Comparison.Ahead(other.length - common)
    else if common == other.length then Comparison.Behind(mine.length - common)
    else Comparison.Diverged(common, mine.last, other.last)
