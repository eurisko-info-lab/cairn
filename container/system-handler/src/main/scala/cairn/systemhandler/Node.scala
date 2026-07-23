package cairn.systemhandler

import cairn.kernel.*
import cairn.systeminterface.Cas
import cairn.systeminterface.Filesystem as Fs
import cairn.systeminterface.LedgerTransport as LT
import java.nio.file.Path

/** Single-node PoA ledger over a CAS directory (Phase 3 ledger-transport
  * family). Moved from `ledger.Node`. All validation delegates to the pure
  * [[LedgerKernel]].
  *
  * Constructed with an [[EffectContext]] for the gate; ledger append still
  * authenticates as the signing authority (`Subject(authority.name)`), not
  * the process-local subject — that identity is intrinsic to the seal.
  * CAS puts/gets on the node store go through [[CasEffects]]; the chain file
  * is read/written through [[Filesystem]] (use [[cairn.runtime.EffectContexts.forLedger]]).
  */
final class Node(val root: Path, val ctx: EffectContext):
  val cas: Cas = DiskCas(root)
  private val chainFile = root.resolve("chain")

  private def fsAbs(p: Path): Fs.Path = Fs.Path(p.toAbsolutePath.normalize.toString)

  private def fsErr(e: Fs.Error): String = e match
    case Fs.Error.NotFound(p) => s"not found: ${p.value}"
    case Fs.Error.Io(m)       => m

  private def fsRun(req: Fs.Request): Either[String, Fs.Response] =
    Filesystem.run(req, ctx).left.map(fsErr)

  /** Read the on-disk chain digest list (authorized FS read/exists). */
  private[systemhandler] def readChainDigests: Either[String, List[Digest]] =
    fsRun(Fs.Request.Exists(fsAbs(chainFile))) match
      case Right(Fs.Response.Bool(false)) => Right(Nil)
      case Right(Fs.Response.Bool(true)) =>
        fsRun(Fs.Request.Read(fsAbs(chainFile))) match
          case Right(Fs.Response.Text(s)) =>
            Right(s.linesIterator.map(_.trim).filter(_.nonEmpty).map(Digest(_)).toList)
          case Left(e)  => Left(e)
          case other    => Left(s"unexpected fs response: $other")
      case Left(e) => Left(e)
      case other   => Left(s"unexpected fs response: $other")

  /** Write the chain file (authorized FS mkdirs + write). */
  private[systemhandler] def writeChain(digests: List[Digest]): Either[String, Unit] =
    for
      _ <- fsRun(Fs.Request.Mkdirs(fsAbs(root)))
      _ <- fsRun(Fs.Request.Write(fsAbs(chainFile), digests.map(_.hex).mkString("", "\n", "\n")))
    yield ()

  def chainDigests: List[Digest] =
    readChainDigests.fold(e => throw RuntimeException(e), identity)

  def blocks: Either[String, List[Block]] =
    readChainDigests.flatMap { digests =>
      digests.foldLeft[Either[String, List[Block]]](Right(Nil)) { (acc, d) =>
        acc.flatMap { bs =>
          CasEffects.get(cas, d, ctx) match
            case Right(a) => Right(bs :+ Block.fromCanon(a.body))
            case Left(cairn.systeminterface.Cas.Error.Missing(_)) =>
              Left(s"blob ${d.short} not in CAS")
            case Left(cairn.systeminterface.Cas.Error.Io(m)) => Left(m)
        }
      }
    }

  def state(authorities: Map[String, Vector[Byte]]): Either[String, LedgerState] =
    blocks.flatMap(LedgerKernel.replay(authorities, _, Ed25519.verify))

  /** Seal and append a block of txs. Thin adapter over [[LedgerTransport.run]]. */
  def append(authority: Keypair, authorities: Map[String, Vector[Byte]], txs: List[SignedTx]): Either[String, Block] =
    LedgerTransport.run(this, authority, LT.Request.Append(authority.name, authorities, txs), ctx) match
      case Right(LT.Response.Sealed(block)) => Right(block)
      case Left(LT.Error.Denied(m))         => Left(m)
      case Left(LT.Error.Io(m))             => Left(m)

object Sync:
  /** Pull-based blob sync: copy blocks + published artifact bodies the
    * consumer is missing, by digest. Returns fetched digests.
    * Blob copies authorize through each node's [[CasEffects]] context;
    * the consumer chain file is written through [[Filesystem]] on [[to]].
    *
    * Any authorized CAS failure (`contains` / `getBytes` / `putBytes`) aborts
    * the pull — the consumer chain is not advanced with a partial blob set.
    */
  def pull(
      from: Node,
      to: Node,
      authorities: Map[String, Vector[Byte]],
      checkpoint: Option[BftFinality.FinalizedCheckpoint] = None,
  ): Either[String, List[Digest]] =
    def casErr(e: Cas.Error): String = e match
      case Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
      case Cas.Error.Io(m)      => m

    def fetchMissing(d: Digest, acc: List[Digest]): Either[String, List[Digest]] =
      CasEffects.contains(to.cas, d, to.ctx).left.map(casErr).flatMap {
        case true => Right(acc)
        case false =>
          for
            bs <- CasEffects.getBytes(from.cas, d, from.ctx).left.map(casErr)
            actual <- CasEffects.putBytes(to.cas, bs, to.ctx).left.map(casErr)
            _ <- Either.cond(actual == d, (), s"put digest mismatch for ${d.short}")
          yield d :: acc
      }

    for
      theirBlocks <- from.blocks
      theirChain = theirBlocks.map(_.digest)
      _ <- BftFinality.requireExtendsCheckpoint(theirChain, checkpoint)
      _ <- LedgerKernel.replay(authorities, theirBlocks, Ed25519.verify)
      st <- from.state(authorities)
      publishedDigests = st.published.toList.flatMap(_.split(":") match
        case Array(_, value, _) => Digest.parse(value).toOption
        case _                  => None)
      want = theirChain ++ publishedDigests
      fetched <- want.foldLeft[Either[String, List[Digest]]](Right(Nil)) { (acc, d) =>
        acc.flatMap(fetchMissing(d, _))
      }.map(_.reverse)
      _ <- to.writeChain(theirChain)
    yield fetched

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
