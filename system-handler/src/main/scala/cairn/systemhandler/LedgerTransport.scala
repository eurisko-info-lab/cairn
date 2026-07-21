package cairn.systemhandler

import cairn.kernel.*
import cairn.systeminterface.LedgerTransport as LT
import java.nio.file.Files

/** Authorized ledger append — same authorize → [[AuthorizedEffect]] → perform
  * spine as Cas / Filesystem. [[Node.append]] is a thin adapter over [[run]].
  */
object LedgerTransport:
  private val iface = EffectMeta.ledgerTransport

  def intent(node: Node, req: LT.Request): (Effects.ActionKey, Authority.Resource) =
    req match
      case LT.Request.Append(_, _, _) =>
        (iface.actionKey("append"), iface.resource.at(node.root.toString))

  def run(
      node: Node,
      authority: Keypair,
      req: LT.Request,
      ctx: EffectContext,
  ): Either[LT.Error, LT.Response] =
    req match
      case LT.Request.Append(name, _, _) if name != authority.name =>
        Left(LT.Error.Denied(s"authority keypair '${authority.name}' does not match request '$name'"))
      case _ =>
        val (action, resource) = intent(node, req)
        ctx.withSubject(Authority.Subject(authority.name)).authorize(action, resource) match
          case Left(err)   => Left(LT.Error.Denied(err))
          case Right(auth) => perform(node, authority, req, auth)

  def perform(
      node: Node,
      authority: Keypair,
      req: LT.Request,
      auth: AuthorizedEffect,
  ): Either[LT.Error, LT.Response] =
    val (action, resource) = intent(node, req)
    if !auth.covers(action, resource) then Left(LT.Error.Denied("authorized effect does not cover request"))
    else req match
      case LT.Request.Append(_, authorities, txs) =>
        appendRaw(node, authority, authorities, txs) match
          case Left(err)    => Left(LT.Error.Io(err))
          case Right(block) => Right(LT.Response.Sealed(block))

  /** Kernel validation + CAS put of the sealed block (CAS via [[CasEffects]]). */
  private[systemhandler] def appendRaw(
      node: Node,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
      txs: List[SignedTx],
  ): Either[String, Block] =
    for
      bs <- node.blocks
      st <- LedgerKernel.replay(authorities, bs, Ed25519.verify)
      parent = bs.lastOption.map(_.digest).getOrElse(LedgerKernel.genesisParent)
      height = bs.length.toLong
      after <- txs.foldLeft[Either[String, LedgerState]](Right(st)) { (acc, stx) =>
        acc.flatMap(LedgerKernel.applyTx(_, stx, Ed25519.verify)) }
      unsealed = Block(height, parent, txs, after.root, authority.name, Vector.empty)
      seal = Ed25519.sign(authority.privateKey, Canon.encode(unsealed.unsealedCanon))
      block = unsealed.copy(seal = seal)
      _ <- LedgerKernel.applyBlock(st, parent, height, authorities, block, Ed25519.verify)
      _ <- CasEffects.put(node.cas, block.artifact, node.ctx).left.map {
        case cairn.systeminterface.Cas.Error.Io(m)      => m
        case cairn.systeminterface.Cas.Error.Missing(d) => s"missing ${d.short}"
      }
    yield
      Files.createDirectories(node.root)
      Files.writeString(node.root.resolve("chain"),
        (node.chainDigests :+ block.digest).map(_.hex).mkString("", "\n", "\n"))
      block
