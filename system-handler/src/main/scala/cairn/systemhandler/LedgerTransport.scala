package cairn.systemhandler

import cairn.kernel.*
import cairn.systeminterface.LedgerTransport as LT

/** Authorized ledger append — same authorize → [[AuthorizedEffect]] → perform
  * spine as Cas / Filesystem. [[Node.append]] is a thin adapter over [[run]].
  * Chain-file write goes through [[Filesystem]] on the node context.
  */
object LedgerTransport:
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.LedgerTransport)

  def intent(node: Node, req: LT.Request, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    val i = iface(registry)
    req match
      case LT.Request.Append(_, _, _) =>
        (i.actionKey("append"), i.resource.at(node.root.toString))

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
        val (action, resource) = intent(node, req, ctx.registry)
        ctx.withSubject(Authority.Subject(authority.name)).authorize(action, resource) match
          case Left(err)   => Left(LT.Error.Denied(err))
          case Right(auth) => perform(node, authority, req, auth, ctx.registry)

  def perform(
      node: Node,
      authority: Keypair,
      req: LT.Request,
      auth: AuthorizedEffect,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
  ): Either[LT.Error, LT.Response] =
    val (action, resource) = intent(node, req, registry)
    if !auth.covers(action, resource) then Left(LT.Error.Denied("authorized effect does not cover request"))
    else req match
      case LT.Request.Append(_, authorities, txs) =>
        appendRaw(node, authority, authorities, txs) match
          case Left(err)    => Left(LT.Error.Io(err))
          case Right(block) => Right(LT.Response.Sealed(block))

  /** Kernel validation + CAS put of the sealed block (CAS via [[CasEffects]]);
    * chain-file append via [[Filesystem]].
    */
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
      _ <- node.writeChain(bs.map(_.digest) :+ block.digest)
    yield block
