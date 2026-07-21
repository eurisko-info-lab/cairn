package cairn.systeminterface

import cairn.kernel.*

/** Ledger transport append — pure Request/Response/Error contract mirroring
  * [[cairn.kernel.EffectMeta.ledgerTransport]]. Handlers authorize →
  * [[cairn.systemhandler.AuthorizedEffect]] → perform; sealing keys stay in
  * the handler (not in this contract).
  */
object LedgerTransport:
  enum Request:
    /** Append sealed txs as the named authority (handler supplies the keypair). */
    case Append(authorityName: String, authorities: Map[String, Vector[Byte]], txs: List[SignedTx])

  enum Response:
    case Sealed(block: Block)

  enum Error:
    case Denied(message: String)
    case Io(message: String)
