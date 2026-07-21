package cairn.systeminterface

import cairn.kernel.Digest

/** Effect family: ledger persistence and transport (Phase 3). Pure transition
  * relation stays in `kernel.LedgerKernel`; this names the effectful ops.
  */
object LedgerTransport:
  enum Request:
    case LoadChain(root: Filesystem.Path)
    case AppendChain(root: Filesystem.Path, digests: List[Digest])
    case Pull(from: Filesystem.Path, to: Filesystem.Path)
    case GossipRound(peerRoots: List[(String, Filesystem.Path)])

  enum Response:
    case Chain(digests: List[Digest])
    case Fetched(digests: List[Digest])
    case Reorgs(events: List[String])

  enum Error:
    case Io(message: String)
    case Validation(message: String)
