package cairn.kernel

/** Unified effect vocabulary (Phase 3). Policies and Kernel authorization
  * reference these tags; handlers implement per-family ops.
  */
object Effects:
  enum Family:
    case Filesystem, Cas, Workspace, Process, Crypto, Clock, Random,
         Network, Http, LedgerTransport, Terminal, Lsp, ExternalBackend

  /** Typed action names used by authority (Phase 4+). */
  enum Action(val family: Family, val name: String):
    case FsRead           extends Action(Family.Filesystem, "read")
    case FsWrite          extends Action(Family.Filesystem, "write")
    case FsMkdirs         extends Action(Family.Filesystem, "mkdirs")
    case CasPut           extends Action(Family.Cas, "put")
    case CasGet           extends Action(Family.Cas, "get")
    case ProcessRun       extends Action(Family.Process, "run")
    case CryptoSign       extends Action(Family.Crypto, "sign")
    case CryptoVerify     extends Action(Family.Crypto, "verify")
    case CryptoSeal       extends Action(Family.Crypto, "seal")
    case CryptoOpen       extends Action(Family.Crypto, "open")
    case CryptoGenerate   extends Action(Family.Crypto, "generate")
    case NetworkGet       extends Action(Family.Network, "get")
    case HttpServe        extends Action(Family.Http, "serve")
    case HttpGet          extends Action(Family.Http, "get")
    case LedgerAppend     extends Action(Family.LedgerTransport, "append")
    case LedgerPull       extends Action(Family.LedgerTransport, "pull")
    case LedgerPublish    extends Action(Family.LedgerTransport, "publish")
    case BranchAdvance    extends Action(Family.LedgerTransport, "branch-advance")
    case ClockNow         extends Action(Family.Clock, "now")
    case ClockTimestampSlug extends Action(Family.Clock, "timestampSlug")
    case RandomBytes      extends Action(Family.Random, "bytes")
    case TerminalWrite    extends Action(Family.Terminal, "write")
    case TerminalRead     extends Action(Family.Terminal, "read")
    case LspRead          extends Action(Family.Lsp, "read")
    case LspWrite         extends Action(Family.Lsp, "write")
    case BackendRun       extends Action(Family.ExternalBackend, "run")
    case BackendFind      extends Action(Family.ExternalBackend, "find")
    case WorkspaceRead    extends Action(Family.Workspace, "read")
