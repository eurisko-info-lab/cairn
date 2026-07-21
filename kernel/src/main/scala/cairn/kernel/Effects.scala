package cairn.kernel

/** Unified effect vocabulary (Phase 3). Policies and Kernel authorization
  * reference [[ActionKey]]s derived from effect-interface artifacts
  * ([[EffectMeta]]); the closed [[Action]] enum remains a host bridge for
  * families that still have hand-tagged Scala cases (Ledger/Cas) and for
  * migration completeness checks.
  */
object Effects:
  enum Family:
    case Filesystem, Cas, Workspace, Process, Clock, Random,
         LedgerTransport, Terminal, Lsp, ExternalBackend

  /** Typed action key — family id + capability-class name.
    *
    * Derived from [[EffectMeta.EffectFamily]] action declarations for live
    * Meta-defined families; constructible from [[Action]] for host-only
    * rights (Ledger/Cas) during migration.
    */
  final case class ActionKey(family: String, name: String):
    def id: String = s"$family.$name"
    /** Host enum case, when one still exists for this key. */
    def toHost: Option[Action] =
      Action.values.find(a => a.family.toString == family && a.name == name)

  object ActionKey:
    def of(a: Action): ActionKey = ActionKey(a.family.toString, a.name)
    def of(family: Family, name: String): ActionKey = ActionKey(family.toString, name)

  /** Host-bridge action tags. Prefer [[ActionKey]] / [[EffectMeta]] at the
    * policy and EffectRequest boundary; keep these cases in sync via
    * [[EffectMeta.completeness]].
    */
  enum Action(val family: Family, val name: String):
    case FsRead           extends Action(Family.Filesystem, "read")
    case FsWrite          extends Action(Family.Filesystem, "write")
    case FsMkdirs         extends Action(Family.Filesystem, "mkdirs")
    case CasPut           extends Action(Family.Cas, "put")
    case CasGet           extends Action(Family.Cas, "get")
    case ProcessRun       extends Action(Family.Process, "run")
    case LedgerAppend     extends Action(Family.LedgerTransport, "append")
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

    def key: ActionKey = ActionKey.of(this)
