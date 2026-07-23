package cairn.surface

import cairn.runtime.PackLoader
import cairn.systemhandler.*
import java.nio.file.Path

/** Porcelain: user-facing CLI verbs that compose [[Plumbing]].
  *
  * Usage (via `cairn …`):
  * {{{
  *   cairn chain status|export|compare
  *   cairn auth check <subject>
  *   cairn branch list
  *   cairn domain show
  *   cairn compose status <lang>
  *   cairn catalog export
  *   cairn workflow list
  *   cairn recover
  *   cairn replay snapshot
  *   cairn tx state
  *   cairn light verify
  *   cairn porcelain <charb-theme>   # promote a deferred Charb name
  * }}}
  */
object Porcelain:

  /** Charb theme names that have a plumbing mapping today. */
  val promotedThemes: Set[String] = Set(
    "authorization", "strict-assumptions-governance",
    "branch-variant-registry",
    "catalog-export", "key-sbom-capability-registry",
    "chain-json-export", "governance-kernel-introspection",
    "chain-recovery-negative", "consistency-batch-mixed-failures",
    "chain-repair", "recovery-suggestions", "strict-governance-recovery-negative",
    "compose-registry",
    "integration-cross-namespace",
    "interop-lightclient-phase4",
    "replay-history-diff", "replay-history-switch-and-diff", "replay-root-switching",
    "audit-registry", "audit-mismatch",
    "runner-registry", "runner-selection", "workflow-registry", "distributed",
    "strict-governance-publish-integration", "governance-real-features",
    "tx-state-phase2",
    "incentive-positive",
    "chain-quarantine",
    "federation-registry",
    "governance-supplychain",
    "mirror-registry",
    "object-run-commit-registry",
  )

  def env(
      home: Path,
      packLoader: PackLoader,
      ledgerCtx: EffectContext,
  ): Plumbing.Env =
    val cas = DiskCas(home)
    val kp = Keypair.dev("porcelain-dev")
    val authorities = Map(kp.name -> kp.publicBytes)
    val node = Node(home.resolve("nodeA"), ledgerCtx)
    val branches = Branches(cas, home.resolve("refs"), EffectContext.forBranches())
    val replay = ReplayStore.memory()
    Plumbing.Env(node, branches, home, authorities, packLoader, replay)

  def dispatch(
      args: List[String],
      home: Path,
      packLoader: PackLoader,
      ledgerCtx: EffectContext,
  ): Either[String, String] =
    val e = env(home, packLoader, ledgerCtx)
    args match
      case List("chain", "status")  => Plumbing.chainStatus(e.node, e.authorities)
      case List("chain", "export")  => Right(Plumbing.chainExport(e.node))
      case List("chain", "compare") => Right(Plumbing.chainCompare(e.node, e.node))
      case List("auth", "check")    => Right(Plumbing.authCheck("alice"))
      case List("auth", "check", sub) => Right(Plumbing.authCheck(sub))
      case List("branch", "list") | List("branch") => Right(Plumbing.branchList(e.branches))
      case List("domain", "show") | List("domain") => Right(Plumbing.domainShow(e.branches))
      case List("compose", "status") => Plumbing.composeStatus(e.packLoader, "stlc")
      case List("compose", "status", lang) => Plumbing.composeStatus(e.packLoader, lang)
      case List("catalog", "export") | List("catalog") => Right(Plumbing.catalogExport(e.packLoader))
      case List("workflow", "list") | List("workflow") => Plumbing.workflowList(e.packLoader)
      case List("recover") => Plumbing.recover(e.branches, e.casRoot)
      case List("replay", "snapshot") | List("replay") => Right(Plumbing.replaySnapshot(e.replay))
      case List("tx", "state") | List("tx") => Plumbing.txState(e.node, e.authorities)
      case List("light", "verify") | List("light") => Plumbing.lightVerify(e.node, e.authorities)
      case "porcelain" :: theme :: Nil => Plumbing.charbTheme(theme, e)
      case other =>
        Left(
          s"""usage: cairn chain|auth|branch|domain|compose|catalog|workflow|recover|replay|tx|light|porcelain …
             |  got: ${other.mkString(" ")}
             |promoted Charb themes: ${promotedThemes.toList.sorted.mkString(", ")}""".stripMargin)
