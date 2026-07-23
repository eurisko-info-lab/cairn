package cairn.runtime

import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.kernel.Authority.{Subject, VerifiedCapability}
import cairn.systemhandler.{AuthorityGate, EffectContext, RevocationView, RuntimeEffectRegistry}
import cairn.systemhandler.EffectContext.Audit

/** Composition-root factories for [[EffectContext]] that decide policies via
  * Core [[PolicyEval]]. Kept out of `system-handler` so the container side
  * never has to import `cairn.core` for this — see [[EffectContext.local]] /
  * [[EffectContext.bootstrapped]] for the policy-free factories that stay
  * there.
  */
object EffectContexts:
  /** PackLoader composition root: local subject + Enforce gate restricted to
    * [[PolicyEval.packLoaderWorkspace]] (workspace `read` under `languages*` only).
    * Optional [[capabilities]] grant bundle takes precedence in [[authorize]].
    */
  def forPackLoader(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    val subject = Subject("local")
    val policies = RuntimeEffectRegistry.rebind(
      PolicyEval.packLoaderWorkspace(subject), registry)
    EffectContext(
      subject = subject,
      gate = AuthorityGate.enforcing(policies, revocation),
      capabilities = capabilities,
      audit = audit,
      clock = () => System.currentTimeMillis(),
      registry = registry,
      revocation = revocation)

  /** Ledger composition root: append under any ledger root (subject `*` so
    * signing authorities authorize), local CAS put/get, and chain-file FS
    * (read/write/mkdirs) for the node store.
    */
  def forLedger(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    localCtx(PolicyEval.ledgerWithCas(), audit, capabilities, registry, revocation)

  /** Process composition root. Default command allow-list matches live
    * transcript / port runners (`scala-cli`, `cargo`, `runghc`, `lake`, `git`).
    * Pass `commands = List("*")` only for explicit allow-all fixtures.
    */
  def forProcess(
      audit: Audit = Audit.Local,
      commands: List[String] = List("scala-cli", "cargo", "runghc", "lake", "git"),
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    localCtx(PolicyEval.processRunner(Subject("local"), commands), audit, capabilities, registry, revocation)

  /** LSP composition root: session read/write. */
  def forLsp(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    localCtx(PolicyEval.lspSession(Subject("local")), audit, capabilities, registry, revocation)

  /** External-backend composition root: find/run listed hosts. */
  def forBackend(
      audit: Audit = Audit.Local,
      hosts: List[String] = List("scala-cli", "cargo", "runghc", "lake"),
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    localCtx(PolicyEval.externalBackend(Subject("local"), hosts), audit, capabilities, registry, revocation)

  /** CAS composition root: put/get/stats plus admin fsck/gc. */
  def forCas(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    localCtx(
      PolicyEval.casStore(Subject("local")) ++ PolicyEval.casAdmin(Subject("local")),
      audit, capabilities, registry, revocation)

  /** Branches composition root: CAS store + refs-directory filesystem. */
  def forBranches(
      refsPathPattern: String = "*",
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    localCtx(
      PolicyEval.branchesStore(Subject("local"), refsPathPattern) ++
        PolicyEval.casAdmin(Subject("local")),
      audit, capabilities, registry, revocation)

  /** Filesystem composition root: read/write/mkdirs under a path pattern. */
  def forFilesystem(
      pathPattern: String = "*",
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    localCtx(PolicyEval.filesystemStore(Subject("local"), pathPattern), audit, capabilities, registry, revocation)

  private def localCtx(
      policies: List[Authority.EffectPolicy],
      audit: Audit,
      capabilities: List[VerifiedCapability] = Nil,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    val rebound = RuntimeEffectRegistry.rebind(policies, registry)
    EffectContext(
      subject = Subject("local"),
      gate = AuthorityGate.enforcing(rebound, revocation),
      capabilities = capabilities,
      audit = audit,
      clock = () => System.currentTimeMillis(),
      registry = registry,
      revocation = revocation)
