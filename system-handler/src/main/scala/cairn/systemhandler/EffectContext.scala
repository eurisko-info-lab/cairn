package cairn.systemhandler

import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.kernel.Authority.{EffectRequest, Resource, Subject, VerifiedCapability}
import cairn.kernel.Effects

/** Explicit effect-execution context. Composition roots construct this and
  * pass it into [[authorize]]; handlers must not invent the caller's identity
  * and must not hold a gate — they accept only [[AuthorizedEffect]].
  *
  * Carries [[subject]] + [[gate]] for the authorize step; [[capabilities]] is
  * consulted first when non-empty (covering [[VerifiedCapability]] → Kernel
  * check, no broad policy re-eval); [[clock]] supplies injectable time for
  * grant expiry.
  *
  * Capabilities must be Kernel-minted via [[VerifiedCapability.fromProof]];
  * raw [[CapabilityGrant]] values are not accepted by [[withCapabilities]].
  * Replay tokens are consumed on the gate's issuer-scoped [[ReplayStore]]
  * (in-memory by default; durable filesystem stores may be shared).
  */
final case class EffectContext(
    subject: Subject,
    gate: AuthorityGate,
    capabilities: List[VerifiedCapability] = Nil,
    audit: EffectContext.Audit = EffectContext.Audit.Local,
    clock: () => Long = System.currentTimeMillis,
):
  def withSubject(s: Subject): EffectContext = copy(subject = s)
  def withGate(g: AuthorityGate): EffectContext = copy(gate = g)
  def withClock(c: () => Long): EffectContext = copy(clock = c)
  def withCapabilities(caps: List[VerifiedCapability]): EffectContext = copy(capabilities = caps)

  /** Build an [[EffectRequest]] using this context's subject. */
  def effectRequest(
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty,
      requestId: Option[String] = None,
  ): EffectRequest =
    EffectRequest(subject, action, resource, args, requestId)

  /** Single authorize entry point.
    *
    * When [[capabilities]] is non-empty: find a covering verified grant →
    * Kernel [[Authority.checkCapability]] (no policy prove). When empty: fall
    * back to Core prove → Kernel checkProof via the gate.
    */
  def authorize(req: EffectRequest): Either[String, AuthorizedEffect] =
    val now = clock()
    if capabilities.nonEmpty then
      capabilities.find(_.covers(req, now)) match
        case Some(cap) => gate.checkCapability(req, cap.grant, now).map(AuthorizedEffect.mint)
        case None      => Left("no covering capability in context")
    else
      gate.check(req, now).map(AuthorizedEffect.mint)

  /** Authorize using this context's subject. */
  def authorize(
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty,
      requestId: Option[String] = None,
  ): Either[String, AuthorizedEffect] =
    authorize(effectRequest(action, resource, args, requestId))

object EffectContext:
  /** Audit-trail identity. Distinct from authorization [[Subject]] when a
    * composition root wants a separate correlation label; defaults to
    * `"local"` for single-process wiring.
    */
  final case class Audit(identity: String)

  object Audit:
    val Local: Audit = Audit("local")

  /** Local-process context over an existing gate. Subject is `"local"` —
    * the historical placeholder, now owned by the composition root rather
    * than invented inside handlers.
    */
  def local(gate: AuthorityGate, audit: Audit = Audit.Local): EffectContext =
    EffectContext(Subject("local"), gate, Nil, audit)

  /** Fresh allow-all Enforce gate + local subject (tests / broad wiring). */
  def bootstrapped(audit: Audit = Audit.Local): EffectContext =
    local(AuthorityGate.bootstrapped(), audit)

  /** PackLoader composition root: local subject + Enforce gate restricted to
    * [[PolicyEval.packLoaderWorkspace]] (workspace `read` under `languages*` only).
    * Optional [[capabilities]] grant bundle takes precedence in [[authorize]].
    */
  def forPackLoader(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    val subject = Subject("local")
    EffectContext(
      subject,
      AuthorityGate.enforcing(PolicyEval.packLoaderWorkspace(subject)),
      capabilities,
      audit)

  /** Ledger composition root: append under any ledger root (subject `*` so
    * signing authorities authorize), local CAS put/get, and chain-file FS
    * (read/write/mkdirs) for the node store.
    */
  def forLedger(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    localCtx(PolicyEval.ledgerWithCas(), audit, capabilities)

  /** Process composition root. Default command allow-list matches live
    * transcript / port runners (`scala-cli`, `cargo`, `runghc`, `lake`, `git`).
    * Pass `commands = List("*")` only for explicit allow-all fixtures.
    */
  def forProcess(
      audit: Audit = Audit.Local,
      commands: List[String] = List("scala-cli", "cargo", "runghc", "lake", "git"),
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    localCtx(PolicyEval.processRunner(Subject("local"), commands), audit, capabilities)

  /** LSP composition root: session read/write. */
  def forLsp(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    localCtx(PolicyEval.lspSession(Subject("local")), audit, capabilities)

  /** External-backend composition root: find/run listed hosts. */
  def forBackend(
      audit: Audit = Audit.Local,
      hosts: List[String] = List("scala-cli", "cargo", "runghc", "lake"),
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    localCtx(PolicyEval.externalBackend(Subject("local"), hosts), audit, capabilities)

  /** CAS composition root: put/get/stats plus admin fsck/gc. */
  def forCas(
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    localCtx(
      PolicyEval.casStore(Subject("local")) ++ PolicyEval.casAdmin(Subject("local")),
      audit,
      capabilities)

  /** Branches composition root: CAS store + refs-directory filesystem. */
  def forBranches(
      refsPathPattern: String = "*",
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    localCtx(
      PolicyEval.branchesStore(Subject("local"), refsPathPattern) ++
        PolicyEval.casAdmin(Subject("local")),
      audit,
      capabilities)

  /** Filesystem composition root: read/write/mkdirs under a path pattern. */
  def forFilesystem(
      pathPattern: String = "*",
      audit: Audit = Audit.Local,
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    localCtx(PolicyEval.filesystemStore(Subject("local"), pathPattern), audit, capabilities)

  private def localCtx(
      policies: List[Authority.EffectPolicy],
      audit: Audit,
      capabilities: List[VerifiedCapability] = Nil,
  ): EffectContext =
    EffectContext(Subject("local"), AuthorityGate.enforcing(policies), capabilities, audit)
