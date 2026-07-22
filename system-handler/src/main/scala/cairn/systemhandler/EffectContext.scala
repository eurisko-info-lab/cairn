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
  * [[registry]] is the live effect-interface vocabulary (disk-loaded via
  * EffectBootstrap or cold-start seeds). Handlers resolve ActionKeys /
  * ResourceSchemas through it.
  *
  * [[revocation]] is consulted on every capability authorize path.
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
    registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
    revocation: RevocationView = RevocationView.empty,
):
  def withSubject(s: Subject): EffectContext = copy(subject = s)
  def withGate(g: AuthorityGate): EffectContext = copy(gate = g)
  def withClock(c: () => Long): EffectContext = copy(clock = c)
  def withCapabilities(caps: List[VerifiedCapability]): EffectContext = copy(capabilities = caps)
  def withRegistry(r: RuntimeEffectRegistry): EffectContext = copy(registry = r)
  def withRevocation(v: RevocationView): EffectContext = copy(revocation = v)

  /** Build an [[EffectRequest]] using this context's subject. */
  def effectRequest(
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty,
      requestId: Option[String] = None,
  ): EffectRequest =
    EffectRequest(subject, action, resource, args, requestId)

  /** Single authorize entry point — Enforce only; yields [[AuthorizedEffect]].
    *
    * When [[capabilities]] is non-empty: find a covering verified grant →
    * Kernel [[Authority.checkCapability]] with [[revocation]] (no policy prove).
    * When empty: fall back to Core prove → Kernel checkProof via the gate.
    *
    * Audit mode cannot produce [[AuthorizedEffect]] (use [[recordAudit]]).
    */
  def authorize(req: EffectRequest): Either[String, AuthorizedEffect] =
    val now = clock()
    if capabilities.nonEmpty then
      capabilities.find(_.covers(req, now)) match
        case Some(cap) =>
          // Prefer context revocation; gate also carries a view for direct calls.
          val view =
            if revocation ne RevocationView.empty then revocation
            else gate.revocationView
          if view.isRevoked(cap.capabilityId) then
            Left(s"grant revoked: ${cap.capabilityId}")
          else
            gate.checkCapability(req, cap, now).map(AuthorizedEffect.mint)
        case None => Left("no covering capability in context")
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

  /** Audit-mode recording — returns [[AuditedEffect]], never Authorized. */
  def recordAudit(req: EffectRequest): AuditedEffect =
    AuditedEffect.mint(gate.audit(req, clock()))

  def recordAudit(
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty,
      requestId: Option[String] = None,
  ): AuditedEffect =
    recordAudit(effectRequest(action, resource, args, requestId))

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
  def local(
      gate: AuthorityGate,
      audit: Audit = Audit.Local,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    EffectContext(
      subject = Subject("local"),
      gate = gate,
      capabilities = Nil,
      audit = audit,
      clock = () => System.currentTimeMillis(),
      registry = registry,
      revocation = revocation)

  /** Fresh allow-all Enforce gate + local subject (tests / broad wiring). */
  def bootstrapped(
      audit: Audit = Audit.Local,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): EffectContext =
    local(AuthorityGate.bootstrapped(registry, revocation), audit, registry, revocation)

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
