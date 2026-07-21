package cairn.systemhandler

import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.kernel.Authority.{CapabilityGrant, EffectRequest, Resource, Subject}
import cairn.kernel.Effects

/** Explicit effect-execution context. Composition roots construct this and
  * pass it into [[authorize]]; handlers must not invent the caller's identity
  * and must not hold a gate — they accept only [[AuthorizedEffect]].
  *
  * Carries [[subject]] + [[gate]] for the authorize step; [[capabilities]] is
  * consulted first when non-empty (covering grant → Kernel check, no broad
  * policy re-eval); [[clock]] supplies injectable time for grant expiry.
  */
final case class EffectContext(
    subject: Subject,
    gate: AuthorityGate,
    capabilities: List[CapabilityGrant] = Nil,
    audit: EffectContext.Audit = EffectContext.Audit.Local,
    clock: () => Long = System.currentTimeMillis,
):
  def withSubject(s: Subject): EffectContext = copy(subject = s)
  def withGate(g: AuthorityGate): EffectContext = copy(gate = g)
  def withClock(c: () => Long): EffectContext = copy(clock = c)
  def withCapabilities(caps: List[CapabilityGrant]): EffectContext = copy(capabilities = caps)

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
    * When [[capabilities]] is non-empty: find a covering grant → Kernel
    * [[Authority.checkCapability]] (no policy prove). When empty: fall back
    * to Core prove → Kernel checkProof via the gate.
    */
  def authorize(req: EffectRequest): Either[String, AuthorizedEffect] =
    val now = clock()
    if capabilities.nonEmpty then
      capabilities.find(_.covers(req, now)) match
        case Some(grant) => gate.checkCapability(req, grant, now).map(AuthorizedEffect.mint)
        case None        => Left("no covering capability in context")
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
    */
  def forPackLoader(audit: Audit = Audit.Local): EffectContext =
    val subject = Subject("local")
    EffectContext(subject, AuthorityGate.enforcing(PolicyEval.packLoaderWorkspace(subject)), Nil, audit)

  /** Ledger composition root: append under any ledger root (subject `*` so
    * signing authorities authorize), local CAS put/get, and chain-file FS
    * (read/write/mkdirs) for the node store.
    */
  def forLedger(audit: Audit = Audit.Local): EffectContext =
    localCtx(PolicyEval.ledgerWithCas(), audit)

  /** Process composition root: run any command (deployment may narrow further). */
  def forProcess(audit: Audit = Audit.Local): EffectContext =
    localCtx(PolicyEval.processRunner(Subject("local")), audit)

  /** LSP composition root: session read/write. */
  def forLsp(audit: Audit = Audit.Local): EffectContext =
    localCtx(PolicyEval.lspSession(Subject("local")), audit)

  /** External-backend composition root: find/run any host. */
  def forBackend(audit: Audit = Audit.Local): EffectContext =
    localCtx(PolicyEval.externalBackend(Subject("local")), audit)

  /** CAS composition root: put/get/stats plus admin fsck/gc. */
  def forCas(audit: Audit = Audit.Local): EffectContext =
    localCtx(PolicyEval.casStore(Subject("local")) ++ PolicyEval.casAdmin(Subject("local")), audit)

  /** Branches composition root: CAS store + refs-directory filesystem. */
  def forBranches(refsPathPattern: String = "*", audit: Audit = Audit.Local): EffectContext =
    localCtx(
      PolicyEval.branchesStore(Subject("local"), refsPathPattern) ++
        PolicyEval.casAdmin(Subject("local")),
      audit)

  /** Filesystem composition root: read/write/mkdirs under a path pattern. */
  def forFilesystem(pathPattern: String = "*", audit: Audit = Audit.Local): EffectContext =
    localCtx(PolicyEval.filesystemStore(Subject("local"), pathPattern), audit)

  private def localCtx(policies: List[Authority.EffectPolicy], audit: Audit): EffectContext =
    EffectContext(Subject("local"), AuthorityGate.enforcing(policies), Nil, audit)
