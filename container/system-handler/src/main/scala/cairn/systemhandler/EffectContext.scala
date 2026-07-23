package cairn.systemhandler

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
