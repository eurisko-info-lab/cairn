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
  * a placeholder until grant bundles are threaded from Kernel validation.
  */
final case class EffectContext(
    subject: Subject,
    gate: AuthorityGate,
    capabilities: List[CapabilityGrant] = Nil,
    audit: EffectContext.Audit = EffectContext.Audit.Local,
):
  def withSubject(s: Subject): EffectContext = copy(subject = s)
  def withGate(g: AuthorityGate): EffectContext = copy(gate = g)

  /** Build an [[EffectRequest]] using this context's subject. */
  def effectRequest(
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty,
  ): EffectRequest =
    EffectRequest(subject, action, resource, args)

  /** Single authorize entry point: gate check → Kernel token → [[AuthorizedEffect]].
    * Narrow pack-loader, bootstrap allow-all, and Audit-mode pass-through all
    * flow through here.
    */
  def authorize(req: EffectRequest): Either[String, AuthorizedEffect] =
    gate.check(req).map(AuthorizedEffect.mint)

  /** Authorize using this context's subject. */
  def authorize(
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty,
  ): Either[String, AuthorizedEffect] =
    authorize(effectRequest(action, resource, args))

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

  /** Fresh allow-all Enforce gate + local subject (tests / non-pack-loader). */
  def bootstrapped(audit: Audit = Audit.Local): EffectContext =
    local(AuthorityGate.bootstrapped(), audit)

  /** PackLoader composition root: local subject + Enforce gate restricted to
    * [[PolicyEval.packLoaderWorkspace]] (workspace `read` under `languages*` only).
    */
  def forPackLoader(audit: Audit = Audit.Local): EffectContext =
    val subject = Subject("local")
    EffectContext(subject, AuthorityGate.enforcing(PolicyEval.packLoaderWorkspace(subject)), Nil, audit)
