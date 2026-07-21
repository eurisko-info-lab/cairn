package cairn.systemhandler

import cairn.kernel.Authority
import cairn.kernel.Authority.{CapabilityGrant, Subject}
import cairn.kernel.Effects

/** Explicit effect-execution context. Composition roots construct this and
  * pass it into handlers; handlers must not invent the caller's identity.
  *
  * Carries enough for a later AuthorizedEffect-only handler split without
  * ambient state: [[subject]] + [[gate]] are live today; [[capabilities]] is
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

  /** Build an [[Authority.EffectRequest]] using this context's subject. */
  def effectRequest(
      action: Effects.Action,
      resource: Authority.Resource,
      args: Map[String, String] = Map.empty,
  ): Authority.EffectRequest =
    Authority.EffectRequest(subject, action, resource, args)

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

  /** Fresh bootstrapped Enforce gate + local subject. */
  def bootstrapped(audit: Audit = Audit.Local): EffectContext =
    local(AuthorityGate.bootstrapped(), audit)
