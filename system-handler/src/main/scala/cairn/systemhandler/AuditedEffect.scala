package cairn.systemhandler

import cairn.kernel.Authority
import cairn.kernel.Authority.{AuditedRequest, EffectRequest, Resource}
import cairn.kernel.Effects

/** Handler-facing proof that an [[EffectRequest]] was *audited* (intent
  * recorded) but **not** enforce-authorized.
  *
  * Minted only by [[EffectContext.audit]] / [[AuthorityGate.audit]]. Must not
  * be accepted by privileged handlers — those require [[AuthorizedEffect]].
  */
final class AuditedEffect private[systemhandler] (val audited: AuditedRequest):
  def request: EffectRequest = audited.request
  def action: Effects.ActionKey = audited.action
  def resource: Resource = audited.resource
  def subject: Authority.Subject = audited.subject

object AuditedEffect:
  /** Package-private mint; ModuleBoundarySuite bans call sites outside EffectContext. */
  private[systemhandler] def mint(a: AuditedRequest): AuditedEffect =
    AuditedEffect(a)
