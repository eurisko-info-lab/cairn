package cairn.systemhandler

import cairn.kernel.Authority
import cairn.kernel.Authority.{AuthorizedRequest, EffectRequest, Resource}
import cairn.kernel.Effects

/** Handler-facing proof that an [[EffectRequest]] was authorized.
  *
  * Minted only by [[EffectContext.authorize]] (via [[AuthorityGate.check]] →
  * Kernel [[AuthorizedRequest]]). Handlers accept this token and must not
  * hold a gate or invent a subject. Not publicly constructible.
  */
final class AuthorizedEffect private[systemhandler] (val authorized: AuthorizedRequest):
  def request: EffectRequest = authorized.request
  def action: Effects.ActionKey = authorized.action
  def resource: Resource = authorized.resource
  def subject: Authority.Subject = authorized.subject

  /** Exact cover check: the token must match the action and resource the
    * handler is about to perform (same values passed to authorize).
    */
  def covers(action: Effects.ActionKey, resource: Resource): Boolean =
    this.action == action && this.resource == resource

object AuthorizedEffect:
  private[systemhandler] def mint(a: AuthorizedRequest): AuthorizedEffect =
    AuthorizedEffect(a)
