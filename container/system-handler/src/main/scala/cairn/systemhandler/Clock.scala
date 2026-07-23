package cairn.systemhandler

import cairn.systeminterface.Clock as Clk
import cairn.kernel.{Authority, Effects}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Wall-clock handler (Phase 3). [[perform]] accepts only a pre-authorized
  * [[AuthorizedEffect]]; use [[run]] as the thin authorize-then-perform adapter.
  * Action/resource keys come from [[EffectMeta.clock]].
  */
object Clock:
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.Clock)
  private val slugFmt = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

  private def nowMillis(): Long = System.currentTimeMillis()

  private def timestampSlug(): String = LocalDateTime.now().format(slugFmt)

  def intent(req: Clk.Request,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    val ctor = req match
      case Clk.Request.Now           => "now"
      case Clk.Request.TimestampSlug => "timestampSlug"
    // "*" is honestly correct: wall-clock time isn't scoped to a target.
    (iface(registry).keyFor(ctor).get, iface(registry).resource.any)

  def run(req: Clk.Request, ctx: EffectContext): Either[Clk.Error, Clk.Response] =
    val (action, resource) = intent(req, ctx.registry)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Clk.Error.Unavailable(s"denied: $err"))
      case Right(auth) => perform(req, auth, ctx.registry)

  def perform(req: Clk.Request, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): Either[Clk.Error, Clk.Response] =
    val (action, resource) = intent(req, registry)
    if !auth.covers(action, resource) then
      Left(Clk.Error.Unavailable("authorized effect does not cover request"))
    else
      try req match
        case Clk.Request.Now => Right(Clk.Response.Instant(nowMillis()))
        case Clk.Request.TimestampSlug => Right(Clk.Response.Slug(timestampSlug()))
      catch case e: Exception => Left(Clk.Error.Unavailable(e.getMessage))
