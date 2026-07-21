package cairn.systemhandler

import cairn.systeminterface.Clock as Clk
import cairn.kernel.{Authority, Effects}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Wall-clock handler (Phase 3). [[perform]] accepts only a pre-authorized
  * [[AuthorizedEffect]]; use [[run]] as the thin authorize-then-perform adapter.
  */
object Clock:
  private val slugFmt = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

  private def nowMillis(): Long = System.currentTimeMillis()

  private def timestampSlug(): String = LocalDateTime.now().format(slugFmt)

  def intent(req: Clk.Request): (Effects.Action, Authority.Resource) =
    val action = req match
      case Clk.Request.Now           => Effects.Action.ClockNow
      case Clk.Request.TimestampSlug => Effects.Action.ClockTimestampSlug
    // "*" is honestly correct: wall-clock time isn't scoped to a target.
    (action, Authority.Resource("clock", "*"))

  def run(req: Clk.Request, ctx: EffectContext): Either[Clk.Error, Clk.Response] =
    val (action, resource) = intent(req)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Clk.Error.Unavailable(s"denied: $err"))
      case Right(auth) => perform(req, auth)

  def perform(req: Clk.Request, auth: AuthorizedEffect): Either[Clk.Error, Clk.Response] =
    val (action, resource) = intent(req)
    if !auth.covers(action, resource) then
      Left(Clk.Error.Unavailable("authorized effect does not cover request"))
    else
      try req match
        case Clk.Request.Now => Right(Clk.Response.Instant(nowMillis()))
        case Clk.Request.TimestampSlug => Right(Clk.Response.Slug(timestampSlug()))
      catch case e: Exception => Left(Clk.Error.Unavailable(e.getMessage))
