package cairn.systemhandler

import cairn.systeminterface.Clock as Clk
import cairn.kernel.{Authority, Effects}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Wall-clock handler (Phase 3). `nowMillis`/`timestampSlug` are private:
  * `perform` is the only entry point, gated by [[AuthorityGate]]
  * (Subject("local") is a placeholder — this family has no real
  * multi-tenant identity yet).
  */
object Clock:
  private val slugFmt = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

  private def nowMillis(): Long = System.currentTimeMillis()

  private def timestampSlug(): String = LocalDateTime.now().format(slugFmt)

  def perform(req: Clk.Request): Either[Clk.Error, Clk.Response] =
    val action = req match
      case Clk.Request.Now           => Effects.Action.ClockNow
      case Clk.Request.TimestampSlug => Effects.Action.ClockTimestampSlug
    val authReq = Authority.EffectRequest(Authority.Subject("local"), action, Authority.Resource("clock", "*"))
    AuthorityGate.checked(authReq)(err => Clk.Error.Unavailable(s"denied: $err")) {
      try req match
        case Clk.Request.Now => Right(Clk.Response.Instant(nowMillis()))
        case Clk.Request.TimestampSlug => Right(Clk.Response.Slug(timestampSlug()))
      catch case e: Exception => Left(Clk.Error.Unavailable(e.getMessage))
    }
