package cairn.systemhandler

import cairn.systeminterface.Clock as Clk
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Wall-clock handler (Phase 3). */
object Clock:
  private val slugFmt = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

  def nowMillis(): Long = System.currentTimeMillis()

  def timestampSlug(): String = LocalDateTime.now().format(slugFmt)

  def perform(req: Clk.Request): Either[Clk.Error, Clk.Response] =
    try req match
      case Clk.Request.Now => Right(Clk.Response.Instant(nowMillis()))
      case Clk.Request.TimestampSlug => Right(Clk.Response.Slug(timestampSlug()))
    catch case e: Exception => Left(Clk.Error.Unavailable(e.getMessage))
