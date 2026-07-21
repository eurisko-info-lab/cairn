package cairn.systemhandler

import cairn.systeminterface.Random as Rnd
import java.security.SecureRandom

/** Secure-randomness handler (Phase 3). */
object Random:
  private val secure = new SecureRandom()

  def bytes(n: Int): Array[Byte] =
    val out = new Array[Byte](n)
    secure.nextBytes(out)
    out

  def perform(req: Rnd.Request): Either[Rnd.Error, Rnd.Response] = req match
    case Rnd.Request.Bytes(n) =>
      try Right(Rnd.Response.Bytes(bytes(n)))
      catch case e: Exception => Left(Rnd.Error.Unavailable(e.getMessage))
