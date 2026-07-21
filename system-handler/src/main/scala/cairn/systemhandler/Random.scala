package cairn.systemhandler

import cairn.systeminterface.Random as Rnd
import cairn.kernel.{Authority, Effects}
import java.security.SecureRandom

/** Secure-randomness handler (Phase 3). `bytes` is private: `perform` is
  * the only entry point, gated via [[EffectContext]] (subject from
  * composition root).
  */
object Random:
  private val secure = new SecureRandom()

  private def bytes(n: Int): Array[Byte] =
    val out = new Array[Byte](n)
    secure.nextBytes(out)
    out

  def perform(req: Rnd.Request, ctx: EffectContext): Either[Rnd.Error, Rnd.Response] =
    val action = req match
      case Rnd.Request.Bytes(_) => Effects.Action.RandomBytes
    // "*" is honestly correct here, not a placeholder: randomness isn't
    // scoped to any target — `n` is a quantity, not something to restrict.
    val authReq = ctx.effectRequest(action, Authority.Resource("random", "*"))
    ctx.gate.checked(authReq)(err => Rnd.Error.Unavailable(s"denied: $err")) {
      try req match
        case Rnd.Request.Bytes(n) => Right(Rnd.Response.Bytes(bytes(n)))
      catch case e: Exception => Left(Rnd.Error.Unavailable(e.getMessage))
    }
