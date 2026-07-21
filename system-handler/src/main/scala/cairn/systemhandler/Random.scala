package cairn.systemhandler

import cairn.systeminterface.Random as Rnd
import cairn.kernel.{Authority, Effects}
import java.security.SecureRandom

/** Secure-randomness handler (Phase 3). [[perform]] accepts only a
  * pre-authorized [[AuthorizedEffect]]; use [[run]] as the thin
  * authorize-then-perform adapter.
  */
object Random:
  private val secure = new SecureRandom()

  private def bytes(n: Int): Array[Byte] =
    val out = new Array[Byte](n)
    secure.nextBytes(out)
    out

  def intent(req: Rnd.Request): (Effects.Action, Authority.Resource) =
    req match
      case Rnd.Request.Bytes(_) =>
        (Effects.Action.RandomBytes, Authority.Resource("random", "*"))

  def run(req: Rnd.Request, ctx: EffectContext): Either[Rnd.Error, Rnd.Response] =
    val (action, resource) = intent(req)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Rnd.Error.Unavailable(s"denied: $err"))
      case Right(auth) => perform(req, auth)

  def perform(req: Rnd.Request, auth: AuthorizedEffect): Either[Rnd.Error, Rnd.Response] =
    val (action, resource) = intent(req)
    if !auth.covers(action, resource) then
      Left(Rnd.Error.Unavailable("authorized effect does not cover request"))
    else
      try req match
        case Rnd.Request.Bytes(n) => Right(Rnd.Response.Bytes(bytes(n)))
      catch case e: Exception => Left(Rnd.Error.Unavailable(e.getMessage))
