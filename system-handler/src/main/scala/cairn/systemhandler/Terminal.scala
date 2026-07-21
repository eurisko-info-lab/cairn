package cairn.systemhandler

import cairn.systeminterface.Terminal as Term
import cairn.kernel.{Authority, Effects}
import java.io.{BufferedReader, InputStreamReader}

/** Stdio terminal handler (Phase 3). [[perform]] accepts only a pre-authorized
  * [[AuthorizedEffect]]; use [[run]] as the thin authorize-then-perform adapter.
  */
object Terminal:
  private lazy val in = new BufferedReader(new InputStreamReader(System.in))

  private def write(text: String): Unit = Console.print(text)
  private def writeLine(text: String): Unit = Console.println(text)

  private def readLine(): Option[String] =
    Option(in.readLine())

  def intent(req: Term.Request): (Effects.Action, Authority.Resource) =
    val action = req match
      case Term.Request.ReadLine     => Effects.Action.TerminalRead
      case Term.Request.Write(_)     => Effects.Action.TerminalWrite
      case Term.Request.WriteLine(_) => Effects.Action.TerminalWrite
    (action, Authority.Resource("terminal", "*"))

  def run(req: Term.Request, ctx: EffectContext): Either[Term.Error, Term.Response] =
    val (action, resource) = intent(req)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Term.Error.Io(s"denied: $err"))
      case Right(auth) => perform(req, auth)

  def perform(req: Term.Request, auth: AuthorizedEffect): Either[Term.Error, Term.Response] =
    val (action, resource) = intent(req)
    if !auth.covers(action, resource) then Left(Term.Error.Io("authorized effect does not cover request"))
    else
      try req match
        case Term.Request.Write(t) => write(t); Right(Term.Response.Ok)
        case Term.Request.WriteLine(t) => writeLine(t); Right(Term.Response.Ok)
        case Term.Request.ReadLine =>
          readLine() match
            case Some(l) => Right(Term.Response.Line(l))
            case None    => Right(Term.Response.Eof)
      catch
        case _: java.io.IOException => Left(Term.Error.Closed)
        case e: Exception           => Left(Term.Error.Io(e.getMessage))
