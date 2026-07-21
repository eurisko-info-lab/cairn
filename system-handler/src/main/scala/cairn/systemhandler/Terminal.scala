package cairn.systemhandler

import cairn.systeminterface.Terminal as Term
import cairn.kernel.{Authority, Effects}
import java.io.{BufferedReader, InputStreamReader}

/** Stdio terminal handler (Phase 3). `write`/`writeLine`/`readLine` are
  * private: `perform` is the only entry point, gated by [[AuthorityGate]]
  * (Subject("local") is a placeholder — this family has no real
  * multi-tenant identity yet).
  */
object Terminal:
  private lazy val in = new BufferedReader(new InputStreamReader(System.in))

  private def write(text: String): Unit = Console.print(text)
  private def writeLine(text: String): Unit = Console.println(text)

  private def readLine(): Option[String] =
    Option(in.readLine())

  def perform(req: Term.Request, gate: AuthorityGate): Either[Term.Error, Term.Response] =
    val action = req match
      case Term.Request.ReadLine     => Effects.Action.TerminalRead
      case Term.Request.Write(_)     => Effects.Action.TerminalWrite
      case Term.Request.WriteLine(_) => Effects.Action.TerminalWrite
    // "*" is honestly correct here, not a placeholder: stdio is a
    // session-level resource, not a per-request one — there's no "which
    // terminal" to scope by.
    val authReq = Authority.EffectRequest(Authority.Subject("local"), action, Authority.Resource("terminal", "*"))
    gate.checked(authReq)(err => Term.Error.Io(s"denied: $err")) {
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
    }
