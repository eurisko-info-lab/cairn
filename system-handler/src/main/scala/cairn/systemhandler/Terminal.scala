package cairn.systemhandler

import cairn.systeminterface.Terminal as Term
import cairn.kernel.{Authority, Effects}
import java.io.{BufferedReader, InputStreamReader}

/** Stdio terminal handler (Phase 3). [[perform]] accepts only a pre-authorized
  * [[AuthorizedEffect]]; use [[run]] as the thin authorize-then-perform adapter.
  * Keys from [[EffectMeta.terminal]].
  */
object Terminal:
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.Terminal)
  private lazy val in = new BufferedReader(new InputStreamReader(System.in))

  private def write(text: String): Unit = Console.print(text)
  private def writeLine(text: String): Unit = Console.println(text)

  private def readLine(): Option[String] =
    Option(in.readLine())

  def intent(req: Term.Request,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    val ctor = req match
      case Term.Request.ReadLine     => "readLine"
      case Term.Request.Write(_)     => "write"
      case Term.Request.WriteLine(_) => "writeLine"
    (iface(registry).keyFor(ctor).get, iface(registry).resource.any)

  def run(req: Term.Request, ctx: EffectContext): Either[Term.Error, Term.Response] =
    val (action, resource) = intent(req, ctx.registry)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Term.Error.Io(s"denied: $err"))
      case Right(auth) => perform(req, auth, ctx.registry)

  def perform(req: Term.Request, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): Either[Term.Error, Term.Response] =
    val (action, resource) = intent(req, registry)
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
