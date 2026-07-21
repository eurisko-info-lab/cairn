package cairn.systemhandler

import cairn.systeminterface.{ExternalBackend as EB, Filesystem as Fs, Process as Proc}
import cairn.kernel.{Authority, Effects}
import java.nio.file.{Files, Path}

/** Host-toolchain discovery and invocation (Phase 3). [[perform]] accepts
  * only a pre-authorized [[AuthorizedEffect]]; nested process runs use
  * [[Process.run]] with a separate process [[EffectContext]].
  */
object ExternalBackend:
  private def findOnPath(name: String): Option[Path] =
    sys.env.getOrElse("PATH", "").split(":")
      .map(Path.of(_, name)).find(Files.isExecutable)

  private def find(host: EB.Host): Option[Path] = host match
    case EB.Host.ScalaCli => findOnPath("scala-cli")
    case EB.Host.Cargo    => findOnPath("cargo")
    case EB.Host.Runghc   => findOnPath("runghc")
    case EB.Host.Lake     => findOnPath("lake")

  private def runHost(host: EB.Host, args: List[String],
          cwd: Option[Path], processCtx: EffectContext): Either[EB.Error, EB.Response] =
    find(host) match
      case None => Right(EB.Response.Missing(host))
      case Some(bin) =>
        Process.run(
          Proc.Request.Run(bin.toString :: args, cwd.map(p => Fs.Path(p.toString))),
          processCtx)
          .map(r => EB.Response.ProcessResult(r.exitCode, r.combined))
          .left.map(e => EB.Error.Io(e.toString))

  def intent(req: EB.Request): (Effects.Action, Authority.Resource) =
    val (action, resourcePath) = req match
      case EB.Request.Find(host)      => (Effects.Action.BackendFind, host.toString)
      case EB.Request.Run(host, _, _) => (Effects.Action.BackendRun, host.toString)
    (action, Authority.Resource("externalBackend", resourcePath))

  def run(req: EB.Request, ctx: EffectContext, processCtx: EffectContext)
      : Either[EB.Error, EB.Response] =
    val (action, resource) = intent(req)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(EB.Error.Io(s"denied: $err"))
      case Right(auth) => perform(req, auth, processCtx)

  def perform(req: EB.Request, auth: AuthorizedEffect, processCtx: EffectContext)
      : Either[EB.Error, EB.Response] =
    val (action, resource) = intent(req)
    if !auth.covers(action, resource) then Left(EB.Error.Io("authorized effect does not cover request"))
    else req match
      case EB.Request.Find(host) =>
        find(host) match
          case Some(p) => Right(EB.Response.Found(Fs.Path(p.toString)))
          case None    => Right(EB.Response.Missing(host))
      case EB.Request.Run(host, args, cwd) =>
        runHost(host, args, cwd.map(p => Path.of(p.value)), processCtx)
