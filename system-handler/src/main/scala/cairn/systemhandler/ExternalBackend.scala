package cairn.systemhandler

import cairn.systeminterface.{ExternalBackend as EB, Filesystem as Fs, Process as Proc}
import cairn.kernel.{Authority, Effects}
import java.nio.file.{Files, Path}

/** Host-toolchain discovery and invocation (Phase 3). `find`/`run` are
  * private: `perform` is the only entry point, gated by [[AuthorityGate]]
  * (Subject("local") is a placeholder — this family has no real
  * multi-tenant identity yet).
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

  private def run(host: EB.Host, args: List[String],
          cwd: Option[Path] = None): Either[EB.Error, EB.Response] =
    find(host) match
      case None => Right(EB.Response.Missing(host))
      case Some(bin) =>
        Process.perform(Proc.Request.Run(bin.toString :: args, cwd.map(p => Fs.Path(p.toString))))
          .map(r => EB.Response.ProcessResult(r.exitCode, r.combined))
          .left.map(e => EB.Error.Io(e.toString))

  def perform(req: EB.Request): Either[EB.Error, EB.Response] =
    // The Host being invoked is the natural resource identifier — there's
    // no path to scope by until `find` resolves one, and the tool itself
    // is what a policy would actually want to restrict.
    val (action, resourcePath) = req match
      case EB.Request.Find(host)      => (Effects.Action.BackendFind, host.toString)
      case EB.Request.Run(host, _, _) => (Effects.Action.BackendRun, host.toString)
    val authReq = Authority.EffectRequest(Authority.Subject("local"), action, Authority.Resource("externalBackend", resourcePath))
    AuthorityGate.forFamily(Effects.Family.ExternalBackend).checked(authReq)(err => EB.Error.Io(s"denied: $err")) {
      req match
        case EB.Request.Find(host) =>
          find(host) match
            case Some(p) => Right(EB.Response.Found(Fs.Path(p.toString)))
            case None    => Right(EB.Response.Missing(host))
        case EB.Request.Run(host, args, cwd) =>
          run(host, args, cwd.map(p => Path.of(p.value)))
    }
