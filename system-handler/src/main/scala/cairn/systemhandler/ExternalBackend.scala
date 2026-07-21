package cairn.systemhandler

import cairn.systeminterface.{ExternalBackend as EB, Filesystem as Fs}
import java.nio.file.{Files, Path}

/** Host-toolchain discovery and invocation (Phase 3). */
object ExternalBackend:
  private def findOnPath(name: String): Option[Path] =
    sys.env.getOrElse("PATH", "").split(":")
      .map(Path.of(_, name)).find(Files.isExecutable)

  def find(host: EB.Host): Option[Path] = host match
    case EB.Host.ScalaCli => findOnPath("scala-cli")
    case EB.Host.Cargo    => findOnPath("cargo")
    case EB.Host.Runghc   => findOnPath("runghc")
    case EB.Host.Lake     => findOnPath("lake")

  def run(host: EB.Host, args: List[String],
          cwd: Option[Path] = None): Either[EB.Error, EB.Response] =
    find(host) match
      case None => Right(EB.Response.Missing(host))
      case Some(bin) =>
        Process.run(bin.toString :: args, cwd).map(r =>
          EB.Response.ProcessResult(r.exitCode, r.combined)).left.map(e =>
          EB.Error.Io(e.toString))

  def perform(req: EB.Request): Either[EB.Error, EB.Response] = req match
    case EB.Request.Find(host) =>
      find(host) match
        case Some(p) => Right(EB.Response.Found(Fs.Path(p.toString)))
        case None    => Right(EB.Response.Missing(host))
    case EB.Request.Run(host, args, cwd) =>
      run(host, args, cwd.map(p => Path.of(p.value)))
