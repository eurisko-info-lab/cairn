package cairn.systemhandler

import cairn.systeminterface.{Filesystem as Fs, Process as Proc}
import cairn.kernel.{Authority, Effects}
import java.nio.file.Path

/** Local process runner (Phase 3). `run` is private: `perform` is the only
  * entry point, gated by [[AuthorityGate]] (Subject("local") is a
  * placeholder — this family has no real multi-tenant identity yet).
  */
object Process:
  private def run(command: List[String], cwd: Option[Path] = None,
          mergeStderr: Boolean = true): Either[Proc.Error, Proc.Result] =
    if command.isEmpty then Left(Proc.Error.Io("empty command"))
    else
      try
        val pb = new ProcessBuilder(command*)
        cwd.foreach(d => pb.directory(d.toFile))
        pb.redirectErrorStream(mergeStderr)
        val proc = pb.start()
        val stdout = new String(proc.getInputStream.readAllBytes())
        val stderr =
          if mergeStderr then ""
          else new String(proc.getErrorStream.readAllBytes())
        val code = proc.waitFor()
        Right(Proc.Result(code, stdout, stderr))
      catch
        case _: java.io.IOException =>
          Left(Proc.Error.NotFound(command.head))
        case e: Exception =>
          Left(Proc.Error.Io(e.getMessage))

  def perform(req: Proc.Request, gate: AuthorityGate): Either[Proc.Error, Proc.Result] =
    // The executable being run is the natural resource identifier.
    val (action, resourcePath) = req match
      case Proc.Request.Run(command, _, _) => (Effects.Action.ProcessRun, command.headOption.getOrElse("*"))
    val authReq = Authority.EffectRequest(Authority.Subject("local"), action, Authority.Resource("process", resourcePath))
    gate.checked(authReq)(err => Proc.Error.Io(s"denied: $err")) {
      req match
        case Proc.Request.Run(cmd, cwd, merge) =>
          run(cmd, cwd.map(p => Path.of(p.value)), merge)
    }
