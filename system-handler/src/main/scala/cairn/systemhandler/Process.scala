package cairn.systemhandler

import cairn.systeminterface.{Filesystem as Fs, Process as Proc}
import java.nio.file.Path

/** Local process runner (Phase 3). */
object Process:
  def run(command: List[String], cwd: Option[Path] = None,
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

  def perform(req: Proc.Request): Either[Proc.Error, Proc.Result] = req match
    case Proc.Request.Run(cmd, cwd, merge) =>
      run(cmd, cwd.map(p => Path.of(p.value)), merge)
