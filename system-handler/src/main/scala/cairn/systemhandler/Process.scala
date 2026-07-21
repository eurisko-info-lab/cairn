package cairn.systemhandler

import cairn.systeminterface.{Filesystem as Fs, Process as Proc}
import cairn.kernel.{Authority, EffectMeta, Effects}
import java.nio.file.Path

/** Local process runner (Phase 3). [[perform]] accepts only a pre-authorized
  * [[AuthorizedEffect]]; use [[run]] as the thin authorize-then-perform adapter.
  * Action/resource keys come from [[EffectMeta.process]].
  */
object Process:
  private val iface = EffectMeta.process

  private def runCmd(command: List[String], cwd: Option[Path] = None,
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

  def intent(req: Proc.Request): (Effects.ActionKey, Authority.Resource) =
    req match
      case Proc.Request.Run(command, _, _) =>
        (iface.actionKey("run"), iface.resource.at(command.headOption.getOrElse("*")))

  def run(req: Proc.Request, ctx: EffectContext): Either[Proc.Error, Proc.Result] =
    val (action, resource) = intent(req)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Proc.Error.Io(s"denied: $err"))
      case Right(auth) => perform(req, auth)

  def perform(req: Proc.Request, auth: AuthorizedEffect): Either[Proc.Error, Proc.Result] =
    val (action, resource) = intent(req)
    if !auth.covers(action, resource) then Left(Proc.Error.Io("authorized effect does not cover request"))
    else req match
      case Proc.Request.Run(cmd, cwd, merge) =>
        runCmd(cmd, cwd.map(p => Path.of(p.value)), merge)
