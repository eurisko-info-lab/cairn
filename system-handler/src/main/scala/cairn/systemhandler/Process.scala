package cairn.systemhandler

import cairn.systeminterface.{Filesystem as Fs, Process as Proc}
import cairn.kernel.{Authority, Effects}
import java.nio.file.Path

/** Local process runner (Phase 3). [[perform]] accepts only a pre-authorized
  * [[AuthorizedEffect]]; use [[run]] as the thin authorize-then-perform adapter.
  * Action/resource keys come from [[EffectMeta.process]].
  */
object Process:
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.Process)

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

  def intent(req: Proc.Request,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    req match
      case Proc.Request.Run(command, _, _) =>
        // Authorize by executable basename so allow-lists name `scala-cli`
        // rather than absolute PATH resolutions.
        val head = command.headOption.getOrElse("*")
        val base =
          try Path.of(head).getFileName.toString
          catch case _: Exception => head
        (iface(registry).actionKey("run"), iface(registry).resource.at(base))

  def run(req: Proc.Request, ctx: EffectContext): Either[Proc.Error, Proc.Result] =
    val (action, resource) = intent(req, ctx.registry)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Proc.Error.Io(s"denied: $err"))
      case Right(auth) => perform(req, auth, ctx.registry)

  def perform(req: Proc.Request, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): Either[Proc.Error, Proc.Result] =
    val (action, resource) = intent(req, registry)
    if !auth.covers(action, resource) then Left(Proc.Error.Io("authorized effect does not cover request"))
    else req match
      case Proc.Request.Run(cmd, cwd, merge) =>
        runCmd(cmd, cwd.map(p => Path.of(p.value)), merge)
