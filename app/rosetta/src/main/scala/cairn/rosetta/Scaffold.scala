package cairn.rosetta

import cairn.core.{RosettaModule2, ScaffoldPlan}
import cairn.systemhandler.{EffectContext, Filesystem}
import cairn.systeminterface.Filesystem as Fs
import java.nio.file.Path

/** M33: full host project scaffolds + machine-readable obligations manifest
  * linking every theorem to its host artifact.
  *
  * Thin I/O façade over `core.ScaffoldPlan` (pure relative-path planning +
  * obligations JSON) and `system-handler.Filesystem`. Public `Project` keeps
  * `java.nio.file.Path` for callers; the pure half never sees `Path`.
  *
  * One deliberate behavior refinement from the pre-split version: writes now
  * happen only after the full plan succeeds, instead of incrementally as
  * each host's port is verified — so a later port failing no longer leaves
  * earlier hosts' files partially written. Nothing tests or relies on the
  * old partial-write-on-failure behavior.
  */
object Scaffold:
  final case class Project(host: String, root: Path, mainFile: Path, buildCommand: List[String])

  export ScaffoldPlan.obligationsManifest

  def emitAll(m: RosettaModule2, dir: Path, ctx: EffectContext): Either[String, List[Project]] =
    ScaffoldPlan.plan(m).flatMap { (ps, writes) =>
      val pathArgs = writes.map(_._1).toSet ++ ps.flatMap(p => List(p.root, p.mainFile))
      val written = writes.foldLeft[Either[String, Unit]](Right(())) { (acc, rw) =>
        val (rel, content) = rw
        acc.flatMap(_ =>
          Filesystem.run(Fs.Request.Write(Fs.Path(dir.resolve(rel).toString), content), ctx)
            .map(_ => ()).left.map(_.toString))
      }
      written.map { _ =>
        ps.map { p =>
          Project(
            p.host,
            dir.resolve(p.root),
            dir.resolve(p.mainFile),
            p.buildCommand.map(a => if pathArgs(a) then dir.resolve(a).toString else a))
        }
      }
    }
