package cairn.systemhandler

import cairn.kernel.*
import cairn.core.Meta
import cairn.systeminterface.Filesystem as Fs
import java.nio.file.Path

/** Phase 7: effectful Meta activation — load bytes, parse via Core, validate
  * via Kernel. Distinct from Core elaboration and Kernel checkers.
  */
object MetaActivation:
  /** Load a `.cairn` Meta/language file from disk, elaborate, and Kernel-check. */
  def loadAndValidate(path: Path, ctx: EffectContext): Either[String, ComposedLanguage] =
    Filesystem.run(Fs.Request.Read(Fs.Path(path.toString)), ctx) match
      case Left(e) => Left(e.toString)
      case Right(Fs.Response.Text(text)) =>
        Meta.parseLanguageAst(text).flatMap { (name, fragments) =>
          MetaValidate.checkFragmentShapes(fragments).flatMap(_ =>
            MetaValidate.checkComposable(name, fragments))
        }
      case Right(other) => Left(s"unexpected filesystem response: $other")
