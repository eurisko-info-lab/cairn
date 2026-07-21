package cairn.systemhandler

import cairn.kernel.*
import cairn.core.Meta
import java.nio.file.Path

/** Phase 7: effectful Meta activation — load bytes, parse via Core, validate
  * via Kernel. Distinct from Core elaboration and Kernel checkers.
  */
object MetaActivation:
  /** Load a `.cairn` Meta/language file from disk, elaborate, and Kernel-check. */
  def loadAndValidate(path: Path): Either[String, ComposedLanguage] =
    try
      val text = Filesystem.readText(path)
      Meta.parseLanguageAst(text).flatMap { (name, fragments) =>
        MetaValidate.checkFragmentShapes(fragments).flatMap(_ =>
          MetaValidate.checkComposable(name, fragments))
      }
    catch case e: Exception => Left(e.getMessage)
