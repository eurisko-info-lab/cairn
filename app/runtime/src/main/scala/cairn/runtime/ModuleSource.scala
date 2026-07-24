package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.Filesystem
import cairn.systeminterface.Filesystem as Fs
import java.nio.file.Path

/** Load instance modules from checked-in `.cairn` ModuleSurface text.
  *
  * Domain fixtures (Law acts, SDS chemicals, profiles, workflow modules)
  * live on disk under `content/languages/`. This is the generic host I/O
  * path — domain packs must not ship Scala that embeds the same data.
  */
object ModuleSource:
  private val fsCtx = EffectContexts.forFilesystem()

  private def fsPath(p: Path): Fs.Path =
    Fs.Path(p.toAbsolutePath.normalize.toString)

  def readText(path: Path): Either[String, String] =
    Filesystem.run(Fs.Request.Read(fsPath(path)), fsCtx) match
      case Right(Fs.Response.Text(s)) => Right(s)
      case Left(Fs.Error.NotFound(_)) => Left(s"missing module source: $path")
      case Left(e) => Left(e.toString)
      case Right(other) => Left(s"unexpected fs response: $other")

  def load(language: ComposedLanguage, path: Path): Either[String, Module] =
    for
      text <- readText(path)
      cst <- Parser.parse(ModuleSurface.grammar(language), text)
      mod <- ModuleSurface.toModule(cst)
    yield mod.sorted

  def lawActsDir: Path = Path.of("content/languages/law/acts")
  def chemicalsDir: Path = Path.of("content/languages/sds/chemicals")
  def profilesDir: Path = Path.of("content/languages/sds/profiles")

  /** Thin Model Chemical Safety Act — Law corpus SoT. */
  def modelChemicalSafetyAct(language: ComposedLanguage): Either[String, Module] =
    load(language, lawActsDir.resolve("model-chemical-safety.cairn"))
