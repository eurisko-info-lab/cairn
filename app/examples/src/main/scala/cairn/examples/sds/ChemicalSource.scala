package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{EffectContext, Filesystem}
import cairn.systeminterface.Filesystem as Fs
import java.nio.file.Path

/** Load SDS / regulatory instance modules from checked-in `.cairn` text
  * (ModuleSurface of a closed language). Chemical instances and the EU-CLP
  * annex profile are source artifacts under `languages/sds/…`, not Scala
  * projections.
  */
object ChemicalSource:
  private val fsCtx = EffectContext.forFilesystem()

  private def fsPath(p: Path): Fs.Path =
    Fs.Path(p.toAbsolutePath.normalize.toString)

  def readText(path: Path): Either[String, String] =
    Filesystem.run(Fs.Request.Read(fsPath(path)), fsCtx) match
      case Right(Fs.Response.Text(s)) => Right(s)
      case Left(Fs.Error.NotFound(_)) => Left(s"missing chemical/profile source: $path")
      case Left(e) => Left(e.toString)
      case Right(other) => Left(s"unexpected fs response: $other")

  def loadModule(language: ComposedLanguage, path: Path): Either[String, Module] =
    for
      text <- readText(path)
      cst <- Parser.parse(ModuleSurface.grammar(language), text)
      mod <- ModuleSurface.toModule(cst)
    yield mod.sorted

  def chemicalsDir: Path = Path.of("content/languages/sds/chemicals")
  def profilesDir: Path = Path.of("content/languages/sds/profiles")

  def acetoneThin(language: ComposedLanguage): Either[String, Module] =
    loadModule(language, chemicalsDir.resolve("acetone-thin.cairn"))
  def acetone(language: ComposedLanguage): Either[String, Module] =
    loadModule(language, chemicalsDir.resolve("acetone.cairn"))
  def ethanolThin(language: ComposedLanguage): Either[String, Module] =
    loadModule(language, chemicalsDir.resolve("ethanol-thin.cairn"))
  def ethanol(language: ComposedLanguage): Either[String, Module] =
    loadModule(language, chemicalsDir.resolve("ethanol.cairn"))

  def euClpAnnexIi(language: ComposedLanguage): Either[String, Module] =
    loadModule(language, profilesDir.resolve("eu-clp-annex-ii.cairn"))
