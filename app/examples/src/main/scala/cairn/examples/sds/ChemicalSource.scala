package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.ModuleSource
import java.nio.file.Path

/** Compatibility façade: SDS / regulatory instance modules load via
  * [[ModuleSource]] from checked-in `.cairn` text under `languages/sds/…`.
  */
object ChemicalSource:
  def readText(path: Path): Either[String, String] = ModuleSource.readText(path)

  def loadModule(language: ComposedLanguage, path: Path): Either[String, Module] =
    ModuleSource.load(language, path)

  def chemicalsDir: Path = ModuleSource.chemicalsDir
  def profilesDir: Path = ModuleSource.profilesDir

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
