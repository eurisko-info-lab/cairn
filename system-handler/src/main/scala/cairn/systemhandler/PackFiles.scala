package cairn.systemhandler

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Raw filesystem access for language-pack discovery (MIGRATION-PLAN.md
  * Phase 2, third slice — "System Meta: loading Meta artifacts; language
  * discovery"). Deliberately `Meta`/`Fragment`/`SurfacePack`-agnostic — it
  * only lists paths and reads text; `workbench.PackLoader` does the parsing.
  */
object PackFiles:
  def languageDirs: List[Path] =
    List("languages", "../languages", "../../languages").map(Path.of(_))
      .filter(Files.isDirectory(_))

  def readText(p: Path): String = Files.readString(p)

  /** `*.cairn` regular files directly under `dir`. */
  def listCairnFiles(dir: Path): List[Path] =
    if !Files.exists(dir) then Nil
    else Files.list(dir).iterator.asScala
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
      .toList

  /** Subdirectories of `dir` — candidate `languages/<lang>/` roots. */
  def listSubdirs(dir: Path): List[Path] =
    if !Files.exists(dir) then Nil
    else Files.list(dir).iterator.asScala.filter(Files.isDirectory(_)).toList

  /** `*.cairn` regular files under `<langDir>/surfaces/`, if that dir exists. */
  def listSurfaceCairnFiles(langDir: Path): List[Path] =
    val surfRoot = langDir.resolve("surfaces")
    if !Files.isDirectory(surfRoot) then Nil
    else Files.list(surfRoot).iterator.asScala
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
      .toList
