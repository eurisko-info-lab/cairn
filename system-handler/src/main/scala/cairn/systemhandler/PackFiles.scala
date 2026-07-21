package cairn.systemhandler

import cairn.systeminterface.{Filesystem as Fs, Workspace as Ws}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Workspace / language-pack discovery handler (Phase 3). Absorbs the old
  * `PackFiles` surface under the workspace effect family.
  */
object Workspace:
  def languageDirs: List[Path] =
    List("languages", "../languages", "../../languages").map(Path.of(_))
      .filter(Files.isDirectory(_))

  def readText(p: Path): String = Files.readString(p)

  def listCairnFiles(dir: Path): List[Path] =
    if !Files.exists(dir) then Nil
    else Files.list(dir).iterator.asScala
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
      .toList

  def listSubdirs(dir: Path): List[Path] =
    if !Files.exists(dir) then Nil
    else Files.list(dir).iterator.asScala.filter(Files.isDirectory(_)).toList

  def listSurfaceCairnFiles(langDir: Path): List[Path] =
    val surfRoot = langDir.resolve("surfaces")
    if !Files.isDirectory(surfRoot) then Nil
    else Files.list(surfRoot).iterator.asScala
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
      .toList

  def perform(req: Ws.Request): Either[Ws.Error, Ws.Response] =
    try req match
      case Ws.Request.LanguageDirs =>
        Right(Ws.Response.Paths(languageDirs.map(p => Fs.Path(p.toString))))
      case Ws.Request.ListCairnFiles(dir) =>
        Right(Ws.Response.Paths(listCairnFiles(Path.of(dir.value)).map(p => Fs.Path(p.toString))))
      case Ws.Request.ListSubdirs(dir) =>
        Right(Ws.Response.Paths(listSubdirs(Path.of(dir.value)).map(p => Fs.Path(p.toString))))
      case Ws.Request.ListSurfaceCairnFiles(langDir) =>
        Right(Ws.Response.Paths(listSurfaceCairnFiles(Path.of(langDir.value)).map(p => Fs.Path(p.toString))))
      case Ws.Request.ReadText(path) =>
        Right(Ws.Response.Text(readText(Path.of(path.value))))
    catch case e: Exception => Left(Ws.Error.Io(e.getMessage))

/** Compatibility alias — call sites and PackLoader historically used PackFiles. */
object PackFiles:
  export Workspace.{languageDirs, readText, listCairnFiles, listSubdirs, listSurfaceCairnFiles}
