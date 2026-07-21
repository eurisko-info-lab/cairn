package cairn.systemhandler

import cairn.systeminterface.{Filesystem as Fs, Workspace as Ws}
import cairn.kernel.{Authority, Effects}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Workspace / language-pack discovery handler (Phase 3). `perform` is the
  * only entry point, gated by [[AuthorityGate]] (Subject("local") is a
  * placeholder — this family has no real multi-tenant identity yet); every
  * other method here is private.
  */
object Workspace:
  private def languageDirs: List[Path] =
    List("languages", "../languages", "../../languages").map(Path.of(_))
      .filter(Files.isDirectory(_))

  private def readText(p: Path): String = Files.readString(p)

  private def listCairnFiles(dir: Path): List[Path] =
    if !Files.exists(dir) then Nil
    else Files.list(dir).iterator.asScala
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
      .toList

  private def listSubdirs(dir: Path): List[Path] =
    if !Files.exists(dir) then Nil
    else Files.list(dir).iterator.asScala.filter(Files.isDirectory(_)).toList

  private def listSurfaceCairnFiles(langDir: Path): List[Path] =
    val surfRoot = langDir.resolve("surfaces")
    if !Files.isDirectory(surfRoot) then Nil
    else Files.list(surfRoot).iterator.asScala
      .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
      .toList

  def perform(req: Ws.Request, gate: AuthorityGate): Either[Ws.Error, Ws.Response] =
    // Real request target, not a wildcard, so path-scoped policies have
    // real data to match against (same pattern as Filesystem). LanguageDirs
    // takes no input — it discovers the dirs rather than targeting one —
    // so "*" there is honestly correct, not a placeholder.
    val resourcePath = req match
      case Ws.Request.LanguageDirs                  => "*"
      case Ws.Request.ListCairnFiles(dir)           => dir.value
      case Ws.Request.ListSubdirs(dir)              => dir.value
      case Ws.Request.ListSurfaceCairnFiles(langDir) => langDir.value
      case Ws.Request.ReadText(path)                => path.value
    val authReq = Authority.EffectRequest(
      Authority.Subject("local"), Effects.Action.WorkspaceRead, Authority.Resource("workspace", resourcePath))
    gate.checked(authReq)(err => Ws.Error.Io(s"denied: $err")) {
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
    }
