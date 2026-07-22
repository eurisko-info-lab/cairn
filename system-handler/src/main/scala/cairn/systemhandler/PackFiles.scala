package cairn.systemhandler

import cairn.systeminterface.{Filesystem as Fs, Workspace as Ws}
import cairn.kernel.{Authority, Effects}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Workspace / language-pack discovery handler (Phase 3). [[perform]] accepts
  * only a pre-authorized [[AuthorizedEffect]]; use [[run]] as the thin
  * authorize-then-perform adapter. Action/resource keys come from
  * [[EffectMeta.workspace]].
  */
object Workspace:
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.Workspace)

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

  /** Authority resource path for a workspace request.
    *
    * Paths under a discovered language-pack root are rewritten to
    * `languages` / `languages/<rel>` so a single prefix policy stays
    * cwd-independent (covers `languages/`, `../languages/`, absolutes).
    * Paths outside those roots pass through unchanged and are denied by
    * [[cairn.core.PolicyEval.packLoaderWorkspace]].
    */
  private def resourcePath(raw: String): String =
    val p = Path.of(raw).toAbsolutePath.normalize
    languageDirs.iterator
      .map(_.toAbsolutePath.normalize)
      .find(root => p.startsWith(root))
      .map { root =>
        val rel = root.relativize(p).toString.replace('\\', '/')
        if rel.isEmpty || rel == "." then "languages"
        else s"languages/$rel"
      }
      .getOrElse(raw.replace('\\', '/'))

  private def ctorName(req: Ws.Request): String = req match
    case Ws.Request.LanguageDirs               => "languageDirs"
    case Ws.Request.ListCairnFiles(_)          => "listCairnFiles"
    case Ws.Request.ListSubdirs(_)             => "listSubdirs"
    case Ws.Request.ListSurfaceCairnFiles(_)   => "listSurfaceCairnFiles"
    case Ws.Request.ReadText(_)                => "readText"

  def intent(req: Ws.Request,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    val path = req match
      case Ws.Request.LanguageDirs                   => "languages"
      case Ws.Request.ListCairnFiles(dir)            => resourcePath(dir.value)
      case Ws.Request.ListSubdirs(dir)               => resourcePath(dir.value)
      case Ws.Request.ListSurfaceCairnFiles(langDir) => resourcePath(langDir.value)
      case Ws.Request.ReadText(path)                 => resourcePath(path.value)
    (iface(registry).keyFor(ctorName(req)).get, iface(registry).resource.at(path))

  def run(req: Ws.Request, ctx: EffectContext): Either[Ws.Error, Ws.Response] =
    val (action, resource) = intent(req, ctx.registry)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Ws.Error.Io(s"denied: $err"))
      case Right(auth) => perform(req, auth, ctx.registry)

  def perform(req: Ws.Request, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): Either[Ws.Error, Ws.Response] =
    val (action, resource) = intent(req, registry)
    if !auth.covers(action, resource) then Left(Ws.Error.Io("authorized effect does not cover request"))
    else
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
