package cairn.runtime

import cairn.kernel.*
import cairn.core.{PackCompose, Meta}
import cairn.systemhandler.Workspace
import cairn.systeminterface.PackAccess
import cairn.systeminterface.{Filesystem as Fs, Workspace as Ws}
import java.nio.file.Path

/** Runtime pack loader (Phase 6/8) — composition root over
  * `system-handler.Workspace` + `core.PackCompose`. Implements [[PackAccess]]
  * so User code never imports this module directly.
  */
object PackLoader extends PackAccess:
  export PackCompose.{DefaultSurface, demote, bindSurface}

  // Install as the process-global PackAccess on first touch.
  PackAccess.install(this)

  private def unwrapPaths(result: Either[Ws.Error, Ws.Response], label: String): List[Path] = result match
    case Right(Ws.Response.Paths(paths)) => paths.map(p => Path.of(p.value))
    case Left(err) => throw RuntimeException(s"$label failed: $err")
    case Right(other) => throw RuntimeException(s"unexpected workspace response for $label: $other")

  private def unwrapText(result: Either[Ws.Error, Ws.Response], label: String): String = result match
    case Right(Ws.Response.Text(t)) => t
    case Left(err) => throw RuntimeException(s"$label failed: $err")
    case Right(other) => throw RuntimeException(s"unexpected workspace response for $label: $other")

  def languageDirs: List[Path] =
    unwrapPaths(Workspace.perform(Ws.Request.LanguageDirs), "languageDirs")

  def loadRaw(dir: Path): Map[String, List[Fragment]] =
    unwrapPaths(Workspace.perform(Ws.Request.ListCairnFiles(Fs.Path(dir.toString))), "listCairnFiles").map { p =>
      val text = unwrapText(Workspace.perform(Ws.Request.ReadText(Fs.Path(p.toString))), s"readText($p)")
      Meta.parseLanguageAst(text) match
        case Right((name, fs)) => name -> fs
        case Left(err) =>
          throw RuntimeException(s"failed to parse language pack $p: $err")
    }.toMap

  def loadRaw(): Map[String, List[Fragment]] =
    languageDirs.view.map(loadRaw).find(_.nonEmpty).getOrElse(Map.empty)

  def loadSurfaces(dir: Path): Map[String, Map[String, SurfacePack]] =
    unwrapPaths(Workspace.perform(Ws.Request.ListSubdirs(Fs.Path(dir.toString))), "listSubdirs").flatMap { langDir =>
      val langName = langDir.getFileName.toString
      val packs = unwrapPaths(
        Workspace.perform(Ws.Request.ListSurfaceCairnFiles(Fs.Path(langDir.toString))), "listSurfaceCairnFiles"
      ).map { p =>
        val text = unwrapText(Workspace.perform(Ws.Request.ReadText(Fs.Path(p.toString))), s"readText($p)")
        Meta.parseSurfaceAst(text) match
          case Right((style, lang, fs)) =>
            if lang != langName then
              throw RuntimeException(
                s"surface pack $p declares for '$lang' but lives under languages/$langName/")
            style -> SurfacePack(style, langName, fs)
          case Left(err) =>
            throw RuntimeException(s"failed to parse surface pack $p: $err")
      }.toMap
      if packs.isEmpty then None else Some(langName -> packs)
    }.toMap

  def loadSurfaces(): Map[String, Map[String, SurfacePack]] =
    languageDirs.view.map(loadSurfaces).find(_.nonEmpty).getOrElse(Map.empty)

  def surfacesFor(lang: String): Map[String, SurfacePack] =
    loadSurfaces().getOrElse(lang, Map.empty)

  def close(
      name: String,
      packs: Map[String, List[Fragment]],
      surfaces: Map[String, Map[String, SurfacePack]] = Map.empty,
      surface: String = DefaultSurface,
  ): Either[List[ComposeError], ComposedLanguage] =
    PackCompose.close(name, packs, surfaces, surface)

  def close(name: String): Either[List[ComposeError], ComposedLanguage] =
    close(name, loadRaw(), loadSurfaces(), DefaultSurface)

  def close(name: String, surface: String): Either[List[ComposeError], ComposedLanguage] =
    close(name, loadRaw(), loadSurfaces(), surface)

  def requireOwn(name: String): List[Fragment] =
    loadRaw().getOrElse(name, throw RuntimeException(s"language pack '$name' not found under languages/"))

  def requireSurface(lang: String, surface: String = DefaultSurface): SurfacePack =
    surfacesFor(lang).getOrElse(surface,
      throw RuntimeException(s"surface '$surface' for language '$lang' not found under languages/$lang/surfaces/"))

  def requireClosed(name: String, surface: String = DefaultSurface): ComposedLanguage =
    close(name, surface).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")),
      identity)

  def loadClosed(dir: Path, surface: String = DefaultSurface): Map[String, ComposedLanguage] =
    val raw = loadRaw(dir)
    val surfaces = loadSurfaces(dir)
    raw.keys.flatMap(n => close(n, raw, surfaces, surface).toOption.map(n -> _)).toMap

  def loadClosed(): Map[String, ComposedLanguage] =
    languageDirs.view.map(d => loadClosed(d)).find(_.nonEmpty).getOrElse(Map.empty)

  def unmetRequires(name: String, packs: Map[String, List[Fragment]]): Set[String] =
    PackCompose.unmetRequires(name, packs)
