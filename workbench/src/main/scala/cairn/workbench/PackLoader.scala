package cairn.workbench

import cairn.kernel.*
import cairn.core.{PackCompose, Meta}
import cairn.systemhandler.PackFiles
import java.nio.file.Path

/** Runtime pack loader for checked-in `.cairn` languages (M42 + exemplar DAG).
  *
  * Fragment `provides`/`requires` edges are real composition constraints: a pack
  * whose own fragments require an interface must pull in a providing pack before
  * [[Compose.compose]] succeeds. The shipped exemplar chain is
  * `PKI (cert) → Law (law) → SDS (sds)`.
  *
  * Dependency fragments are demoted (no `top`, no `varCtor`) so leaf packs keep
  * a single surface top while still amalgamating dependency sorts/ctors/grammar.
  *
  * Phase 2/3: semantic language files under `languages/<name>.cairn` may be paired
  * with concrete surfaces at `languages/<name>/surfaces/<style>.cairn`
  * (`surface <style> for <name> { … }`). Closing a pack binds the requested surface
  * (default `"default"`) before compose. Fused packs (no `surfaces/` dir — e.g. meta)
  * keep grammar in the language file.
  *
  * MIGRATION-PLAN.md Phase 2 (third slice): this object is now a thin
  * orchestrator over `system-handler.PackFiles` (raw I/O) and
  * `core.PackCompose` (the pure composition algorithm) — its own public API
  * is unchanged, so no external call site needed to change. It stays here,
  * not in `core` or `system-handler`, because it inherently combines both:
  * there's no `runtime`/`user` composition-root module yet for it to live in.
  */
object PackLoader:
  export PackCompose.{DefaultSurface, demote, bindSurface, unmetRequires}

  def languageDirs: List[Path] = PackFiles.languageDirs

  /** Raw fragments per language file name (no composition / no dep resolution).
    * Parse failures are fatal — silent skips hide incomplete `.cairn` packs.
    */
  def loadRaw(dir: Path): Map[String, List[Fragment]] =
    PackFiles.listCairnFiles(dir).map { p =>
      Meta.parseLanguageAst(PackFiles.readText(p)) match
        case Right((name, fs)) => name -> fs
        case Left(err) =>
          throw RuntimeException(s"failed to parse language pack $p: $err")
    }.toMap

  def loadRaw(): Map[String, List[Fragment]] =
    languageDirs.view.map(loadRaw).find(_.nonEmpty).getOrElse(Map.empty)

  /** Surfaces keyed by language name, then style name. */
  def loadSurfaces(dir: Path): Map[String, Map[String, SurfacePack]] =
    PackFiles.listSubdirs(dir).flatMap { langDir =>
      val langName = langDir.getFileName.toString
      val packs = PackFiles.listSurfaceCairnFiles(langDir).map { p =>
        Meta.parseSurfaceAst(PackFiles.readText(p)) match
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

  /** Close every pack that can be closed from `dir` (skips unsatisfied graphs). */
  def loadClosed(dir: Path, surface: String = DefaultSurface): Map[String, ComposedLanguage] =
    val raw = loadRaw(dir)
    val surfaces = loadSurfaces(dir)
    raw.keys.flatMap(n => close(n, raw, surfaces, surface).toOption.map(n -> _)).toMap

  def loadClosed(): Map[String, ComposedLanguage] =
    languageDirs.view.map(d => loadClosed(d)).find(_.nonEmpty).getOrElse(Map.empty)
