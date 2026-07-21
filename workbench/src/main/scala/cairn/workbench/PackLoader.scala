package cairn.workbench

import cairn.kernel.*
import java.nio.file.{Files, Path}

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
  */
object PackLoader:
  val DefaultSurface: String = "default"

  /** Strip leaf-only surface markers when a pack is used as a dependency. */
  def demote(f: Fragment): Fragment =
    f.copy(varCtor = None, grammar = f.grammar.copy(top = None))

  /** Merge surface grammar onto semantic fragments by fragment name. */
  def bindSurface(semantic: List[Fragment], surface: SurfacePack): List[Fragment] =
    val byName = surface.fragments.map(f => f.name -> f.grammar).toMap
    val unknown = byName.keySet -- semantic.map(_.name).toSet
    if unknown.nonEmpty then
      throw RuntimeException(
        s"surface ${surface.language}/${surface.name} has fragments not in language: ${unknown.toList.sorted.mkString(", ")}")
    semantic.map(f => f.copy(grammar = byName.getOrElse(f.name, f.grammar)))

  def languageDirs: List[Path] =
    List("languages", "../languages", "../../languages").map(Path.of(_))
      .filter(Files.isDirectory(_))

  /** Raw fragments per language file name (no composition / no dep resolution).
    * Parse failures are fatal — silent skips hide incomplete `.cairn` packs.
    */
  def loadRaw(dir: Path): Map[String, List[Fragment]] =
    if !Files.exists(dir) then Map.empty
    else
      import scala.jdk.CollectionConverters.*
      Files.list(dir).iterator.asScala
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
        .map { p =>
          Meta.parseLanguageAst(Files.readString(p)) match
            case Right((name, fs)) => name -> fs
            case Left(err) =>
              throw RuntimeException(s"failed to parse language pack $p: $err")
        }
        .toMap

  def loadRaw(): Map[String, List[Fragment]] =
    languageDirs.view.map(loadRaw).find(_.nonEmpty).getOrElse(Map.empty)

  /** Surfaces keyed by language name, then style name. */
  def loadSurfaces(dir: Path): Map[String, Map[String, SurfacePack]] =
    if !Files.exists(dir) then Map.empty
    else
      import scala.jdk.CollectionConverters.*
      Files.list(dir).iterator.asScala
        .filter(Files.isDirectory(_))
        .flatMap { langDir =>
          val langName = langDir.getFileName.toString
          val surfRoot = langDir.resolve("surfaces")
          if !Files.isDirectory(surfRoot) then Iterator.empty
          else
            val packs = Files.list(surfRoot).iterator.asScala
              .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".cairn"))
              .map { p =>
                Meta.parseSurfaceAst(Files.readString(p)) match
                  case Right((style, lang, fs)) =>
                    if lang != langName then
                      throw RuntimeException(
                        s"surface pack $p declares for '$lang' but lives under languages/$langName/")
                    style -> SurfacePack(style, langName, fs)
                  case Left(err) =>
                    throw RuntimeException(s"failed to parse surface pack $p: $err")
              }
              .toMap
            if packs.isEmpty then Iterator.empty else Iterator(langName -> packs)
        }
        .toMap

  def loadSurfaces(): Map[String, Map[String, SurfacePack]] =
    languageDirs.view.map(loadSurfaces).find(_.nonEmpty).getOrElse(Map.empty)

  def surfacesFor(lang: String): Map[String, SurfacePack] =
    loadSurfaces().getOrElse(lang, Map.empty)

  private def bindPack(
      name: String,
      fs: List[Fragment],
      surfaces: Map[String, Map[String, SurfacePack]],
      surfaceName: String,
  ): List[Fragment] =
    surfaces.get(name) match
      case None => fs // fused: grammar lives in the language file
      case Some(styles) =>
        val style = if styles.contains(surfaceName) then surfaceName
                    else if styles.contains(DefaultSurface) then DefaultSurface
                    else styles.keys.toList.sorted.headOption.getOrElse(surfaceName)
        styles.get(style) match
          case Some(surf) => bindSurface(fs, surf)
          case None =>
            throw RuntimeException(
              s"language '$name' has surfaces ${styles.keys.toList.sorted.mkString(", ")} but not '$surfaceName'")

  /** Resolve transitive `requires` by pulling providing packs, then compose. */
  def close(
      name: String,
      packs: Map[String, List[Fragment]],
      surfaces: Map[String, Map[String, SurfacePack]] = Map.empty,
      surface: String = DefaultSurface,
  ): Either[List[ComposeError], ComposedLanguage] =
    packs.get(name) match
      case None =>
        Left(List(ComposeError("pack", name, "-", s"language pack '$name' not found")))
      case Some(own) =>
        var selected = Map(name -> own)
        var guard = 0
        var progress = true
        while progress && guard < 32 do
          guard += 1
          progress = false
          val provided = selected.values.flatten.flatMap(_.provides).toSet
          val needed = selected.values.flatten.flatMap(_.requires).filterNot(provided.contains).toSet
          for iface <- needed do
            packs.find { (n, fs) =>
              !selected.contains(n) && fs.exists(_.provides.contains(iface))
            } match
              case Some((dep, fs)) =>
                selected = selected + (dep -> fs)
                progress = true
              case None => ()
        val fragments = selected.toList.flatMap { (n, fs) =>
          val bound = bindPack(n, fs, surfaces, surface)
          if n == name then bound else bound.map(demote)
        }
        Compose.compose(name, fragments)

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

  /** Interfaces this pack's own fragments still need after local provides. */
  def unmetRequires(name: String, packs: Map[String, List[Fragment]]): Set[String] =
    packs.get(name) match
      case None => Set.empty
      case Some(fs) =>
        val provided = fs.flatMap(_.provides).toSet
        fs.flatMap(_.requires).filterNot(provided.contains).toSet
