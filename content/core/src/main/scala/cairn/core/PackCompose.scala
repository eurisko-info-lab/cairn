package cairn.core

import cairn.kernel.*

/** Pure pack-composition algorithm (MIGRATION-PLAN.md Phase 2, third slice —
  * "Core Meta: fragment composition algorithm"), moved out of
  * `workbench.PackLoader`. Operates only on already-in-memory `Fragment`/
  * `SurfacePack` data — no I/O — and calls `kernel.Compose.compose`,
  * Core→Kernel, the correct direction. `workbench.PackLoader` keeps the
  * exact same public names as thin delegators to this object, so no
  * external call site changes.
  */
object PackCompose:
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

  /** Interfaces this pack's own fragments still need after local provides. */
  def unmetRequires(name: String, packs: Map[String, List[Fragment]]): Set[String] =
    packs.get(name) match
      case None => Set.empty
      case Some(fs) =>
        val provided = fs.flatMap(_.provides).toSet
        fs.flatMap(_.requires).filterNot(provided.contains).toSet
