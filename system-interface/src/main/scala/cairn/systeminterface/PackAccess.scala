package cairn.systeminterface

import cairn.kernel.*

/** Pure pack-loading contract (Phase 6). User languages depend on this —
  * never on `system-handler` or `runtime.PackLoader` directly.
  */
trait PackAccess:
  def requireOwn(name: String): List[Fragment]
  def requireClosed(name: String, surface: String = "default"): ComposedLanguage
  def requireSurface(lang: String, surface: String = "default"): SurfacePack
  def surfacesFor(lang: String): Map[String, SurfacePack]
  def loadRaw(): Map[String, List[Fragment]]
  def loadClosed(): Map[String, ComposedLanguage]
  def unmetRequires(name: String, packs: Map[String, List[Fragment]]): Set[String]

/** Process-global install point filled by the runtime composition root.
  * User code may read; only runtime should [[install]].
  */
object PackAccess:
  @volatile private var current: Option[PackAccess] = None

  def install(access: PackAccess): Unit = current = Some(access)

  def get: PackAccess =
    current.getOrElse(
      throw RuntimeException(
        "PackAccess not installed — ensure cairn.runtime.PackLoader is on the classpath"))
