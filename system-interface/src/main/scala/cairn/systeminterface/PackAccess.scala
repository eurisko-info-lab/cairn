package cairn.systeminterface

import cairn.kernel.*

/** Pure pack-loading contract (Phase 6). User languages depend on this —
  * never on `system-handler` or `runtime.PackLoader` directly.
  *
  * There is no process-global install/`get`: composition roots construct a
  * `PackAccess` implementation (typically `runtime.PackLoader`) and pass it
  * explicitly into User language packs and other consumers.
  */
trait PackAccess:
  def requireOwn(name: String): List[Fragment]
  def requireClosed(name: String, surface: String = "default"): ComposedLanguage
  def requireSurface(lang: String, surface: String = "default"): SurfacePack
  def surfacesFor(lang: String): Map[String, SurfacePack]
  def loadRaw(): Map[String, List[Fragment]]
  def loadClosed(): Map[String, ComposedLanguage]
  def unmetRequires(name: String, packs: Map[String, List[Fragment]]): Set[String]
