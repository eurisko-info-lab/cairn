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
    current match
      case Some(a) => a
      case None =>
        // Forces runtime.PackLoader's class init (and its `install(this)`
        // side effect) without system-interface depending on runtime at
        // compile time — the only such mechanism available given that
        // constraint. Removed once on the mistaken belief every real call
        // path already triggers this some other way; restored once a full
        // call-graph audit found the real mechanism was accidental
        // JVM-wide class-init ordering (e.g. one test class happening to
        // touch PackLoader before another needs it), not a real guarantee.
        try Class.forName("cairn.runtime.PackLoader$")
        catch case _: ClassNotFoundException => ()
        current.getOrElse(
          throw RuntimeException(
            "PackAccess not installed — ensure cairn.runtime.PackLoader is on the classpath"))
