package cairn.systemhandler

import java.nio.file.{Files, Path}

/** Generic filesystem effects (MIGRATION-PLAN.md Phase 3's eventual
  * "filesystem" effect family) — a minimal first cut, not the full
  * interface/handler split Phase 3 describes, since nothing needs it
  * swappable yet. Domain-agnostic on purpose: just `Path`/`String`, no
  * `rosetta`/`Fragment` types, matching `PackFiles`' precedent.
  */
object Filesystem:
  def mkdirs(dir: Path): Unit = Files.createDirectories(dir)

  /** Write `content` to `path`, creating parent directories if needed. */
  def writeFile(path: Path, content: String): Unit =
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.writeString(path, content)
