package cairn.tests

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** MIGRATION-PLAN.md Phase 0: mechanical regression guards for forbidden-import
  * rules that are checkable today — kernel and core must not import
  * filesystem, networking, or process APIs. `user`/`system-handler` rules
  * (§ docs/architecture.md) aren't fully enforceable yet (`user` does not
  * exist; system-handler is allowed I/O by design).
  *
  * Kernel's current `java.io.{ByteArrayOutputStream,DataOutputStream}`
  * (in-memory buffers, `Canon.scala`), `java.nio.charset.StandardCharsets`
  * (in-memory encoding, `Canon.scala`), and `java.security.MessageDigest`
  * (in-memory hashing, `Artifact.scala`) are all legitimate and NOT flagged
  * — this checks for actual filesystem/network/process I/O, not `java.io`/
  * `java.nio` wholesale.
  */
class ModuleBoundarySuite extends munit.FunSuite:

  private val forbiddenIo = List(
    "java.nio.file",
    "java.net.",
    "java.io.File",
    "java.io.FileInputStream",
    "java.io.FileOutputStream",
    "java.io.FileReader",
    "java.io.FileWriter",
    "java.lang.ProcessBuilder",
    "scala.sys.process")

  private def scalaFilesUnder(root: Path): List[Path] =
    if !Files.exists(root) then Nil
    else Files.walk(root).iterator().asScala.filter(p => p.toString.endsWith(".scala")).toList

  private def importViolations(srcRoot: Path, label: String): List[String] =
    val files = scalaFilesUnder(srcRoot)
    assert(files.nonEmpty, s"expected $label sources under $srcRoot, found none — check working directory")
    for
      file <- files
      (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
      if line.trim.startsWith("import")
      bad <- forbiddenIo.find(line.contains)
    yield s"${file}:${i + 1}: imports '$bad' — ${line.trim}"

  test("kernel imports no filesystem, networking, or process APIs"):
    val violations = importViolations(Path.of("kernel/src/main/scala"), "kernel")
    assert(violations.isEmpty, violations.mkString("\n"))

  test("core imports no filesystem, networking, or process APIs"):
    val violations = importViolations(Path.of("core/src/main/scala"), "core")
    assert(violations.isEmpty, violations.mkString("\n"))
