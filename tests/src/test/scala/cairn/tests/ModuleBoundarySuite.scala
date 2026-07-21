package cairn.tests

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** MIGRATION-PLAN.md Phase 0: a mechanical regression guard for the one
  * forbidden-import rule that's checkable today — "kernel must not import
  * java.nio.file, networking, or process APIs" — since `kernel` is the only
  * one of the plan's five target areas that already exists as its own
  * module. `core`/`user`/`system-handler`'s rules (§ docs/architecture.md)
  * aren't enforced here because those modules don't exist yet.
  *
  * Kernel's current `java.io.{ByteArrayOutputStream,DataOutputStream}`
  * (in-memory buffers, `Canon.scala`), `java.nio.charset.StandardCharsets`
  * (in-memory encoding, `Canon.scala`), and `java.security.MessageDigest`
  * (in-memory hashing, `Artifact.scala`) are all legitimate and NOT flagged
  * — this checks for actual filesystem/network/process I/O, not `java.io`/
  * `java.nio` wholesale.
  */
class ModuleBoundarySuite extends munit.FunSuite:

  private val forbiddenInKernel = List(
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

  test("kernel imports no filesystem, networking, or process APIs"):
    val kernelSrc = Path.of("kernel/src/main/scala")
    val files = scalaFilesUnder(kernelSrc)
    assert(files.nonEmpty, s"expected kernel sources under $kernelSrc, found none — check working directory")
    val violations = for
      file <- files
      (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
      if line.trim.startsWith("import")
      bad <- forbiddenInKernel.find(line.contains)
    yield s"${file}:${i + 1}: imports '$bad' — ${line.trim}"
    assert(violations.isEmpty, violations.mkString("\n"))
