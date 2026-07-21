package cairn.tests

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** MIGRATION-PLAN.md Phase 0/6/8: mechanical regression guards for
  * forbidden-import rules.
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

  private def importViolations(srcRoot: Path, label: String, forbidden: List[String]): List[String] =
    val files = scalaFilesUnder(srcRoot)
    assert(files.nonEmpty, s"expected $label sources under $srcRoot, found none — check working directory")
    for
      file <- files
      (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
      if line.trim.startsWith("import")
      bad <- forbidden.find(line.contains)
    yield s"${file}:${i + 1}: imports '$bad' — ${line.trim}"

  test("kernel imports no filesystem, networking, or process APIs"):
    val violations = importViolations(Path.of("kernel/src/main/scala"), "kernel", forbiddenIo)
    assert(violations.isEmpty, violations.mkString("\n"))

  test("core imports no filesystem, networking, or process APIs"):
    val violations = importViolations(Path.of("core/src/main/scala"), "core", forbiddenIo)
    assert(violations.isEmpty, violations.mkString("\n"))

  test("user imports no system-handler packages"):
    val violations = importViolations(
      Path.of("user/src/main/scala"), "user",
      List("cairn.systemhandler", "java.nio.file", "java.net.", "java.lang.ProcessBuilder", "scala.sys.process"))
    assert(violations.isEmpty, violations.mkString("\n"))

  test("user module sources exist"):
    assert(Files.isDirectory(Path.of("user/src/main/scala/cairn/user")))
