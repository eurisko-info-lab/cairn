package cairn.tests

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** MIGRATION-PLAN.md Phase 0/6/8: mechanical regression guards for
  * forbidden-import rules and trust-boundary patterns.
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
    "java.io.RandomAccessFile",
    "scala.io.",
    "java.lang.ProcessBuilder",
    "scala.sys.process",
    "java.net.Socket",
    "java.net.URL",
    "java.net.http.HttpClient")

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

  private def codeHits(
      roots: List[Path],
      banned: List[String],
      allowFiles: Set[String] = Set.empty,
  ): List[String] =
    for
      root <- roots if Files.exists(root)
      file <- scalaFilesUnder(root)
      if !allowFiles.contains(file.getFileName.toString)
      if !file.toString.contains("ModuleBoundarySuite")
      (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
      bad <- banned.find(line.contains)
      if !line.trim.startsWith("//") && !line.trim.startsWith("*")
    yield s"${file}:${i + 1}: '$bad' — ${line.trim}"

  test("kernel imports no filesystem, networking, or process APIs"):
    val violations = importViolations(Path.of("kernel/src/main/scala"), "kernel", forbiddenIo)
    assert(violations.isEmpty, violations.mkString("\n"))

  test("core imports no filesystem, networking, or process APIs"):
    val violations = importViolations(Path.of("content/core/src/main/scala"), "core", forbiddenIo)
    assert(violations.isEmpty, violations.mkString("\n"))

  test("kernel and core do not import system-handler or runtime"):
    val banned = List("cairn.systemhandler", "cairn.runtime", "cairn.surface")
    val hits =
      importViolations(Path.of("kernel/src/main/scala"), "kernel", banned) ++
        importViolations(Path.of("content/core/src/main/scala"), "core", banned)
    assert(hits.isEmpty, hits.mkString("\n"))

  test("user imports no system-handler packages"):
    val violations = importViolations(
      Path.of("content/user/src/main/scala"), "user",
      List("cairn.systemhandler", "cairn.runtime", "java.nio.file", "java.net.", "java.lang.ProcessBuilder", "scala.sys.process"))
    assert(violations.isEmpty, violations.mkString("\n"))

  test("user module sources exist"):
    assert(Files.isDirectory(Path.of("content/user/src/main/scala/cairn/user")))

  test("no PackAccess.get/install or AuthorityGate.forFamily/default escape hatches"):
    val roots = List(
      "kernel", "container/kernel-container", "content/kernel-rewrite", "contracts",
      "container/system-interface", "container/system-handler", "app/runtime",
      "content/core", "content/user", "app/surface", "app/examples",
      "app/rosetta", "content/proof", "app/tests"
    ).map(d => Path.of(s"$d/src"))
    val banned = List(
      "PackAccess.get", "PackAccess.install",
      "AuthorityGate.forFamily", "AuthorityGate.default")
    assert(codeHits(roots, banned).isEmpty, codeHits(roots, banned).mkString("\n"))

  test("handlers do not invent Subject(\"local\") — subject comes from EffectContext"):
    val handlerRoot = Path.of("container/system-handler/src/main/scala/cairn/systemhandler")
    val allow = Set("EffectContext.scala") // composition-root factory may mint local
    val hits =
      for
        file <- scalaFilesUnder(handlerRoot)
        if !allow.contains(file.getFileName.toString)
        (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
        if line.contains("""Subject("local")""") || line.contains("""Subject('local')""")
        if !line.trim.startsWith("//") && !line.trim.startsWith("*")
      yield s"${file}:${i + 1}: ${line.trim}"
    assert(hits.isEmpty, hits.mkString("\n"))

  test("handlers do not call gate.check/checked — authorize then AuthorizedEffect.perform"):
    val handlerRoot = Path.of("container/system-handler/src/main/scala/cairn/systemhandler")
    val allow = Set(
      "AuthorityGate.scala", "EffectContext.scala",
      "AuthorizedEffect.scala", "AuditedEffect.scala")
    val hits =
      for
        file <- scalaFilesUnder(handlerRoot)
        if !allow.contains(file.getFileName.toString)
        (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
        if line.contains("gate.check") // also matches gate.checked
        if !line.trim.startsWith("//") && !line.trim.startsWith("*")
      yield s"${file}:${i + 1}: ${line.trim}"
    assert(hits.isEmpty, hits.mkString("\n"))

  test("auditPass / AuditedEffect are distinct from AuthorizedEffect at the type level"):
    val handlerRoot = Path.of("container/system-handler/src/main/scala/cairn/systemhandler")
    val hits =
      for
        file <- scalaFilesUnder(handlerRoot)
        name = file.getFileName.toString
        if !Set("AuditedEffect.scala", "EffectContext.scala", "AuthorityGate.scala").contains(name)
        (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
        if line.contains("AuditedEffect") && line.contains("perform")
        if !line.trim.startsWith("//") && !line.trim.startsWith("*")
      yield s"${file}:${i + 1}: ${line.trim}"
    assert(hits.isEmpty, hits.mkString("\n"))

  test("AuthorizedEffect.mint only from EffectContext"):
    val handlerRoot = Path.of("container/system-handler/src/main/scala/cairn/systemhandler")
    val hits =
      for
        file <- scalaFilesUnder(handlerRoot)
        name = file.getFileName.toString
        if name != "EffectContext.scala" && name != "AuthorizedEffect.scala"
        (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
        if line.contains("AuthorizedEffect.mint")
        if !line.trim.startsWith("//") && !line.trim.startsWith("*")
      yield s"${file}:${i + 1}: ${line.trim}"
    assert(hits.isEmpty, hits.mkString("\n"))

  test("handlers do not resolve EffectMeta.* static seeds for live authorize"):
    // Runtime vocabulary must come from RuntimeEffectRegistry; static EffectMeta
    // family vals are cold-start seeds only (allowed in RuntimeEffectRegistry).
    val handlerRoot = Path.of("container/system-handler/src/main/scala/cairn/systemhandler")
    val allow = Set("RuntimeEffectRegistry.scala")
    val banned = List(
      "EffectMeta.filesystem", "EffectMeta.cas", "EffectMeta.workspace",
      "EffectMeta.process", "EffectMeta.clock", "EffectMeta.random",
      "EffectMeta.terminal", "EffectMeta.lsp", "EffectMeta.externalBackend",
      "EffectMeta.ledgerTransport")
    val hits =
      for
        file <- scalaFilesUnder(handlerRoot)
        if !allow.contains(file.getFileName.toString)
        (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
        bad <- banned.find(line.contains)
        // KDoc [[EffectMeta.foo]] and prose mentions are fine
        if !line.trim.startsWith("//") && !line.trim.startsWith("*")
        if !line.contains("[[") // scaladoc link
      yield s"${file}:${i + 1}: '$bad' — ${line.trim}"
    assert(hits.isEmpty, hits.mkString("\n"))

  test("content does not import system-handler"):
    val banned = List("cairn.systemhandler")
    val hits =
      importViolations(Path.of("content/core/src/main/scala"), "core", banned) ++
        importViolations(Path.of("content/user/src/main/scala"), "user", banned) ++
        importViolations(Path.of("content/proof/src/main/scala"), "proof", banned) ++
        importViolations(Path.of("content/kernel-rewrite/src/main/scala"), "kernel-rewrite", banned)
    assert(hits.isEmpty, hits.mkString("\n"))

  test("container does not import content.user or content.proof"):
    val banned = List("cairn.user", "cairn.proof")
    val hits =
      importViolations(Path.of("container/system-handler/src/main/scala"), "system-handler", banned) ++
        importViolations(Path.of("container/system-interface/src/main/scala"), "system-interface", banned) ++
        importViolations(Path.of("container/kernel-container/src/main/scala"), "kernel-container", banned)
    assert(hits.isEmpty, hits.mkString("\n"))

  test("contracts is a pure leaf like kernel — no container/content imports"):
    val banned = List("cairn.systemhandler", "cairn.core", "cairn.user", "cairn.proof", "cairn.runtime")
    val hits = importViolations(Path.of("contracts/src/main/scala"), "contracts", banned)
    assert(hits.isEmpty, hits.mkString("\n"))

  test("container→core imports are allowlisted (shrink toward empty)"):
    // Remaining core imports keep systemHandler.dependsOn(core) alive until
    // Branches/EffectContext factories move to app/runtime and
    // AuthorityGate.DefaultProver is deleted in favor of PolicyEvalProver.
    // MetaActivation moved to app/runtime (zero real callers, trivial win).
    val allowedFiles = Set(
      "AuthorityGate.scala", // DefaultProver → PolicyEval (temporary)
      "EffectContext.scala", // PolicyEval.* factory policies
      "Cas.scala", // Branches → SemanticRepository/Delta/…
    )
    val hits =
      for
        file <- scalaFilesUnder(Path.of("container/system-handler/src/main/scala"))
        if !allowedFiles.contains(file.getFileName.toString)
        (line, i) <- Files.readAllLines(file).asScala.zipWithIndex
        if line.trim.startsWith("import") && line.contains("cairn.core")
      yield s"${file}:${i + 1}: ${line.trim}"
    assert(hits.isEmpty, hits.mkString("\n"))
