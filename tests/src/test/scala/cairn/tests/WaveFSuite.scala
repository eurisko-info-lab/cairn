package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.core.Ports2.*
import cairn.rosetta.Scaffold
import cairn.systemhandler.EffectContext
import cairn.examples.quicksort.QuickSort2

/** Wave F acceptance (M30–M34). */
class WaveFSuite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(300, "s")
  val m = QuickSort2.module
  val allPorts: List[PortV2] = List(ScalaPort2, LeanPort2, HaskellPort2, RustPort2)
  private val scaffoldCtx = EffectContext.forFilesystem()

  test("M30: generic Ord-constrained quicksort is one canonical artifact"):
    assertEquals(m.artifact.kind, ArtifactKind.RosettaDecl)
    assertEquals(m.artifact.digest, QuickSort2.module.artifact.digest)
    assert(m.defs.exists(d => d.typeParams == List("a") && d.constraints == List(("a", "Ord"))))
    assert(m.effects.exists(_.name == "counter"))
    assert(m.datas.nonEmpty)

  test("M31: all four ports pass the whole-file byte fixpoint"):
    for port <- allPorts do
      PortV2.verified(port, m) match
        case Right(out) =>
          assert(out.text.contains(m.artifact.digest.short), s"${port.hostName}: artifact digest missing")
        case Left(e) => fail(s"${port.hostName}: $e")

  test("M31: port file grammars lint clean"):
    for port <- allPorts do
      assertEquals(GrammarLint.errors(port.fileGrammar), Nil, s"${port.hostName} grammar lint")

  test("isSorted is one shared decision procedure across Scala/Haskell/Rust; Lean keeps its own stdlib relation"):
    // Scala/Haskell/Rust: same logical shape (nested match, <=, &&), derived
    // from Ports2's single private isSortedBody — not three independently
    // hand-written strings. Concrete Int now (not generic Ord), matching how
    // isPerm was always Int-only; the shared vocabulary doesn't need generic
    // Ordering machinery this way.
    val scala = PortV2.verified(ScalaPort2, m).toOption.get.text
    assert(scala.contains("def isSorted(xs: List[Int]): Boolean ="), scala)
    assert(scala.contains("(h <= h2)") && scala.contains("&&") && scala.contains("isSorted(xs)"), scala)
    val haskell = PortV2.verified(HaskellPort2, m).toOption.get.text
    assert(haskell.contains("isSorted :: ([Int] -> Bool)"), haskell)
    assert(haskell.contains("h <= h2") && haskell.contains("&&"), haskell)
    val rust = PortV2.verified(RustPort2, m).toOption.get.text
    assert(rust.contains("fn is_sorted(xs: &[i64]) -> bool"), rust)
    assert(rust.contains("h <= h2") && rust.contains("&&"), rust)
    // Lean untouched: still its own idiomatic stdlib-backed relation, not the
    // shared boolean decision procedure (regressing a real Prop to `= true`
    // would make Lean's port worse, not better).
    val lean = PortV2.verified(LeanPort2, m).toOption.get.text
    assert(lean.contains("List.Pairwise (fun x y => x <= y) xs"), lean)
    assert(lean.contains("listSorted (quicksort xs)"), lean)

  test("M32/M34: scala port runs — generic + effect obligations pass in host"):
    val scalaCli = sys.env.getOrElse("PATH", "").split(":").map(java.nio.file.Paths.get(_, "scala-cli"))
      .find(java.nio.file.Files.isExecutable)
    assume(scalaCli.isDefined, "scala-cli not on PATH")
    val out = PortV2.verified(ScalaPort2, m).fold(e => fail(e), identity)
    val dir = java.nio.file.Files.createTempDirectory("cairn-port2")
    val f = dir.resolve(out.fileName)
    java.nio.file.Files.writeString(f, out.text)
    val pb = new ProcessBuilder(scalaCli.get.toString, "run", "--server=false", f.toString)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val stdout = new String(proc.getInputStream.readAllBytes())
    assertEquals(proc.waitFor(), 0, s"scala-cli failed:\n$stdout\n--- source:\n${out.text}")
    assert(stdout.contains("ALL TESTS PASS"), stdout)

  test("M32: haskell port runs when runghc is available"):
    val runghc = sys.env.getOrElse("PATH", "").split(":").map(java.nio.file.Paths.get(_, "runghc"))
      .find(java.nio.file.Files.isExecutable)
    assume(runghc.isDefined, "runghc not on PATH")
    val out = PortV2.verified(HaskellPort2, m).fold(e => fail(e), identity)
    val dir = java.nio.file.Files.createTempDirectory("cairn-hs")
    val f = dir.resolve(out.fileName)
    java.nio.file.Files.writeString(f, out.text)
    val pb = new ProcessBuilder(runghc.get.toString, f.toString)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val stdout = new String(proc.getInputStream.readAllBytes())
    assertEquals(proc.waitFor(), 0, s"runghc failed:\n$stdout\n--- source:\n${out.text}")
    assert(stdout.contains("ALL TESTS PASS"), stdout)

  test("M32: rust port emits a cargo project (cargo run optional)"):
    val out = PortV2.verified(RustPort2, m).fold(e => fail(e), identity)
    assert(out.text.contains("fn quicksort<T: Ord + Clone>"), out.text)
    assert(out.text.contains("#[test] fn quicksort_sorted()"), out.text)
    val cargo = sys.env.getOrElse("PATH", "").split(":").map(java.nio.file.Paths.get(_, "cargo"))
      .find(java.nio.file.Files.isExecutable)
    assume(cargo.isDefined, "cargo not on PATH")
    val dir = java.nio.file.Files.createTempDirectory("cairn-rs")
    Scaffold.emitAll(m, dir, scaffoldCtx).fold(e => fail(e), identity)
    val pb = new ProcessBuilder(cargo.get.toString, "test", "--quiet")
    pb.directory(dir.resolve("rust").toFile)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val stdout = new String(proc.getInputStream.readAllBytes())
    assertEquals(proc.waitFor(), 0, s"cargo test failed:\n$stdout\n--- source:\n${out.text}")

  test("M33: lean skeleton has generic defs + theorem obligations"):
    val out = PortV2.verified(LeanPort2, m).fold(e => fail(e), identity)
    assert(out.text.contains("partial def quicksort {a : Type} [Ord a]"), out.text)
    assert(out.text.contains("theorem quicksort_sorted : ∀ xs : List Int, listSorted (quicksort xs) := by sorry"), out.text)
    assert(out.text.contains("(StateM Nat (List Int))"), out.text)

  test("M33: scaffolds + obligations manifest"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-scaffold")
    val projects = Scaffold.emitAll(m, dir, scaffoldCtx).fold(e => fail(e), identity)
    assertEquals(projects.map(_.host).toSet, Set("scala", "lean", "haskell", "rust"))
    assert(java.nio.file.Files.exists(dir.resolve("lean/lakefile.lean")))
    assert(java.nio.file.Files.exists(dir.resolve("rust/Cargo.toml")))
    val manifest = java.nio.file.Files.readString(dir.resolve("obligations.json"))
    // manifest is valid JSON in our own surface and covers 2 theorems x 4 hosts
    val decoded = cairn.core.JsonSurface.decode(manifest).fold(e => fail(e), identity)
    decoded match
      case Cst.Node("obligations", List(Cst.Node("list", entries))) =>
        assertEquals(entries.length, 8)
      case other => fail(s"unexpected manifest shape: ${other.render}")

  test("M34: counter effect projects to all hosts idiomatically"):
    val scala = PortV2.verified(ScalaPort2, m).toOption.get.text
    assert(scala.contains("class Counter"), scala)
    assert(scala.contains("def countingQuicksort(c: Counter, xs: List[Int]): List[Int] = { c.tick(); quicksort(xs) }"), scala)
    val lean = PortV2.verified(LeanPort2, m).toOption.get.text
    assert(lean.contains("partial def countingQuicksort (xs : List Int) : (StateM Nat (List Int)) := (do tick; pure (quicksort xs))"), lean)
    val rust = PortV2.verified(RustPort2, m).toOption.get.text
    assert(rust.contains("struct Counter"), rust)
    assert(rust.contains("fn counting_quicksort(c: &mut Counter, xs: &[i64]) -> Vec<i64>"), rust)
