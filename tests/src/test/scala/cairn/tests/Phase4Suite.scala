package cairn.tests

import cairn.rosetta.*
import cairn.examples.quicksort.QuickSort

/** Phase 4 acceptance (S30–S34): one artifact emits two ports; the Scala port
  * builds and its generated tests pass in the host.
  */
class Phase4Suite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(180, "s")

  test("rosetta module is a canonical artifact (S30)"):
    val a = QuickSort.module.artifact
    assertEquals(a.digest, QuickSort.module.artifact.digest)
    assertEquals(a.kind.name, "rosetta-decl")

  test("scala port emits round-trip-verified declarations (S31/S32)"):
    ScalaPort.emit(QuickSort.module) match
      case Right(text) =>
        assert(text.contains("def quicksort(xs: List[Int]): List[Int] ="), text)
        assert(text.contains("artifact " + QuickSort.module.artifact.digest.short))
        assert(text.contains("quicksort_sorted"))
      case Left(e) => fail(e)

  test("lean port emits skeleton + theorem statements (S31/S33)"):
    LeanPort.emit(QuickSort.module) match
      case Right(text) =>
        assert(text.contains("partial def quicksort (xs : List Int) : List Int :="), text)
        assert(text.contains("theorem quicksort_sorted : ∀ xs : List Int, listSorted (quicksort xs) := by sorry"), text)
        assert(text.contains("theorem quicksort_perm : ∀ xs : List Int, listPerm xs (quicksort xs) := by sorry"), text)
        assert(text.contains("namespace Quicksort"))
      case Left(e) => fail(e)

  test("ports do not fork semantics: same artifact digest cited by both (S31)"):
    val d = QuickSort.module.artifact.digest.short
    for port <- List(ScalaPort, LeanPort) do
      assert(port.emit(QuickSort.module).toOption.get.contains(d))

  test("generated scala port compiles and its tests pass (S32/S34 acceptance)"):
    val scalaCli = sys.env.getOrElse("PATH", "").split(":").map(java.nio.file.Paths.get(_, "scala-cli"))
      .find(java.nio.file.Files.isExecutable)
    assume(scalaCli.isDefined, "scala-cli not on PATH; skipping host-run check")
    val dir = java.nio.file.Files.createTempDirectory("cairn-port")
    val f = dir.resolve("quicksort.scala")
    java.nio.file.Files.writeString(f, ScalaPort.emit(QuickSort.module).toOption.get)
    val pb = new ProcessBuilder(scalaCli.get.toString, "run", "--server=false", f.toString)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val out = new String(proc.getInputStream.readAllBytes())
    val code = proc.waitFor()
    assertEquals(code, 0, s"scala-cli failed:\n$out")
    assert(out.contains("ALL TESTS PASS"), out)
