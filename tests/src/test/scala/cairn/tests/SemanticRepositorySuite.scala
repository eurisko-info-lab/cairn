package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{Branches, DiskCas, Provenance}
import java.nio.file.Files

/** End-to-end semantic repository spine: Branches + ΔL + ChangeAlgebra +
  * Merge + Migrate wired through [[SemanticRepository]].
  */
class SemanticRepositorySuite extends munit.FunSuite:
  val lang = Stlc.language
  val dl = Delta.deltaOf(lang).toOption.get
  val m0 = Module(List("a" -> Stlc.tru, "b" -> Stlc.fls))

  def parseChange(src: String): Cst =
    Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  test("spine: commit → tip → commute → integrate → accepted head"):
    val cA = parseChange("{ replace a = false ; add fromA = true ; }")
    val cB = parseChange("{ replace b = true ; add fromB = false ; }")
    assert(SemanticRepository.commutes(lang, cA, cB))
    val tipA = SemanticRepository.tipAfter(lang, m0, cA).fold(e => fail(e), identity)
    val tipB = SemanticRepository.tipAfter(lang, m0, cB).fold(e => fail(e), identity)
    assertEquals(tipA.baseDigest, tipB.baseDigest)
    SemanticRepository.integrateTips(lang, tipA, tipB) match
      case Right(SemanticRepository.Outcome.Accepted(merged, vcs, _, migrated)) =>
        assert(!migrated)
        assertEquals(merged.get("a"), Some(Stlc.fls))
        assertEquals(merged.get("b"), Some(Stlc.tru))
        assert(merged.get("fromA").isDefined && merged.get("fromB").isDefined)
        assertEquals(vcs.base, m0.digest)
        assertEquals(vcs.result, merged.digest)
      case Right(SemanticRepository.Outcome.Conflicted(c)) => fail(c.render)
      case Left(e) => fail(e)

  test("spine: overlapping edits → conflict artifact, no accept"):
    val cA = parseChange("{ replace a = false ; }")
    val cB = parseChange("{ edit a at [] = fun x : Bool . x ; }")
    assert(!SemanticRepository.commutes(lang, cA, cB))
    SemanticRepository.integrate(lang, m0, cA, cB) match
      case Right(SemanticRepository.Outcome.Conflicted(conflict)) =>
        assertEquals(conflict.overlap, Set("a"))
        assertEquals(conflict.artifact.kind, ArtifactKind.ChangeSet)
      case Right(SemanticRepository.Outcome.Accepted(_, _, _, _)) =>
        fail("expected conflict")
      case Left(e) => fail(e)

  test("Branches.merge: disjoint tips advance into a queryable new head"):
    val dir = Files.createTempDirectory("cairn-semrepo")
    val cas = DiskCas(dir.resolve("cas"))
    val branches = Branches(cas, dir.resolve("refs"))
    val cA = parseChange("{ replace a = false ; add fromA = true ; }")
    val cB = parseChange("{ replace b = true ; add fromB = false ; }")
    branches.commitModule("base", m0)
    val tipA = SemanticRepository.tipAfter(lang, m0, cA).fold(e => fail(e), identity)
    val tipB = SemanticRepository.tipAfter(lang, m0, cB).fold(e => fail(e), identity)
    branches.commitModule("feat-a", tipA.tip)
    branches.commitModule("feat-b", tipB.tip)
    branches.merge(lang, "main", m0, cA, cB) match
      case Right(Right(manifest)) =>
        assertEquals(manifest.branch, "main")
        assert(manifest.head.isDefined)
        val head = branches.headModule("main").fold(e => fail(e), identity)
        assertEquals(head.get("a"), Some(Stlc.fls))
        assertEquals(head.get("b"), Some(Stlc.tru))
        assert(head.get("fromA").isDefined && head.get("fromB").isDefined)
        assertEquals(branches.list().sorted, List("base", "feat-a", "feat-b", "main"))
        val hops = Provenance.why(dir.resolve("cas"), head.digest)
        assert(hops.exists(_.record.tool == "semantic-merge"), hops.toString)
      case Right(Left(c)) => fail(c.render)
      case Left(e) => fail(e)

  test("Branches.merge: conflict persists artifact and leaves target head unset"):
    val dir = Files.createTempDirectory("cairn-semrepo-conflict")
    val cas = DiskCas(dir.resolve("cas"))
    val branches = Branches(cas, dir.resolve("refs"))
    val cA = parseChange("{ replace a = false ; }")
    val cB = parseChange("{ edit a at [] = fun x : Bool . x ; }")
    branches.merge(lang, "main", m0, cA, cB) match
      case Right(Left(conflict)) =>
        assertEquals(conflict.overlap, Set("a"))
        assert(cas.contains(conflict.artifact.digest))
        assertEquals(branches.load("main").head, None)
      case Right(Right(_)) => fail("expected conflict")
      case Left(e) => fail(e)

  test("spine: optional migration step before accepted state"):
    val v2 = Compose.compose("stlc2", Stlc.fragments).toOption.get
    val mig = LangMigration(lang.digest, v2.digest, Map.empty, Map.empty)
    val cA = parseChange("{ replace a = false ; }")
    val cB = parseChange("{ replace b = true ; }")
    SemanticRepository.integrate(lang, m0, cA, cB, Some(mig -> v2)) match
      case Right(SemanticRepository.Outcome.Accepted(merged, vcs, _, migrated)) =>
        assert(migrated)
        assertEquals(vcs.language, v2.digest)
        assertEquals(merged.get("a"), Some(Stlc.fls))
        assertEquals(merged.get("b"), Some(Stlc.tru))
      case Right(SemanticRepository.Outcome.Conflicted(c)) => fail(c.render)
      case Left(e) => fail(e)
