package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.ledger.Keypair
import cairn.systemhandler.{Branches, CasEffects, DiskCas, EffectContext, Provenance}
import java.nio.file.Files

/** End-to-end semantic repository spine: Branches + ΔL + ChangeAlgebra +
  * Merge + Migrate wired through [[SemanticRepository]].
  */
class SemanticRepositorySuite extends munit.FunSuite:
  val lang = Stlc.language
  val dl = Delta.deltaOf(lang).toOption.get
  val m0 = Module(List("a" -> Stlc.tru, "b" -> Stlc.fls))
  private val casCtx = EffectContext.forBranches()

  def parseChange(src: String): Cst =
    Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  def branchesAt(dir: java.nio.file.Path): Branches =
    Branches(DiskCas(dir.resolve("cas")), dir.resolve("refs"), casCtx)

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
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val cA = parseChange("{ replace a = false ; add fromA = true ; }")
    val cB = parseChange("{ replace b = true ; add fromB = false ; }")
    branches.commitModule("base", m0)
    val tipA = SemanticRepository.tipAfter(lang, m0, cA).fold(e => fail(e), identity)
    val tipB = SemanticRepository.tipAfter(lang, m0, cB).fold(e => fail(e), identity)
    branches.commitTip("feat-a", tipA)
    branches.commitTip("feat-b", tipB)
    branches.mergeBranches(lang, "main", "feat-a", "feat-b") match
      case Right(Right(manifest)) =>
        assertEquals(manifest.branch, "main")
        assert(manifest.head.isDefined)
        val head = branches.headModule("main").fold(e => fail(e), identity)
        assertEquals(head.get("a"), Some(Stlc.fls))
        assertEquals(head.get("b"), Some(Stlc.tru))
        assert(head.get("fromA").isDefined && head.get("fromB").isDefined)
        assertEquals(branches.list().sorted, List("base", "feat-a", "feat-b", "main"))
        val hops = Provenance.why(dir.resolve("cas"), head.digest, casCtx)
          .fold(e => fail(e), identity)
        assert(hops.exists(_.record.tool == "semantic-merge"), hops.toString)
      case Right(Left(c)) => fail(c.render)
      case Left(e) => fail(e)

  test("Branches.publishHead: optional ledger SetBranchHead after accept"):
    val dir = Files.createTempDirectory("cairn-semrepo-pub")
    val cas = DiskCas(dir.resolve("cas"))
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val cA = parseChange("{ replace a = false ; }")
    val tipA = SemanticRepository.tipAfter(lang, m0, cA).fold(e => fail(e), identity)
    branches.commitTip("feat", tipA)
    branches.merge(lang, "main", m0, cA, parseChange("{ replace b = true ; }")) match
      case Right(Right(_)) =>
        val alice = Keypair.dev("alice")
        val auth = Map("alice" -> alice.publicBytes)
        val node = cairn.systemhandler.Node(dir.resolve("ledger"), EffectContext.forLedger())
        node.append(alice, auth, List(alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))))
          .fold(e => fail(e), identity)
        // Accept is local-only: heads stay empty until explicit publishHead
        assert(!node.state(auth).fold(e => fail(e), _.heads.contains("main")))
        branches.publishHead("main", node, alice, auth).fold(e => fail(e), identity)
        val st = node.state(auth).fold(e => fail(e), identity)
        assert(st.heads.contains("main"), s"heads=${st.heads}")
        assertEquals(st.heads("main"), branches.load("main").head.get)
      case Right(Left(c)) => fail(c.render)
      case Left(e) => fail(e)

  test("Branches.loadTip / loadChangeHistory reconstruct from sidecars"):
    val dir = Files.createTempDirectory("cairn-semrepo-hist")
    val cas = DiskCas(dir.resolve("cas"))
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val c1 = parseChange("{ replace a = false ; }")
    val tip1 = SemanticRepository.tipAfter(lang, m0, c1).fold(e => fail(e), identity)
    branches.commitTip("feat", tip1)
    val c2 = parseChange("{ replace b = true ; }")
    val tip2 = SemanticRepository.tipAfter(lang, tip1.tip, c2).fold(e => fail(e), identity)
    // Second tip on same branch: history log grows; tip sidecar is latest
    branches.commitTip("feat", tip2)
    val loaded = branches.loadTip("feat", lang).fold(e => fail(e), identity)
    assertEquals(loaded.tipDigest, tip2.tipDigest)
    assertEquals(loaded.baseDigest, tip2.baseDigest)
    val hist = branches.loadChangeHistory("feat", lang).fold(e => fail(e), identity)
    assertEquals(hist.length, 2)
    assertEquals(hist.last.result, tip2.tipDigest)

  test("Branches.mergeBranches: stacked histories compose (not tip-only)"):
    val dir = Files.createTempDirectory("cairn-semrepo-stack")
    val branches = branchesAt(dir)
    val cA1 = parseChange("{ replace a = false ; }")
    val cA2 = parseChange("{ add fromA = true ; }")
    val cB1 = parseChange("{ replace b = true ; }")
    val cB2 = parseChange("{ add fromB = false ; }")
    val tipA1 = SemanticRepository.tipAfter(lang, m0, cA1).fold(e => fail(e), identity)
    val tipA2 = SemanticRepository.tipAfter(lang, tipA1.tip, cA2).fold(e => fail(e), identity)
    val tipB1 = SemanticRepository.tipAfter(lang, m0, cB1).fold(e => fail(e), identity)
    val tipB2 = SemanticRepository.tipAfter(lang, tipB1.tip, cB2).fold(e => fail(e), identity)
    branches.commitTip("feat-a", tipA1)
    branches.commitTip("feat-a", tipA2)
    branches.commitTip("feat-b", tipB1)
    branches.commitTip("feat-b", tipB2)
    // Tip-only would fail: tipA2.base = tipA1.tip ≠ tipB2.base = tipB1.tip
    assertNotEquals(tipA2.baseDigest, tipB2.baseDigest)
    branches.mergeBranches(lang, "main", "feat-a", "feat-b") match
      case Right(Right(_)) =>
        val head = branches.headModule("main").fold(e => fail(e), identity)
        assertEquals(head.get("a"), Some(Stlc.fls))
        assertEquals(head.get("b"), Some(Stlc.tru))
        assert(head.get("fromA").isDefined && head.get("fromB").isDefined)
      case Right(Left(c)) => fail(c.render)
      case Left(e) => fail(e)

  test("Branches.merge: conflict persists artifact and leaves target head unset"):
    val dir = Files.createTempDirectory("cairn-semrepo-conflict")
    val cas = DiskCas(dir.resolve("cas"))
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val cA = parseChange("{ replace a = false ; }")
    val cB = parseChange("{ edit a at [] = fun x : Bool . x ; }")
    branches.merge(lang, "main", m0, cA, cB) match
      case Right(Left(conflict)) =>
        assertEquals(conflict.overlap, Set("a"))
        assert(CasEffects.contains(cas, conflict.artifact.digest, casCtx).contains(true))
        assertEquals(branches.load("main").head, None)
      case Right(Right(_)) => fail("expected conflict")
      case Left(e) => fail(e)

  test("ValidatedTip forgery: claimed tip digest must match apply"):
    val cA = parseChange("{ replace a = false ; }")
    val real = SemanticRepository.tipAfter(lang, m0, cA).fold(e => fail(e), identity)
    val forged = SemanticRepository.Tip(m0, m0, cA) // tip == base, not apply result
    assert(SemanticRepository.ValidatedTip.check(lang, forged).isLeft)
    assert(SemanticRepository.ValidatedTip.check(lang, real.asTip).isRight)
    // ValidatedChangeSet.decodeClaim alone is not validated
    val claim = Delta.ValidatedChangeSet.Claim(lang.digest, m0.digest, cA, m0.digest)
    assert(Delta.ValidatedChangeSet.check(lang, m0, claim).isLeft)
    val okClaim = real.vcs.claim
    assert(Delta.ValidatedChangeSet.check(lang, m0, okClaim).isRight)

  test("Branches.commitTip records causal digests on BranchManifest"):
    val dir = Files.createTempDirectory("cairn-manifest-causal")
    val branches = branchesAt(dir)
    val tip = SemanticRepository.tipAfter(lang, m0, parseChange("{ replace a = false ; }"))
      .fold(e => fail(e), identity)
    val m = branches.commitTip("feat", tip)
    assert(m.acceptedChange.isDefined, m.toString)
    assertEquals(m.causalHistoryRoot, Some(m0.digest))
    assertEquals(m.acceptedChange, Some(tip.vcs.artifact.digest))

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
