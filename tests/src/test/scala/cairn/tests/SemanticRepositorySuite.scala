package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{Branches, CasAdminEffects, CasEffects, DiskCas, EffectContext, Provenance, Keypair, Node}
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
    branches.importModule("base", m0)
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
    // Manifest alone carries changeHistory; sidecars are caches
    val m = branches.load("feat")
    assertEquals(m.changeHistory.length, 2)
    assertEquals(m.acceptedChange, Some(tip2.vcs.artifact.digest))
    Files.deleteIfExists(dir.resolve("refs/feat.change"))
    Files.deleteIfExists(dir.resolve("refs/feat.changes"))
    val histManifest = branches.loadChangeHistory("feat", lang).fold(e => fail(e), identity)
    assertEquals(histManifest.length, 2)
    assertEquals(histManifest.last.result, tip2.tipDigest)
    val tipAlone = branches.loadTip("feat", lang).fold(e => fail(e), identity)
    assertEquals(tipAlone.tipDigest, tip2.tipDigest)

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

  test("Branches: transactional accept clears journal; recoverPendingAccepts is idle"):
    val dir = Files.createTempDirectory("cairn-txn-accept")
    val branches = branchesAt(dir)
    val tip = SemanticRepository.tipAfter(lang, m0, parseChange("{ replace a = false ; }"))
      .fold(e => fail(e), identity)
    branches.commitTip("feat", tip)
    assertEquals(branches.recoverPendingAccepts(), Right(Nil))
    assert(branches.headModule("feat").isRight)

  test("Branches.reclaimOrphanBlobs: sweeps unreferenced put; keeps live tip"):
    val dir = Files.createTempDirectory("cairn-reclaim")
    val casRoot = dir.resolve("cas")
    val cas = DiskCas(casRoot)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val tip = SemanticRepository.tipAfter(lang, m0, parseChange("{ replace a = false ; }"))
      .fold(e => fail(e), identity)
    branches.commitTip("feat", tip)
    val orphan = Artifact(ArtifactKind.Claim, Canon.CStr("orphan-accept-blob"))
    val orphanDig = CasEffects.put(cas, orphan, casCtx).fold(e => fail(e.toString), _.valueHash)
    assert(CasEffects.contains(cas, orphanDig, casCtx).contains(true))
    val before = CasAdminEffects.stats(casRoot, casCtx).fold(e => fail(e.toString), identity)
    val report = branches.reclaimOrphanBlobs(casRoot).fold(e => fail(e), identity)
    assert(report.gc.swept >= 1, report.toString)
    assert(CasEffects.contains(cas, orphanDig, casCtx).contains(false))
    assert(branches.headModule("feat").isRight)
    val after = CasAdminEffects.stats(casRoot, casCtx).fold(e => fail(e.toString), identity)
    assert(after.objects < before.objects)

  test("Branches.merge: conflict writes .conflict sidecar as live root"):
    val dir = Files.createTempDirectory("cairn-conflict-root")
    val branches = branchesAt(dir)
    val cA = parseChange("{ replace a = false ; }")
    val cB = parseChange("{ edit a at [] = fun x : Bool . x ; }")
    branches.merge(lang, "main", m0, cA, cB) match
      case Right(Left(conflict)) =>
        val roots = branches.liveCasRoots().fold(e => fail(e), identity)
        assert(roots.contains(conflict.artifact.digest), roots.toString)
      case Right(Right(_)) => fail("expected conflict")
      case Left(e) => fail(e)

  test("Branches.mergeBranches: causal LCA by shared result (not only identical changes)"):
    // Two histories that reach the same intermediate module via different
    // change objects (commuting edits applied in opposite order), then diverge.
    val dir = Files.createTempDirectory("cairn-semrepo-lca")
    val branches = branchesAt(dir)
    val cA = parseChange("{ replace a = false ; }")
    val cB = parseChange("{ replace b = true ; }")
    // Path 1: a then b
    val tip1a = SemanticRepository.tipAfter(lang, m0, cA).fold(e => fail(e), identity)
    val tip1ab = SemanticRepository.tipAfter(lang, tip1a.tip, cB).fold(e => fail(e), identity)
    // Path 2: b then a — same tip module digest as tip1ab if they commute
    assert(SemanticRepository.commutes(lang, cA, cB))
    val tip2b = SemanticRepository.tipAfter(lang, m0, cB).fold(e => fail(e), identity)
    val tip2ba = SemanticRepository.tipAfter(lang, tip2b.tip, cA).fold(e => fail(e), identity)
    assertEquals(tip1ab.tipDigest, tip2ba.tipDigest)
    // Divergent suffixes from the shared module state
    val cFromA = parseChange("{ add fromA = true ; }")
    val cFromB = parseChange("{ add fromB = false ; }")
    val tipA = SemanticRepository.tipAfter(lang, tip1ab.tip, cFromA).fold(e => fail(e), identity)
    val tipB = SemanticRepository.tipAfter(lang, tip2ba.tip, cFromB).fold(e => fail(e), identity)
    branches.commitTip("feat-a", tip1a)
    branches.commitTip("feat-a", tip1ab)
    branches.commitTip("feat-a", tipA)
    branches.commitTip("feat-b", tip2b)
    branches.commitTip("feat-b", tip2ba)
    branches.commitTip("feat-b", tipB)
    // Linear identical-prefix would be 0 (change objects differ); LCA by result finds the shared module.
    branches.mergeBranches(lang, "main", "feat-a", "feat-b") match
      case Right(Right(_)) =>
        val head = branches.headModule("main").fold(e => fail(e), identity)
        assertEquals(head.get("a"), Some(Stlc.fls))
        assertEquals(head.get("b"), Some(Stlc.tru))
        assert(head.get("fromA").isDefined && head.get("fromB").isDefined)
      case Right(Left(c)) => fail(c.render)
      case Left(e) => fail(e)

  test("property: causal-LCA merge over nontrivial DAG shapes (seeded)"):
    // For each seed, build a diamond/fork DAG where two branches reach a shared
    // intermediate module via commuting edits in opposite order, then diverge
    // with disjoint adds. Merge must accept and contain every suffix binding.
    // Trials 0..23: classic diamond. Trials 24..47: deeper optional mid commits
    // plus a third commuting replace before the fork (wider DAG).
    val rng = new scala.util.Random(20260722L)
    var accepted = 0
    for trial <- 0 until 48 do
      val dir = Files.createTempDirectory(s"cairn-lca-prop-$trial")
      val branches = branchesAt(dir)
      val names = (0 until (if trial < 24 then 4 else 5)).map(i => s"v${trial}_$i").toList
      val baseDefs = names.map(n => n -> (if rng.nextBoolean() then Stlc.tru else Stlc.fls))
      val base = Module(baseDefs)
      branches.importModule("root", base)
      val pair = names.take(2)
      val cA = parseChange(s"{ replace ${pair(0)} = false ; }")
      val cB = parseChange(s"{ replace ${pair(1)} = true ; }")
      assert(SemanticRepository.commutes(lang, cA, cB), clues(trial, pair))
      val tip1a = SemanticRepository.tipAfter(lang, base, cA).fold(e => fail(e), identity)
      val tip1ab = SemanticRepository.tipAfter(lang, tip1a.tip, cB).fold(e => fail(e), identity)
      val tip2b = SemanticRepository.tipAfter(lang, base, cB).fold(e => fail(e), identity)
      val tip2ba = SemanticRepository.tipAfter(lang, tip2b.tip, cA).fold(e => fail(e), identity)
      assertEquals(tip1ab.tipDigest, tip2ba.tipDigest)
      val shared = tip1ab.tip
      val afterShared =
        if trial >= 24 then
          val cC = parseChange(s"{ replace ${names(2)} = false ; }")
          val tipC = SemanticRepository.tipAfter(lang, shared, cC).fold(e => fail(e), identity)
          tipC.tip
        else shared
      val sufA = s"sufA$trial"
      val sufB = s"sufB$trial"
      val tipA = SemanticRepository.tipAfter(lang, afterShared,
        parseChange(s"{ add $sufA = true ; }")).fold(e => fail(e), identity)
      val tipB = SemanticRepository.tipAfter(lang, afterShared,
        parseChange(s"{ add $sufB = false ; }")).fold(e => fail(e), identity)
      if rng.nextBoolean() then
        branches.commitTip(s"a$trial", tip1a)
      branches.commitTip(s"a$trial", tip1ab)
      if trial >= 24 then
        val midA = SemanticRepository.tipAfter(lang, tip1ab.tip,
          parseChange(s"{ replace ${names(2)} = false ; }")).fold(e => fail(e), identity)
        branches.commitTip(s"a$trial", midA)
      branches.commitTip(s"a$trial", tipA)
      if rng.nextBoolean() then
        branches.commitTip(s"b$trial", tip2b)
      branches.commitTip(s"b$trial", tip2ba)
      if trial >= 24 then
        val midB = SemanticRepository.tipAfter(lang, tip2ba.tip,
          parseChange(s"{ replace ${names(2)} = false ; }")).fold(e => fail(e), identity)
        branches.commitTip(s"b$trial", midB)
      branches.commitTip(s"b$trial", tipB)
      branches.mergeBranches(lang, s"m$trial", s"a$trial", s"b$trial") match
        case Right(Right(_)) =>
          val head = branches.headModule(s"m$trial").fold(e => fail(e), identity)
          assertEquals(head.get(pair(0)), Some(Stlc.fls))
          assertEquals(head.get(pair(1)), Some(Stlc.tru))
          assert(head.get(sufA).isDefined && head.get(sufB).isDefined)
          accepted += 1
        case Right(Left(c)) => fail(s"trial $trial conflict: ${c.render}")
        case Left(e) => fail(s"trial $trial: $e")
    assertEquals(accepted, 48)

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
