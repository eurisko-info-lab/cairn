package cairn.tests

import cairn.core.{ChangeAlgebra, Delta, Module, PatchGraph}
import cairn.examples.stlc.Stlc
import cairn.kernel.{Cst, Digest}
import cairn.runtime.{Branches, WorkflowRunner}
import cairn.systemhandler.{EffectContext, MemCas}

/** Patch DAG + bootstrap import + workflow runner (architecture priorities 4–6). */
class PatchGraphSuite extends munit.FunSuite:

  private def dig(tag: String): Digest =
    Digest.of(cairn.kernel.Canon.CStr(tag))

  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).toOption.get
  private def parseChange(src: String): Cst =
    cairn.core.Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  test("PatchGraph: linear chain + diamond LCA"):
    val root = dig("root-change")
    val left = dig("left-change")
    val right = dig("right-change")
    val base = dig("base-mod")
    val r1 = dig("result-1")
    val r2 = dig("result-2")
    val r3 = dig("result-3")
    val g0 = PatchGraph.Graph.empty
      .add(PatchGraph.Node(root, Nil, base, r1)).toOption.get
      .add(PatchGraph.Node(left, List(root), r1, r2)).toOption.get
      .add(PatchGraph.Node(right, List(root), r1, r3)).toOption.get
    assertEquals(g0.lca(left, right), Some(root))
    assertEquals(g0.ancestors(left), Set(root))
    assert(g0.add(PatchGraph.Node(root, Nil, base, r1)).isLeft) // duplicate
    assert(g0.add(PatchGraph.Node(dig("orphan"), List(dig("missing")), base, r1)).isLeft)
    val round = PatchGraph.Graph.fromCanon(g0.canon).fold(e => fail(e), identity)
    assertEquals(round.lca(left, right), Some(root))

  test("PatchGraph.linear builds ordered parent edges"):
    val a = dig("a"); val b = dig("b"); val c = dig("c")
    val g = PatchGraph.Graph.linear(List(
      (a, dig("b0"), dig("r0")),
      (b, dig("r0"), dig("r1")),
      (c, dig("r1"), dig("r2")))).fold(e => fail(e), identity)
    assertEquals(g.get(b).map(_.parents), Some(List(a)))
    assertEquals(g.lca(b, c), Some(b))

  test("Branches.importModule is bootstrap/import — no ValidatedChangeSet required"):
    val cas = MemCas()
    val refs = java.nio.file.Files.createTempDirectory("cairn-import")
    val branches = Branches(cas, refs, EffectContext.forBranches())
    val mod = cairn.core.Module(List("x" -> cairn.kernel.Cst.Leaf("1")))
    val m = branches.importModule("boot", mod)
    assertEquals(m.head.map(_.valueHash), Some(mod.digest))
    assertEquals(m.changeHistory, Nil) // no ΔL acceptance
    assert(m.acceptedChange.isEmpty)

  test("WorkflowRunner sequences language steps; fails closed"):
    val steps = List(
      WorkflowRunner.Step("author", "write"),
      WorkflowRunner.Step("shadow", "write"),
      WorkflowRunner.Step("publish", "ledger"))
    val ok = WorkflowRunner.run(steps, s => Right(s.name)).fold(e => fail(e), identity)
    assertEquals(ok.completed, List("author", "shadow", "publish"))
    val frag = WorkflowRunner.runFragment(steps, "author", "shadow", s => Right("ok"))
      .fold(e => fail(e), identity)
    assertEquals(frag.completed, List("author", "shadow"))
    assert(WorkflowRunner.run(steps, s =>
      if s.name == "shadow" then Left("boom") else Right("ok")).isLeft)

  test("PatchGraph.commuteOk + inverseStep deepen ChangeAlgebra bridge"):
    val m0 = Module(List("a" -> Stlc.tru, "b" -> Stlc.fls))
    val chA = parseChange("{ replace a = false ; }")
    val chB = parseChange("{ replace b = true ; }")
    val chOverlap = parseChange("{ remove a ; }")
    assert(PatchGraph.commuteOk(lang, chA, chB))
    assert(!PatchGraph.commuteOk(lang, chA, chOverlap))
    assert(ChangeAlgebra.commutes(lang, chA, chB))
    val (fwd, fwdVcs) = Delta.apply(lang, m0, chA).toOption.get
    val inv = PatchGraph.inverseStep(lang, m0, chA, fwdVcs.artifact.digest)
      .fold(e => fail(e), identity)
    assertEquals(inv._1.result, m0.sorted.digest)
    assertEquals(inv._1.parents, List(fwdVcs.artifact.digest))
    val g = PatchGraph.Graph.empty
      .add(PatchGraph.Node(fwdVcs.artifact.digest, Nil, m0.digest, fwd.digest)).toOption.get
      .add(inv._1).toOption.get
    assertEquals(g.lca(fwdVcs.artifact.digest, inv._1.id), Some(fwdVcs.artifact.digest))
    // Multi-parent merge node
    val left = dig("L"); val right = dig("R"); val merge = dig("M")
    val g2 = PatchGraph.Graph.empty
      .add(PatchGraph.Node(dig("root"), Nil, dig("b0"), dig("r0"))).toOption.get
      .add(PatchGraph.Node(left, List(dig("root")), dig("r0"), dig("rL"))).toOption.get
      .add(PatchGraph.Node(right, List(dig("root")), dig("r0"), dig("rR"))).toOption.get
      .add(PatchGraph.mergeNode(merge, List(left, right), dig("r0"), dig("rM"))).toOption.get
    assertEquals(g2.lca(left, right), Some(dig("root")))
    assertEquals(g2.lca(merge, left), Some(left))
