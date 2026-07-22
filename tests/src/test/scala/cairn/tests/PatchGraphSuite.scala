package cairn.tests

import cairn.core.PatchGraph
import cairn.kernel.Digest
import cairn.runtime.WorkflowRunner
import cairn.systemhandler.{Branches, EffectContext, MemCas}

/** Patch DAG + bootstrap import + workflow runner (architecture priorities 4–6). */
class PatchGraphSuite extends munit.FunSuite:

  private def dig(tag: String): Digest =
    Digest.of(cairn.kernel.Canon.CStr(tag))

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
