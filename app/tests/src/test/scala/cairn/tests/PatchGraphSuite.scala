package cairn.tests
import cairn.runtime.EffectContexts

import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.kernel.{Canon, Cst, Digest}
import cairn.runtime.{Branches, ConflictResolutionOutcome, WorkflowRunner, ResolvedApplication}
import cairn.systemhandler.MemCas

/** Patch DAG + bootstrap import + workflow runner (architecture priorities 4–6). */
class PatchGraphSuite extends munit.FunSuite:

  private def dig(tag: String): Digest =
    Digest.of(cairn.kernel.Canon.CStr(tag))

  private def causal(label: String, deps: Set[Digest] = Set.empty,
      context: List[ContextDependency] = Nil, resolves: Option[Digest] = None): CausalChange =
    CausalChange(dig(s"vcs-$label"), deps, context, dig(s"base-$label"),
      dig(s"result-$label"), dig("runtime"), resolves)

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
    val branches = Branches(cas, refs, EffectContexts.forBranches())
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

  test("PR27 native graph admits explicit change and semantic-context dependencies"):
    val root = causal("root")
    val location = SemanticLocation.WholeDefinition("sheet")
    val child = causal("child", Set(root.id), List(ContextDependency(location, Set(root.id))))
    val graph = NativeRepository.empty.add(root).flatMap(_.add(child)).fold(e => fail(e), identity)
    assertEquals(graph.ancestors(Set(child.id)), Set(root.id, child.id))
    assert(graph.add(causal("bad", context = List(ContextDependency(location, Set(root.id))))).isLeft)
    assertEquals(NativeRepository.fromArtifact(graph.artifact), Right(graph))

  test("PR27 partial application retains unavailable changes and resumes when dependencies arrive"):
    val root = causal("partial-root")
    val child = causal("partial-child", Set(root.id))
    val first = NativeRepository.empty.offer(List(child)).fold(e => fail(e), identity)
    assertEquals(first._2.applied, Nil)
    assertEquals(first._2.pending, Set(child.id))
    assertEquals(first._2.missing, Set(root.id))
    val second = first._1.offer(List(root)).fold(e => fail(e), identity)
    assertEquals(second._2.applied, List(root.id, child.id))
    assertEquals(second._1.pending, Map.empty)

  test("PR27 conflicts remain graph state and resolution is an ordinary dependent change"):
    val left = causal("conflict-left")
    val right = causal("conflict-right")
    val conflictId = dig("conflict-artifact")
    val location = SemanticLocation.WholeDefinition("sheet")
    val graph = NativeRepository.empty.add(left).flatMap(_.add(right)).flatMap(_.recordConflict(
      RepositoryConflict(conflictId, Set(left.id, right.id), Set(location)))).fold(e => fail(e), identity)
    assert(graph.addResolution(causal("invalid-resolution", Set(left.id), resolves = Some(conflictId))).isLeft)
    val resolution = causal("resolution", Set(left.id, right.id), resolves = Some(conflictId))
    val resolved = graph.addResolution(resolution).fold(e => fail(e), identity)
    assertEquals(resolved.conflicts(conflictId).resolution, Some(resolution.id))
    assertEquals(resolved.conflicts(conflictId).unresolved, Set.empty)

  test("PR27 pull and push transfer causal closures; heads are views and GC roots come from graph"):
    val root = causal("transfer-root")
    val child = causal("transfer-child", Set(root.id))
    val graph = NativeRepository.empty.add(root).flatMap(_.add(child))
      .flatMap(_.setHeads("main", Set(child.id))).fold(e => fail(e), identity)
    val payload = graph.transfer(Set(child.id), Set.empty).fold(e => fail(e), identity)
    assertEquals(payload.map(_.id), List(root.id, child.id))
    val receiver = NativeRepository.empty.offer(payload).fold(e => fail(e), identity)._1
    assertEquals(receiver.changes.keySet, Set(root.id, child.id))
    assertEquals(graph.heads("main"), Set(child.id))
    assert(graph.gcRoots.contains(child.change))
    assert(ArtifactDependencies.direct(graph.artifact).toOption.get.contains(root.change))

  test("PR30 certified replication admits valid changes and quarantines incomplete or forged claims"):
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).fold(e => fail(e), identity)
    val base = Module(List("a" -> Stlc.tru))
    val change = parseChange("{ replace a = false ; }")
    val result = SemanticRepository.commit(runtime, base, change).fold(e => fail(e), identity)._1
    val accepted = AcceptedTip.checkTip(runtime, SemanticRepository.Tip(base, result, change))
      .fold(e => fail(e), identity)
    val sourceCas = MemCas()
    val branches = Branches(sourceCas, java.nio.file.Files.createTempDirectory("cairn-native-repo"),
      EffectContexts.forBranches())
    branches.importModule("main", base)
    val manifest = branches.commitTip("main", accepted)
    assertEquals(manifest.domainRuntime, Some(runtime.digest))
    assert(manifest.repositoryGraph.nonEmpty)
    val graph = branches.nativeRepository.fold(e => fail(e), identity)
    assertEquals(manifest.changeHistory, Nil)
    assertEquals(graph.heads("main").size, 1)
    val head = graph.heads("main").head
    assertEquals(graph.changes(head).change, accepted.vcs.artifact.digest)
    val roots = branches.liveCasRoots().fold(e => fail(e), identity)
    assert(graph.gcRoots.subsetOf(roots))
    assertEquals(branches.pullChanges("main", Set.empty).toOption.get.map(_.id), List(head))
    val payload = branches.pullChangeArtifacts("main", Set.empty).fold(e => fail(e), identity)
    val receiver = Branches(MemCas(), java.nio.file.Files.createTempDirectory("cairn-native-receiver"),
      EffectContexts.forBranches())
    val declaredMachine = GenericMachine.declare(List(runtime.digest))
    val implementations = declaredMachine.supportArtifacts.filter(_.kind == cairn.kernel.ArtifactKind.InterpreterImplementation)
      .map(a => InterpreterImplementation.fromArtifact(a).toOption.get)
    val grammar = cairn.kernel.Artifact(cairn.kernel.ArtifactKind.Grammar, cairn.kernel.GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest,
      capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest("causal-receiver", declaredMachine.machine.digest, List(appLanguage), Nil)
    val application = ResolvedApplication(appManifest.digest, appManifest, declaredMachine.machine,
      implementations, Map("stlc" -> capabilities), Map.empty, Set.empty, Map("stlc" -> runtime))
    val imported = receiver.pushChangeArtifacts(payload, application).fold(e => fail(e), identity)
    assertEquals(imported.applied, List(head))
    assertEquals(receiver.nativeRepository.toOption.get.changes.keySet, Set(head))
    assertEquals(receiver.nativeRepository.toOption.get.digest, graph.digest)
    assertEquals(receiver.verifyNativeRepository(application), Right(receiver.nativeRepository.toOption.get.digest))

    // One unordered adversarial batch: a valid envelope is admitted, a
    // semantically forged result is rejected, and a genuine but incomplete
    // envelope remains pending until its named evidence arrives.
    val original = graph.changes(head)
    val forged = original.copy(result = original.base)
    val wrongRuntime = original.copy(runtime = dig("forged-runtime"))
    val wrongContext = original.copy(context = Nil)
    val malformed = cairn.kernel.Artifact(cairn.kernel.ArtifactKind.ChangeSet,
      Canon.CTag("causal-change", Canon.cmap("malformed" -> Canon.CInt(1))))
    val acceptance = payload.find(_.digest == original.acceptanceEvidence.get).get
    val alternateAcceptance = cairn.kernel.Artifact(cairn.kernel.ArtifactKind.AcceptanceEvidence,
      Canon.cmap((acceptance.body.asMap.toList :+ ("transportVariant" -> Canon.CStr("pending")))*))
    val incomplete = original.copy(acceptanceEvidence = Some(alternateAcceptance.digest))
    val adversarialReceiver = Branches(MemCas(), java.nio.file.Files.createTempDirectory("cairn-native-adversarial"),
      EffectContexts.forBranches())
    val mixed = scala.util.Random(29).shuffle(payload ++ List(
      forged.artifact, wrongRuntime.artifact, wrongContext.artifact, malformed, incomplete.artifact))
      .filterNot(_.digest == alternateAcceptance.digest).toList
    val first = adversarialReceiver.pushChangeArtifacts(mixed, application).fold(e => fail(e), identity)
    assert(first.applied.contains(original.id))
    assert(first.rejected.contains(forged.id))
    assert(first.rejected.contains(wrongRuntime.id))
    assert(first.rejected.contains(wrongContext.id))
    assert(first.rejected.contains(malformed.digest))
    assert(first.pending.contains(incomplete.id))
    assert(first.missing.contains(alternateAcceptance.digest))
    val completed = adversarialReceiver.pushChangeArtifacts(
      List(incomplete.artifact, alternateAcceptance), application).fold(e => fail(e), identity)
    assert(completed.applied.contains(incomplete.id))
    assert(!completed.pending.contains(incomplete.id))

  test("PR27 Branches preserves conflicts in graph state and resolves them as dependent changes"):
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).fold(e => fail(e), identity)
    val base = Module(List("a" -> Stlc.tru))
    val left = parseChange("{ replace a = false ; }")
    val right = parseChange("{ edit a at [] = fun x : Bool . x ; }")
    val branches = Branches(MemCas(), java.nio.file.Files.createTempDirectory("cairn-native-conflict"),
      EffectContexts.forBranches())
    def commit(branch: String, change: Cst): Unit =
      branches.importModule(branch, base)
      val result = SemanticRepository.commit(runtime, base, change).toOption.get._1
      val accepted = AcceptedTip.checkTip(runtime, SemanticRepository.Tip(base, result, change)).toOption.get
      branches.commitTip(branch, accepted)
    commit("ours", left)
    commit("theirs", right)
    val conflict = branches.mergeBranches(runtime, "merged", "ours", "theirs")
      .fold(e => fail(e), identity).left.toOption.getOrElse(fail("expected conflict"))
    val conflicted = branches.nativeRepository.fold(e => fail(e), identity)
    assertEquals(conflicted.heads("merged"), conflicted.heads("ours") ++ conflicted.heads("theirs"))
    assertEquals(conflicted.conflicts(conflict.artifact.digest).unresolved, conflict.overlap)
    val resolutionLanguage = ConflictDelta.deltaOf(lang).toOption.get
    val program = Parser.parse(resolutionLanguage.grammar, "{ accept-left; }").fold(e => fail(e), identity)
    branches.resolveConflict(runtime, "merged", base, left, right, program)
      .fold(e => fail(e), identity) match
        case ConflictResolutionOutcome.Accepted(_, _) => ()
        case _ => fail("expected accepted resolution")
    val resolved = branches.nativeRepository.fold(e => fail(e), identity)
    val record = resolved.conflicts(conflict.artifact.digest)
    assert(record.resolution.nonEmpty)
    assertEquals(record.unresolved, Set.empty)
    assert(record.causes.subsetOf(resolved.changes(record.resolution.get).dependencies))
