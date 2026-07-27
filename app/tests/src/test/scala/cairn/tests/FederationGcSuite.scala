package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{BftFinality, CasEffects, DiskCas, FederationFinality, Keypair, Node}
import java.nio.file.Files

/** PR31 slice 6: generalized, federation-wide GC-root computation, and
  * reclaim gated on a FINALIZED epoch rather than a locally-computed one.
  */
class FederationGcSuite extends munit.FunSuite:
  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).toOption.get
  private val m0 = Module(List("a" -> Stlc.tru))
  private val casCtx = EffectContexts.forBranches()
  private val authority = Keypair.dev("federation-gc-authority")

  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  private def accept(tip: SemanticRepository.ValidatedTip): AcceptedTip =
    AcceptedTip.checkTip(lang, tip.asTip, AcceptancePolicy.open).fold(e => fail(e), identity)

  /** One namespace: a real committed branch, a real application, and a real
    * signed ecosystem release — enough for computeFederationGcRoots to have
    * something concrete to walk.
    */
  private def namespaceFixture(dir: java.nio.file.Path, name: String): (Branches, Digest, SignedEcosystemBundle) =
    val cas = DiskCas(dir.resolve("cas"))
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val tip = SemanticRepository.tipAfter(lang, m0, parseChange("{ add extra = true ; }")).fold(e => fail(e), identity)
    branches.commitTip(name, accept(tip))
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val manifest = ApplicationManifest(s"$name-app", machine.machine.digest, List(appLanguage), Nil)
    val release = EcosystemBundles.sign(name, SemanticVersion(1, 0, 0), manifest.digest,
      EcosystemRootKind.Application, Nil, Nil, authority)
    (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact, manifest.artifact, grammar, release.artifact))
      .distinctBy(_.digest).foreach(cas.put)
    (branches, manifest.digest, release)

  test("computeFederationGcRoots: union covers each namespace's live branch state, application closure, and release closure"):
    val dirA = Files.createTempDirectory("cairn-fedgc-a")
    val dirB = Files.createTempDirectory("cairn-fedgc-b")
    val (branchesA, appA, releaseA) = namespaceFixture(dirA, "org-a")
    val (branchesB, appB, releaseB) = namespaceFixture(dirB, "org-b")
    // The resolver's local CAS only needs to already contain what it's asked to
    // audit; each namespace fixture already put its own closure into its own cas.
    val resolverA = ArtifactApplicationResolver(DiskCas(dirA.resolve("cas")))
    val resolverB = ArtifactApplicationResolver(DiskCas(dirB.resolve("cas")))
    val rootsA = FederationGc.computeFederationGcRoots(
      Map("org-a" -> branchesA), Map("org-a" -> appA), Map("org-a" -> releaseA), resolverA)
      .fold(e => fail(e), identity)
    val liveA = branchesA.liveCasRoots().fold(e => fail(e), identity)
    val appClosureA = resolverA.audit(appA).fold(e => fail(e), identity)
    val releaseClosureA = resolverA.audit(releaseA.digest).fold(e => fail(e), identity)
    assert(liveA.subsetOf(rootsA), "must include the namespace's own live branch roots")
    assert(appClosureA.subsetOf(rootsA), "must include the application/machine closure")
    assert(releaseClosureA.subsetOf(rootsA), "must include the ecosystem release closure")

    val rootsBoth = FederationGc.computeFederationGcRoots(
      Map("org-a" -> branchesA, "org-b" -> branchesB),
      Map("org-a" -> appA, "org-b" -> appB),
      Map("org-a" -> releaseA, "org-b" -> releaseB),
      resolverA)
    // org-b's artifacts live in a DIFFERENT cas than resolverA's — auditing
    // org-b's roots through resolverA's cas must fail, proving the aggregation
    // genuinely tries to resolve every namespace's own closure (not just org-a's).
    assert(rootsBoth.isLeft, rootsBoth.toString)

  test("reclaimAgainstFinalizedEpoch: reclaims exactly against the epoch a real finality certificate names, never a locally-computed one"):
    val dir = Files.createTempDirectory("cairn-fedgc-reclaim")
    val casRoot = dir.resolve("cas")
    val cas = DiskCas(casRoot)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val tip = SemanticRepository.tipAfter(lang, m0, parseChange("{ add extra = true ; }")).fold(e => fail(e), identity)
    branches.commitTip("main", accept(tip))
    val liveRoots = branches.liveCasRoots().fold(e => fail(e), identity)
    val epoch = ReplicatedGcEpoch(1, liveRoots, None)
    cas.put(epoch.artifact)
    val orphan = Artifact(ArtifactKind.Claim, Canon.CStr("orphan-blob"))
    val orphanDigest = CasEffects.put(cas, orphan, casCtx).fold(e => fail(e.toString), _.valueHash)
    assert(CasEffects.contains(cas, orphanDigest, casCtx).contains(true))

    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val federationId = Digest.of(Canon.CStr("federation-gc-test"))
    // reclaimAgainstFinalizedEpoch always audits the full closure of the
    // state it finalizes (see its own doc comment), so every field must be a
    // real, decodable artifact — not a bare placeholder digest.
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ledger-stand-in"))
    val repoIndex = RepositoryIndex(Map.empty)
    val appIndex = ApplicationIndex(Map.empty)
    val nsIndex = NamespaceIndex(Map.empty)
    List(ledgerStandIn, repoIndex.artifact, appIndex.artifact, nsIndex.artifact, manifest.artifact).foreach(cas.put)
    val state = FederationState(
      ledger = ledgerStandIn.digest, repository = repoIndex.digest,
      applications = appIndex.digest, namespaces = nsIndex.digest,
      trustRoots = manifest.digest, gcEpoch = epoch.digest)
    cas.put(state.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, manifest, view = 0, stateDigest = state.digest, epoch = 1L,
      previousState = Digest.of(Canon.CStr("genesis")), federationId = federationId).fold(e => fail(e), identity)

    val node = Node(dir.resolve("ledger"), EffectContexts.forLedger())
    val report = FederationGc.reclaimAgainstFinalizedEpoch(
      casRoot, state, cas, cert, manifest, federationId, casCtx, node).fold(e => fail(e), identity)
    assert(report.swept >= 1, report.toString)
    assert(CasEffects.contains(cas, orphanDigest, casCtx).contains(false))
    assert(branches.headModule("main").isRight, "the live branch head must survive reclaim")

  test("reclaimAgainstFinalizedEpoch rejects a certificate that doesn't name the candidate state"):
    val dir = Files.createTempDirectory("cairn-fedgc-reclaim-wrong")
    val cas = DiskCas(dir.resolve("cas"))
    val epoch = ReplicatedGcEpoch(1, Set.empty, None)
    cas.put(epoch.artifact)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val federationId = Digest.of(Canon.CStr("federation-gc-test-2"))
    val state = FederationState(Digest.of(Canon.CStr("l")), Digest.of(Canon.CStr("r")),
      Digest.of(Canon.CStr("a")), Digest.of(Canon.CStr("n")), manifest.digest, epoch.digest)
    val differentState = state.copy(ledger = Digest.of(Canon.CStr("different-ledger")))
    val certForDifferentState = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, manifest, view = 0, stateDigest = differentState.digest, epoch = 1L,
      previousState = Digest.of(Canon.CStr("genesis")), federationId = federationId).fold(e => fail(e), identity)
    val node = Node(dir.resolve("ledger"), EffectContexts.forLedger())
    val rejected = FederationGc.reclaimAgainstFinalizedEpoch(
      dir.resolve("cas"), state, cas, certForDifferentState, manifest, federationId, casCtx, node)
    assert(rejected.left.exists(_.contains("does not finalize")), rejected.toString)

  test("reclaimAgainstFinalizedEpoch rejects a certificate from the wrong federation"):
    val dir = Files.createTempDirectory("cairn-fedgc-reclaim-fed")
    val cas = DiskCas(dir.resolve("cas"))
    val epoch = ReplicatedGcEpoch(1, Set.empty, None)
    cas.put(epoch.artifact)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val federationId = Digest.of(Canon.CStr("federation-gc-test-3"))
    val state = FederationState(Digest.of(Canon.CStr("l")), Digest.of(Canon.CStr("r")),
      Digest.of(Canon.CStr("a")), Digest.of(Canon.CStr("n")), manifest.digest, epoch.digest)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, manifest, view = 0, stateDigest = state.digest, epoch = 1L,
      previousState = Digest.of(Canon.CStr("genesis")), federationId = federationId).fold(e => fail(e), identity)
    val wrongFederation = Digest.of(Canon.CStr("some-other-federation"))
    val node = Node(dir.resolve("ledger"), EffectContexts.forLedger())
    val rejected = FederationGc.reclaimAgainstFinalizedEpoch(
      dir.resolve("cas"), state, cas, cert, manifest, wrongFederation, casCtx, node)
    assert(rejected.left.exists(_.contains("federation id mismatch")), rejected.toString)

  /** PR32 slice 6: a real reclaim run against generation 2 must not sweep
    * generation 1's own FederationTransition/FederationState/index
    * artifacts, even though nothing in generation 2's OWN closure points
    * back at them (a state has no predecessor field) — this is the direct
    * regression test for the gap `permanentHistoryRoots` closes.
    */
  test("reclaimAgainstFinalizedEpoch retains every prior generation's transition/state history, not just the live one"):
    val dir = Files.createTempDirectory("cairn-fedgc-history-retention")
    val cas = DiskCas(dir.resolve("cas"))
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest("org-a-app", machine.machine.digest, List(appLanguage), Nil)
    val owner = Keypair.dev("org-a-owner-hist")
    val trustManifest = NamespaceTrustManifest.of("org-a", List(owner.name -> owner.publicBytes)).fold(e => fail(e), identity)
    val release = EcosystemBundles.sign("org-a", SemanticVersion(1, 0, 0), appManifest.digest,
      EcosystemRootKind.Application, Nil, Nil, authority)

    def buildGeneration(src: String, previousEpoch: ReplicatedGcEpoch, number: Long) =
      val change = parseChange(src)
      val (result, vcs) = Delta.apply(lang, m0, change).fold(e => fail(e), identity)
      val evidence = AcceptanceEvidence(lang.digest, m0.digest, Some(vcs.artifact.digest), result.digest,
        constitution.digest, "open", capabilities.changeModel.digest, constitution = Some(constitution.digest),
        runtime = Some(runtime.digest))
      val causal = CausalChange(vcs.artifact.digest, Set.empty, Nil, m0.digest, result.digest, runtime.digest,
        acceptanceEvidence = Some(evidence.digest))
      val graph = NativeRepository(changes = Map(causal.id -> causal), heads = Map("main" -> Set(causal.id)))
      val branchView = BranchManifest("main", None, Nil, acceptanceEvidence = Some(evidence.digest),
        domainRuntime = Some(runtime.digest), repositoryGraph = Some(graph.digest))
      val epoch = ReplicatedGcEpoch(number,
        graph.gcRoots ++ Set(graph.digest, appManifest.digest, trustManifest.digest, release.digest), Some(previousEpoch.digest))
      val commit = FederationCommit("org-a", "main", graph.digest, branchView.artifact.digest, evidence.digest,
        runtime.digest, appManifest.digest, release.digest, trustManifest.digest, epoch.digest)
      List(vcs.artifact, m0.artifact, result.artifact, evidence.artifact, graph.artifact, branchView.artifact,
        commit.artifact, epoch.artifact).foreach(cas.put)
      (graph, commit, epoch)

    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ledger-stand-in-hist"))
    val node = Node(dir.resolve("ledger"), EffectContexts.forLedger())
    val authorities = Map(authority.name -> authority.publicBytes)
    node.append(authority, authorities, List(authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes))))
      .fold(e => fail(e), identity)
    val federationId = node.chainDigests.head
    List(replicaSet.artifact, appManifest.artifact, grammar, trustManifest.artifact, release.artifact, ledgerStandIn)
      .foreach(cas.put)
    (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact)).foreach(cas.put)

    val genesisEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    cas.put(genesisEpoch.artifact)
    val genesisState = FederationState.genesis(ledgerStandIn.digest, replicaSet.digest)
    cas.put(genesisState.artifact)

    val (graph1, commit1, epoch1) = buildGeneration("{ add extra = true ; }", genesisEpoch, 1L)
    val repoIndex1 = RepositoryIndex(Map("org-a" -> graph1.digest))
    val appIndex = ApplicationIndex(Map("org-a" -> appManifest.digest))
    val nsIndex = NamespaceIndex(Map("org-a" -> trustManifest.digest))
    List(repoIndex1.artifact, appIndex.artifact, nsIndex.artifact).foreach(cas.put)
    val state1 = FederationState(ledgerStandIn.digest, repoIndex1.digest, appIndex.digest, nsIndex.digest,
      replicaSet.digest, epoch1.digest)

    val coord1 = FederationTransactionCoordinator(dir.resolve("home-1"), cas, node, Map.empty, replicaSet, federationId)
    coord1.publishLocalTestOnly(replicas, List(commit1), genesisState, state1, epoch = 1L, authority, authorities).fold(e => fail(e), identity)
    val transitionDigestsAfterGen1 = FederationGc.orderedTransitionDigests(node).fold(e => fail(e), identity)
    assertEquals(transitionDigestsAfterGen1.length, 1)
    val transition1Digest = transitionDigestsAfterGen1.head

    val (graph2, commit2, epoch2) = buildGeneration("{ add extra = true ; add second = false ; }", epoch1, 2L)
    val repoIndex2 = RepositoryIndex(Map("org-a" -> graph2.digest))
    cas.put(repoIndex2.artifact)
    val state2 = FederationState(ledgerStandIn.digest, repoIndex2.digest, appIndex.digest, nsIndex.digest,
      replicaSet.digest, epoch2.digest)

    val coord2 = FederationTransactionCoordinator(dir.resolve("home-2"), cas, node, Map.empty, replicaSet, federationId)
    val (cert2, _) = coord2.publishLocalTestOnly(replicas, List(commit2), state1, state2, epoch = 2L, authority, authorities).fold(e => fail(e), identity)

    val orphan = Artifact(ArtifactKind.Claim, Canon.CStr("history-retention-orphan"))
    val orphanDigest = CasEffects.put(cas, orphan, casCtx).fold(e => fail(e.toString), _.valueHash)
    val report = FederationGc.reclaimAgainstFinalizedEpoch(
      dir.resolve("cas"), state2, cas, cert2, replicaSet, federationId, casCtx, node).fold(e => fail(e), identity)
    assert(report.swept >= 1, report.toString)
    assert(CasEffects.contains(cas, orphanDigest, casCtx).contains(false), "the orphan must actually be swept")

    assert(cas.getByDigest(transition1Digest).isRight,
      "generation 1's own FederationTransition must survive reclaim against generation 2")
    assert(cas.getByDigest(genesisState.digest).isRight, "genesis state must survive reclaim")
    assert(cas.getByDigest(state1.digest).isRight, "generation 1's own state must survive reclaim")
    assert(cas.getByDigest(repoIndex1.digest).isRight, "generation 1's own repository index must survive reclaim")
