package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{BftFinality, DiskCas, FederationFinality, Keypair, Node}
import java.nio.file.Files

/** PR32 slice 7: [[FederationHistory]] — replaying the ledger's own ordered
  * transition sequence from genesis, and auditing one transition in
  * isolation.
  */
class FederationHistorySuite extends munit.FunSuite:
  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).toOption.get
  private val m0 = Module(List("a" -> Stlc.tru))
  private val casCtx = EffectContexts.forBranches()
  private val authority = Keypair.dev("federation-history-authority")
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)

  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  /** Two real, ledger-anchored generations for one namespace: genesis ->
    * state1 -> state2, each via [[FederationTransactionCoordinator.publish]].
    */
  private def twoGenerationFixture(): (DiskCas, Node, Digest, FederationState, FederationState, FederationState, ReplicaSetManifest) =
    val dir = Files.createTempDirectory("cairn-fedhist")
    val cas = DiskCas(dir.resolve("cas"))
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest("org-a-app", machine.machine.digest, List(appLanguage), Nil)
    val owner = Keypair.dev("org-a-owner-fedhist")
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

    val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ledger-stand-in-fedhist"))
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

    val (graph2, commit2, epoch2) = buildGeneration("{ add extra = true ; add second = false ; }", epoch1, 2L)
    val repoIndex2 = RepositoryIndex(Map("org-a" -> graph2.digest))
    cas.put(repoIndex2.artifact)
    val state2 = FederationState(ledgerStandIn.digest, repoIndex2.digest, appIndex.digest, nsIndex.digest,
      replicaSet.digest, epoch2.digest)
    val coord2 = FederationTransactionCoordinator(dir.resolve("home-2"), cas, node, Map.empty, replicaSet, federationId)
    coord2.publishLocalTestOnly(replicas, List(commit2), state1, state2, epoch = 2L, authority, authorities).fold(e => fail(e), identity)

    (cas, node, federationId, genesisState, state1, state2, replicaSet)

  test("replayFromGenesis reproduces the current state exactly across two real generations"):
    val (_, node, federationId, genesisState, state1, state2, _) = twoGenerationFixture()
    val replayed = FederationHistory.replayFromGenesis(node, node.cas, genesisState, federationId).fold(e => fail(e), identity)
    assertEquals(replayed.digest, state2.digest)

  test("replayFromGenesis rejects a chain that does not start from the true genesis"):
    val (_, node, federationId, genesisState, state1, state2, _) = twoGenerationFixture()
    val wrongGenesis = genesisState.copy(ledger = Digest.of(Canon.CStr("wrong-genesis-ledger")))
    val result = FederationHistory.replayFromGenesis(node, node.cas, wrongGenesis, federationId)
    assert(result.left.exists(_.contains("does not chain from")), result.toString)

  test("auditPublishedTransition independently re-verifies a single transition by digest"):
    val (_, node, federationId, genesisState, state1, state2, _) = twoGenerationFixture()
    val digests = FederationGc.orderedTransitionDigests(node).fold(e => fail(e), identity)
    assertEquals(digests.length, 2)
    val secondVerified = FederationHistory.auditPublishedTransition(node, node.cas, digests(1), federationId).fold(e => fail(e), identity)
    assertEquals(secondVerified.before.digest, state1.digest)
    assertEquals(secondVerified.after.digest, state2.digest)

  test("auditPublishedTransition rejects an unknown digest"):
    val (_, node, federationId, genesisState, state1, state2, _) = twoGenerationFixture()
    val result = FederationHistory.auditPublishedTransition(node, node.cas, Digest.of(Canon.CStr("not-a-real-transition")), federationId)
    assert(result.isLeft, result.toString)

  test("auditPublishedTransition rejects a well-formed transition that was never actually ledger-anchored"):
    val (_, node, federationId, genesisState, state1, state2, _) = twoGenerationFixture()
    // A structurally valid, fully self-consistent transition (decode +
    // VerifiedFederationTransition.verify would both accept it — a genuine
    // no-op generation, unchanged indices, matching certificate) but never
    // published in any block on this node's ledger — must still be rejected.
    val ledger = Digest.of(Canon.CStr("unpublished-ledger-stand-in"))
    val gcEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val repoIndex = RepositoryIndex(Map.empty)
    val appIndex = ApplicationIndex(Map.empty)
    val nsIndex = NamespaceIndex(Map.empty)
    val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val stray = FederationState(ledger, repoIndex.digest, appIndex.digest, nsIndex.digest, replicaSet.digest, gcEpoch.digest)
    List(gcEpoch.artifact, repoIndex.artifact, appIndex.artifact, nsIndex.artifact, replicaSet.artifact, stray.artifact)
      .foreach(node.cas.put)
    // Two-step construction (mirrors FederationTransactionCoordinator.publishWithCert):
    // the proposal a replica would have voted on names the PRE-cert transition
    // (finality = None); only once a certificate exists does the FINAL,
    // finality-bound transition (a different digest) get built — needed since
    // PR33.1's VerifiedFederationTransition.verify independently fetches and
    // decodes `cert.proposal`, requiring it to be a real, resolvable artifact.
    val preCertTransition = FederationTransition(stray.digest, Nil, stray.digest, Nil, None)
    node.cas.put(preCertTransition.artifact)
    val proposal = FederationFinality.FederationProposal(
      federationId, preCertTransition.digest, stray.digest, stray.digest, 1L, replicaSet.replicaSetDigest)
    node.cas.put(proposal.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, replicaSet, view = 0, proposal)
      .fold(e => fail(e), identity)
    node.cas.put(cert.artifact)
    val unpublished = preCertTransition.copy(finality = Some(cert.digest))
    node.cas.put(unpublished.artifact)
    val result = FederationHistory.auditPublishedTransition(node, node.cas, unpublished.digest, federationId)
    assert(result.left.exists(_.contains("were not co-published")), result.toString)
