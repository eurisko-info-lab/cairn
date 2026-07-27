package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{BftFinality, DiskCas, Keypair, Node}
import java.nio.file.Files

/** PR31 slice 8: the six-phase crash-recoverable FederationState transition
  * protocol. One crash point per phase boundary — asserting old-state-
  * preserved (staged/proposed/certified) vs. new-state-completes
  * (ledgered), never a hybrid — plus one full, uninterrupted publish.
  */
class FederationTransactionSuite extends munit.FunSuite:
  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).toOption.get
  private val m0 = Module(List("a" -> Stlc.tru))
  private val casCtx = EffectContexts.forBranches()
  private val authority = Keypair.dev("federation-tx-authority")
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)

  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  /** One namespace's full closure (repository, application, namespace trust)
    * plus the FederationCommit binding them, and a genesis FederationState
    * naming it — everything persisted into `source`. Mirrors
    * FederationStateVerifySuite's fixture, extended with a FederationCommit
    * and a real ledger-anchored genesis (needed here since publish/recover
    * actually append to `node`, unlike verifyFederationState's tests).
    */
  private def fixture(): (DiskCas, Node, FederationCommit, FederationState, FederationState, ReplicaSetManifest, Digest) =
    val dir = Files.createTempDirectory("cairn-fedtx")
    val cas = DiskCas(dir.resolve("cas"))
    val change = parseChange("{ add extra = true ; }")
    val (result, vcs) = Delta.apply(lang, m0, change).fold(e => fail(e), identity)
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest("org-a-app", machine.machine.digest, List(appLanguage), Nil)
    val owner = Keypair.dev("org-a-owner")
    val trustManifest = NamespaceTrustManifest.of("org-a", List(owner.name -> owner.publicBytes)).fold(e => fail(e), identity)
    val evidence = AcceptanceEvidence(lang.digest, m0.digest, Some(vcs.artifact.digest), result.digest,
      constitution.digest, "open", capabilities.changeModel.digest, constitution = Some(constitution.digest),
      runtime = Some(runtime.digest))
    val causal = CausalChange(vcs.artifact.digest, Set.empty, Nil, m0.digest, result.digest, runtime.digest,
      acceptanceEvidence = Some(evidence.digest))
    val graph = NativeRepository(changes = Map(causal.id -> causal), heads = Map("main" -> Set(causal.id)))
    val release = EcosystemBundles.sign("org-a", SemanticVersion(1, 0, 0), appManifest.digest,
      EcosystemRootKind.Application, Nil, Nil, authority)
    val branchView = BranchManifest("main", None, Nil, acceptanceEvidence = Some(evidence.digest),
      domainRuntime = Some(runtime.digest), repositoryGraph = Some(graph.digest))
    val genesisEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val nextEpoch = ReplicatedGcEpoch(1,
      graph.gcRoots ++ Set(graph.digest, appManifest.digest, trustManifest.digest, release.digest), Some(genesisEpoch.digest))
    val commit = FederationCommit("org-a", "main", graph.digest, branchView.artifact.digest, evidence.digest,
      runtime.digest, appManifest.digest, release.digest, trustManifest.digest, nextEpoch.digest)

    val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val repoIndex = RepositoryIndex(Map("org-a" -> graph.digest))
    val appIndex = ApplicationIndex(Map("org-a" -> appManifest.digest))
    val nsIndex = NamespaceIndex(Map("org-a" -> trustManifest.digest))

    val node = Node(dir.resolve("ledger"), EffectContexts.forLedger())
    val authorities = Map(authority.name -> authority.publicBytes)
    node.append(authority, authorities, List(authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes))))
      .fold(e => fail(e), identity)
    val genesisLedgerBlock = node.chainDigests.head
    // The ledger block lives in `node`'s own internal ledger storage, not in
    // this test's separate content `cas` — a lightweight stand-in artifact
    // lets verifyFederationState's cas.getByDigest(state.ledger) decode
    // SOMETHING at that digest (the block's real replay-validity is the
    // ledger's own concern, checked separately below via node.state), same
    // approach as FederationStateVerifySuite.
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ledger-block-stand-in"))

    val genesisState = FederationState.genesis(ledgerStandIn.digest, replicaSet.digest)
    val newState = FederationState(ledgerStandIn.digest, repoIndex.digest, appIndex.digest, nsIndex.digest,
      replicaSet.digest, nextEpoch.digest)

    (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact, appManifest.artifact, grammar,
      trustManifest.artifact, vcs.artifact, m0.artifact, result.artifact, evidence.artifact, graph.artifact,
      release.artifact, branchView.artifact, commit.artifact, replicaSet.artifact, repoIndex.artifact,
      appIndex.artifact, nsIndex.artifact, genesisEpoch.artifact, nextEpoch.artifact, genesisState.artifact,
      ledgerStandIn))
      .distinctBy(_.digest).foreach(cas.put)
    val federationId = genesisLedgerBlock
    (cas, node, commit, genesisState, newState, replicaSet, federationId)

  private def coordinator(home: java.nio.file.Path, source: DiskCas, node: Node, replicaSet: ReplicaSetManifest, federationId: Digest) =
    FederationTransactionCoordinator(home, source, node, replicas, replicaSet, federationId)

  private def authorities = Map(authority.name -> authority.publicBytes)

  test("no crash: publish completes and current reflects the new state"):
    val (cas, node, commit, genesis, newState, replicaSet, federationId) = fixture()
    val home = Files.createTempDirectory("cairn-fedtx-home-ok")
    val coord = coordinator(home, cas, node, replicaSet, federationId)
    val (cert, block) = coord.publish(List(commit), genesis, newState, epoch = 1L, authority, authorities)
      .fold(e => fail(e), identity)
    assertEquals(cert.stateDigest, newState.digest)
    assertEquals(coord.current, Right(Some(newState.digest)))
    val ledgerState = node.state(authorities).fold(e => fail(e), identity)
    assert(ledgerState.published.contains(newState.artifact.key.render))
    assert(ledgerState.heads.get("org-a/main").contains(commit.artifact.key))

  test("crash after staged: old state preserved, intent discarded on recover"):
    val (cas, node, commit, genesis, newState, replicaSet, federationId) = fixture()
    val home = Files.createTempDirectory("cairn-fedtx-home-staged")
    val coord = coordinator(home, cas, node, replicaSet, federationId)
    assert(coord.publish(List(commit), genesis, newState, 1L, authority, authorities,
      FederationTransactionPhase.AfterStaged).isLeft)
    assertEquals(coord.current, Right(None))
    assertEquals(node.state(authorities).fold(e => fail(e), identity).heads, Map.empty)
    assertEquals(coord.recover(authorities), Right(None))
    assertEquals(coord.current, Right(None))

  test("crash after proposed: old state preserved, intent discarded on recover"):
    val (cas, node, commit, genesis, newState, replicaSet, federationId) = fixture()
    val home = Files.createTempDirectory("cairn-fedtx-home-proposed")
    val coord = coordinator(home, cas, node, replicaSet, federationId)
    assert(coord.publish(List(commit), genesis, newState, 1L, authority, authorities,
      FederationTransactionPhase.AfterProposed).isLeft)
    assertEquals(coord.current, Right(None))
    assertEquals(node.state(authorities).fold(e => fail(e), identity).heads, Map.empty)
    assertEquals(coord.recover(authorities), Right(None))

  test("crash after certified: old state preserved (no ledger append happened), intent discarded on recover"):
    val (cas, node, commit, genesis, newState, replicaSet, federationId) = fixture()
    val home = Files.createTempDirectory("cairn-fedtx-home-certified")
    val coord = coordinator(home, cas, node, replicaSet, federationId)
    assert(coord.publish(List(commit), genesis, newState, 1L, authority, authorities,
      FederationTransactionPhase.AfterCertified).isLeft)
    assertEquals(coord.current, Right(None))
    assertEquals(node.state(authorities).fold(e => fail(e), identity).heads, Map.empty,
      "no ledger append happened yet — the certificate alone is not durable exposure")
    assertEquals(coord.recover(authorities), Right(None))

  test("crash after ledgered: recover completes to exactly the new, certified state"):
    val (cas, node, commit, genesis, newState, replicaSet, federationId) = fixture()
    val home = Files.createTempDirectory("cairn-fedtx-home-ledgered")
    val coord = coordinator(home, cas, node, replicaSet, federationId)
    assert(coord.publish(List(commit), genesis, newState, 1L, authority, authorities,
      FederationTransactionPhase.AfterLedgered).isLeft)
    assertEquals(coord.current, Right(None), "not yet exposed locally — that's the crash point")
    val ledgerState = node.state(authorities).fold(e => fail(e), identity)
    assert(ledgerState.published.contains(newState.artifact.key.render), "the ledger append itself is atomic and already durable")
    assert(ledgerState.heads.get("org-a/main").contains(commit.artifact.key))
    assertEquals(coord.recover(authorities), Right(Some(newState.digest)))
    assertEquals(coord.current, Right(Some(newState.digest)))
