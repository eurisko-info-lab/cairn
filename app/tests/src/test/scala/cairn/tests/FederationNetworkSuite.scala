package cairn.tests

import cairn.runtime.*
import cairn.core.*
import cairn.kernel.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.*

/** PR33 slice 5: `FederationReplica` + `HttpNode`'s federation endpoints
  * reach real quorum agreement over actual HTTP — the same
  * "four in-JVM `HttpNode`s, real `HttpServer`s on loopback ports" pattern
  * `DistributionDaemonSuite`'s own BFT-block-finality tests already use.
  * The verify callback is stubbed to always accept: this suite's own focus
  * is the network protocol/wiring, not verification depth (already covered
  * by `FederationReplicaVerificationSuite`), so no CAS content needs to
  * exist anywhere — the proposal's digests are just consistent stand-ins.
  */
class FederationNetworkSuite extends munit.FunSuite:
  private val ledgerCtx = EffectContexts.forLedger()
  private val alwaysVerified: FederationReplica.VerifyProposal = (_, _) => FederationReplica.VerifyOutcome.Verified

  private def sampleProposal(
      federationId: Digest, replicaSetDigest: Digest, epoch: Long,
  ): FederationFinality.FederationProposal =
    FederationFinality.FederationProposal(
      federationId = federationId,
      transition = Digest.of(Canon.CStr(s"transition-$epoch")),
      before = Digest.of(Canon.CStr(s"before-$epoch")),
      after = Digest.of(Canon.CStr(s"after-$epoch")),
      epoch = epoch,
      replicaSet = replicaSetDigest)

  test("four HttpNode FederationReplicas exchange messages over real HTTP and mint a network certificate"):
    val auth = Keypair.dev("federation-network-auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val ids = manifest.ids
    val federationId = Digest.of(Canon.CStr("federation-network-suite-federation"))
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-federation-net-$id")).toMap
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), ledgerCtx)
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      id -> n
    }
    val https = scala.collection.mutable.ListBuffer.empty[HttpNode]
    try
      val ports = ids.map { id =>
        val home = homes(id)
        val federation = FederationReplica.certified(
          replicas.find(_.name == id).get, manifest, federationId, alwaysVerified,
          certStore = Some(home.resolve("federation-certs.canon")),
          stateStore = Some(home.resolve("federation-state.canon")))
          .fold(e => fail(e), identity)
        val http = HttpNode(nodes(id), ledgerAuth, peersRoot = Some(home), federation = Some(federation))
        https += http
        id -> http.start()
      }.toMap
      ids.foreach { id =>
        ids.foreach { peer =>
          PeerRegistry.addBound(
            homes(id), replicas.find(_.name == peer).get,
            s"http://127.0.0.1:${ports(peer)}",
            PeerRegistry.Role.Replica).fold(e => fail(e), identity)
        }
      }
      val urls = ids.map(id => id -> s"http://127.0.0.1:${ports(id)}").toMap
      val proposal = sampleProposal(federationId, manifest.replicaSetDigest, epoch = 1L)
      val cert = FederationFinality.agreeNetworkRemote(
        urls, proposal, replicas.head, authorities = manifest.authorities, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert.stateDigest, proposal.after)
      assertEquals(cert.previousState, proposal.before)
      assertEquals(cert.epoch, 1L)
      assertEquals(cert.federationId, federationId)
      assert(cert.commits.map(_._1.id).distinct.length >= BftQuorum.quorumSize(4))
      assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert, manifest), Right(()))

      // Continuous finality: second round on the same running durable replicas.
      val proposal2 = sampleProposal(federationId, manifest.replicaSetDigest, epoch = 2L)
        .copy(before = proposal.after)
      val cert2 = FederationFinality.agreeNetworkRemote(
        urls, proposal2, replicas.head, authorities = manifest.authorities, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert2.stateDigest, proposal2.after)
      assertEquals(cert2.previousState, proposal.after)
      assertEquals(cert2.epoch, 2L)
      assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert2, manifest), Right(()))
    finally https.foreach(_.stop())

  /** A real, PR30-deep-certifiable single-namespace round (mirrors
    * `FederationReplicaVerificationSuite`'s own fixture shape) plus the
    * proposal naming it. Returns the round-specific artifacts separately
    * from the rest so a test can withhold exactly those from one replica's
    * CAS — standing in for "a replica that has genesis but is missing
    * everything from this round" — while every other supporting artifact
    * (language/machine/grammar/evidence/etc., which every replica already
    * shares) is present everywhere.
    */
  private def deepFixture(
      federationId: Digest, replicaSetDigest: Digest,
  ): (List[Artifact], List[Artifact], FederationFinality.FederationProposal) =
    val lang = Stlc.language
    val dl = Delta.deltaOf(lang).toOption.get
    val m0 = Module(List("a" -> Stlc.tru))
    val authority = Keypair.dev("federation-network-fetch-authority")
    val change = Parser.parse(dl.grammar, "{ add extra = true ; }").fold(e => fail(e), identity)
    val (result, vcs) = Delta.apply(lang, m0, change).fold(e => fail(e), identity)
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest("org-fetch-app", machine.machine.digest, List(appLanguage), Nil)
    val owner = Keypair.dev("org-fetch-owner")
    val trustManifest = NamespaceTrustManifest.of("org-fetch", List(owner.name -> owner.publicBytes)).fold(e => fail(e), identity)
    val evidence = AcceptanceEvidence(lang.digest, m0.digest, Some(vcs.artifact.digest), result.digest,
      AcceptancePolicy.open.digest, "", capabilities.changeModel.digest, constitution = Some(constitution.digest),
      runtime = Some(runtime.digest))
    val trace = ChangeAlgebra.accessTrace(lang, m0, vcs.change, capabilities.changeModel).fold(e => fail(e.toString), identity)
    val context = trace.accesses.map(a => ContextDependency(a.location, Set.empty))
    val causal = CausalChange(vcs.artifact.digest, Set.empty, context, m0.digest, result.digest, runtime.digest,
      acceptanceEvidence = Some(evidence.digest))
    val graph = NativeRepository(changes = Map(causal.id -> causal), heads = Map("main" -> Set(causal.id)))
    val release = EcosystemBundles.sign("org-fetch", SemanticVersion(1, 0, 0), appManifest.digest,
      EcosystemRootKind.Application, Nil, Nil, authority)
    val branchView = BranchManifest("main", None, Nil, acceptanceEvidence = Some(evidence.digest),
      domainRuntime = Some(runtime.digest), repositoryGraph = Some(graph.digest))
    val genesisEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val nextEpoch = ReplicatedGcEpoch(1,
      graph.gcRoots ++ Set(graph.digest, appManifest.digest, trustManifest.digest, release.digest), Some(genesisEpoch.digest))
    val commit = FederationCommit("org-fetch", "main", graph.digest, branchView.artifact.digest, evidence.digest,
      runtime.digest, appManifest.digest, release.digest, trustManifest.digest, nextEpoch.digest)

    val repoIndex = RepositoryIndex(Map("org-fetch" -> graph.digest))
    val appIndex = ApplicationIndex(Map("org-fetch" -> appManifest.digest))
    val nsIndex = NamespaceIndex(Map("org-fetch" -> trustManifest.digest))
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("federation-network-fetch-ledger-stand-in"))
    val genesisState = FederationState.genesis(ledgerStandIn.digest, replicaSetDigest)
    val state1 = FederationState(ledgerStandIn.digest, repoIndex.digest, appIndex.digest, nsIndex.digest,
      replicaSetDigest, nextEpoch.digest)
    val transition = FederationTransition(genesisState.digest, List(commit.digest), state1.digest, List(trustManifest.digest), None)

    val shared = (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact, appManifest.artifact, grammar,
      trustManifest.artifact, vcs.artifact, m0.artifact, result.artifact, evidence.artifact, graph.artifact,
      release.artifact, branchView.artifact, repoIndex.artifact,
      appIndex.artifact, nsIndex.artifact, genesisEpoch.artifact, nextEpoch.artifact, genesisState.artifact, ledgerStandIn))
      .distinctBy(_.digest)
    val roundSpecific = List(commit.artifact, state1.artifact, transition.artifact).distinctBy(_.digest)
    val proposal = FederationFinality.FederationProposal(
      federationId, transition.digest, genesisState.digest, state1.digest, epoch = 1L, replicaSetDigest)
    (shared, roundSpecific, proposal)

  test("a replica missing this round's closure catches up via /blob/<hex> and independently mints a matching certificate"):
    val auth = Keypair.dev("federation-network-fetch-auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val ids = manifest.ids
    val federationId = Digest.of(Canon.CStr("federation-network-fetch-federation"))
    val (shared, roundSpecific, proposal) = deepFixture(federationId, manifest.replicaSetDigest)
    val catchingUp = "r3"
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-federation-fetch-$id")).toMap
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), ledgerCtx)
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      shared.foreach(n.cas.put)
      if id != catchingUp then roundSpecific.foreach(n.cas.put)
      id -> n
    }
    roundSpecific.foreach(a => assert(!nodes(catchingUp).cas.contains(a.digest)))
    val https = scala.collection.mutable.ListBuffer.empty[HttpNode]
    try
      val ports = ids.map { id =>
        val home = homes(id)
        val verify: FederationReplica.VerifyProposal =
          (proposerId, prop) => FederationReplicaVerification.verify(proposerId, prop, nodes(id).cas)
        val federation = FederationReplica.certified(
          replicas.find(_.name == id).get, manifest, federationId, verify,
          certStore = Some(home.resolve("federation-certs.canon")),
          stateStore = Some(home.resolve("federation-state.canon")))
          .fold(e => fail(e), identity)
        val http = HttpNode(nodes(id), ledgerAuth, peersRoot = Some(home), federation = Some(federation))
        https += http
        id -> http.start()
      }.toMap
      ids.foreach { id =>
        ids.foreach { peer =>
          PeerRegistry.addBound(
            homes(id), replicas.find(_.name == peer).get,
            s"http://127.0.0.1:${ports(peer)}",
            PeerRegistry.Role.Replica).fold(e => fail(e), identity)
        }
      }
      val urls = ids.map(id => id -> s"http://127.0.0.1:${ports(id)}").toMap
      val cert = FederationFinality.agreeNetworkRemote(
        urls, proposal, replicas.head, authorities = manifest.authorities, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert.stateDigest, proposal.after)
      assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert, manifest), Right(()))
      // The catching-up replica actually fetched the round's closure into
      // its own CAS. `agreeNetworkRemote` only needs quorum (3 of 4), so it
      // can return before the PrePrepare's independent fan-out to r3 (which
      // isn't on the quorum's own critical path) has finished landing —
      // bounded poll for that, rather than asserting the instant it returns.
      val deadline = System.nanoTime() + 3000L * 1000000L
      while !roundSpecific.forall(a => nodes(catchingUp).cas.contains(a.digest)) && System.nanoTime() < deadline do
        Thread.sleep(50)
      roundSpecific.foreach(a => assert(nodes(catchingUp).cas.contains(a.digest), a.digest.toString))
    finally https.foreach(_.stop())

  test("/federation/status serves a verifiable FederationViewStatus"):
    val auth = Keypair.dev("federation-network-status-auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val federationId = Digest.of(Canon.CStr("federation-network-status-federation"))
    val home = java.nio.file.Files.createTempDirectory("cairn-federation-net-status")
    val node = Node(home.resolve("node"), ledgerCtx)
    val federation = FederationReplica.certified(replicas.head, manifest, federationId, alwaysVerified)
      .fold(e => fail(e), identity)
    val http = HttpNode(node, ledgerAuth, federation = Some(federation))
    try
      val port = http.start()
      val url = s"http://127.0.0.1:$port"
      val status = FederationFinality.fetchViewStatus(url).fold(e => fail(e), identity)
      assertEquals(status.replica, federation.id)
      assertEquals(status.view, 0)
      assertEquals(status.federationId, federationId)
      assertEquals(status.replicaSet, manifest.replicaSetDigest)
      assertEquals(FederationFinality.verifyViewStatus(manifest.authorities, status, federationId, manifest.replicaSetDigest), Right(()))
    finally http.stop()
