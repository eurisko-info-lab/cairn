package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{BftFinality, DiskCas, FederationFinality, FederationReplica, Keypair}
import java.nio.file.Files

/** PR33 slice 4: `FederationReplicaVerification.verify` — the real,
  * local-CAS-only deep check wired as `FederationReplica`'s
  * `VerifyProposal` callback. Structural checks are already covered by
  * `FederationTransitionCheckSuite`; this suite's own focus is (a) the
  * missing-closure classification and (b) that a namespace actually
  * touched by a commit gets PR30's full deep re-certification, not just
  * `verifyFederationState`'s shallow "evidence artifact has the right
  * kind" check.
  */
class FederationReplicaVerificationSuite extends munit.FunSuite:
  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).toOption.get
  private val m0 = Module(List("a" -> Stlc.tru))
  private val authority = Keypair.dev("federation-replica-verify-authority")
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
  private val federationId = Digest.of(Canon.CStr("federation-replica-verify-chain"))
  private val proposerId = BftQuorum.ReplicaId(replicas.head.name)

  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  /** One namespace's complete, independently deep-certifiable closure — a
    * real ΔL-replayable causal change with genuine acceptance evidence,
    * plus a genesis -> state1 transition naming it. Mirrors
    * `FederationTransactionSuite`'s own fixture shape (already proven to
    * satisfy `verifyFederationState`'s shallow check); this suite is the
    * first to also exercise the DEEP path against it.
    */
  private def fixture(stripEvidence: Boolean = false): (DiskCas, FederationFinality.FederationProposal, FederationState) =
    val dir = Files.createTempDirectory("cairn-fedreplica-verify")
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
    // `LanguageCapabilities.standard` has no validation model, so
    // `runtime.moduleGate(...)` always falls back to `ModuleGate.passthrough`
    // regardless of the resolver — i.e. exactly `AcceptancePolicy.open`,
    // independent of what any commit's application later resolves. The
    // deep path recomputes this policy and compares its digest against
    // `evidence.policy`, unlike the shallow check, which never looks at it.
    val evidence = AcceptanceEvidence(lang.digest, m0.digest, Some(vcs.artifact.digest), result.digest,
      AcceptancePolicy.open.digest, "", capabilities.changeModel.digest, constitution = Some(constitution.digest),
      runtime = Some(runtime.digest))
    // The deep path independently recomputes the access trace and requires
    // it to match `context` exactly (unlike the shallow visibility check,
    // which never looks at this at all) — genesis-relative, so every
    // location's providers are empty (nothing preceding it could have
    // provided them).
    val trace = ChangeAlgebra.accessTrace(lang, m0, vcs.change, capabilities.changeModel).fold(e => fail(e.toString), identity)
    val context = trace.accesses.map(a => ContextDependency(a.location, Set.empty))
    // `stripEvidence`: verifyFederationState's shallow certifyStructurally
    // treats a MISSING acceptanceEvidence as trivially fine (`None =>
    // Right(())`); PR30's deep certifyIncoming unconditionally requires it
    // (`causal.acceptanceEvidence.toRight(...)`) — a real discriminator
    // between the two depths, built consistently from construction so
    // every cross-referenced digest (repository index, state, transition)
    // already agrees, rather than patched in after the fact.
    val causal = CausalChange(vcs.artifact.digest, Set.empty, context, m0.digest, result.digest, runtime.digest,
      acceptanceEvidence = if stripEvidence then None else Some(evidence.digest))
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

    val repoIndex = RepositoryIndex(Map("org-a" -> graph.digest))
    val appIndex = ApplicationIndex(Map("org-a" -> appManifest.digest))
    val nsIndex = NamespaceIndex(Map("org-a" -> trustManifest.digest))

    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ledger-block-stand-in"))
    val genesisState = FederationState.genesis(ledgerStandIn.digest, replicaSet.digest)
    val state1 = FederationState(ledgerStandIn.digest, repoIndex.digest, appIndex.digest, nsIndex.digest,
      replicaSet.digest, nextEpoch.digest)
    // org-a's namespace-trust entry is new relative to genesis (which has
    // none) — VerifiedFederationTransition treats a namespace's first
    // appearance as a rotation requiring its manifest digest listed here.
    val transition = FederationTransition(genesisState.digest, List(commit.digest), state1.digest, List(trustManifest.digest), None)

    (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact, appManifest.artifact, grammar,
      trustManifest.artifact, vcs.artifact, m0.artifact, result.artifact, evidence.artifact, graph.artifact,
      release.artifact, branchView.artifact, commit.artifact, replicaSet.artifact, repoIndex.artifact,
      appIndex.artifact, nsIndex.artifact, genesisEpoch.artifact, nextEpoch.artifact, genesisState.artifact,
      state1.artifact, transition.artifact, ledgerStandIn))
      .distinctBy(_.digest).foreach(cas.put)

    val proposal = FederationFinality.FederationProposal(
      federationId, transition.digest, genesisState.digest, state1.digest, epoch = 1L, replicaSet.replicaSetDigest)
    (cas, proposal, state1)

  test("verify accepts a real, PR30-deep-certifiable single-namespace transition"):
    val (cas, proposal, _) = fixture()
    val outcome = FederationReplicaVerification.verify(proposerId, proposal, cas)
    assertEquals(outcome, FederationReplica.VerifyOutcome.Verified)

  test("verify reports MissingClosure when the transition artifact itself is not locally resident"):
    val (cas, proposal, _) = fixture()
    val emptyCas = DiskCas(Files.createTempDirectory("cairn-fedreplica-verify-empty").resolve("cas"))
    val outcome = FederationReplicaVerification.verify(proposerId, proposal, emptyCas)
    outcome match
      case FederationReplica.VerifyOutcome.MissingClosure(digests) =>
        assert(digests.contains(proposal.transition), digests.toString)
      case other => fail(s"expected MissingClosure, got $other")

  test("verify reports MissingClosure when a named commit is not locally resident (transition/states present)"):
    val (cas, proposal, state1) = fixture()
    // A second CAS with only the top-level transition/state artifacts, not the commit.
    val partialDir = Files.createTempDirectory("cairn-fedreplica-verify-partial")
    val partialCas = DiskCas(partialDir.resolve("cas"))
    val transitionArtifact = cas.getByDigest(proposal.transition).fold(e => fail(e), identity)
    val transition = FederationTransition.fromArtifact(transitionArtifact).fold(e => fail(e), identity)
    val genesisArtifact = cas.getByDigest(proposal.before).fold(e => fail(e), identity)
    List(transitionArtifact, genesisArtifact, state1.artifact).foreach(partialCas.put)
    val outcome = FederationReplicaVerification.verify(proposerId, proposal, partialCas)
    outcome match
      case FederationReplica.VerifyOutcome.MissingClosure(digests) =>
        assertEquals(digests, transition.transactions.toSet)
      case other => fail(s"expected MissingClosure, got $other")

  test("verify rejects a namespace whose resident causal change has no acceptance evidence, unlike the shallow check"):
    val (cas, proposal, _) = fixture(stripEvidence = true)
    val outcome = FederationReplicaVerification.verify(proposerId, proposal, cas)
    outcome match
      case FederationReplica.VerifyOutcome.Rejected(reason) =>
        assert(reason.contains("re-certification failed"), reason)
      case other => fail(s"expected Rejected, got $other")
