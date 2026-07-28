package cairn.tests

import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.kernel.*
import cairn.runtime.*
import cairn.systemhandler.{BftFinality, DiskCas, FederationFinality, Keypair, Node}
import java.nio.file.{Files, Path}
import scala.sys.process.{Process, ProcessLogger}

class CKCParitySuite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private val repoRoot: Path = Path.of("").toAbsolutePath.normalize
  private val rustManifest: Path = repoRoot.resolve("verifier-rust/Cargo.toml")
  private val leanDir: Path = repoRoot.resolve("verifier-lean")

  private val scalaConstitution = CKC.KernelConstitution()
  private val scalaBudget = CKC.Budget()

  private final case class Fixture(
      casRoot: Path,
      nodeRoot: Path,
      federationId: Digest,
      genesisState: Digest,
      resolveDigestG0: Digest,
      resolveDigestG1: Digest,
  )

  private final case class CertFixture(
      casRoot: Path,
      manifestDigest: Digest,
      cert1: FederationFinality.FederationFinalityCertificate,
      cert2: FederationFinality.FederationFinalityCertificate,
  )

  private def buildFixture(): Fixture =
    val dir = Files.createTempDirectory("cairn-ckc-parity")
    val cas = DiskCas(dir.resolve("cas"))
    val node = Node(dir.resolve("ledger"), EffectContexts.forLedger())

    val lang = Stlc.language
    val dl = Delta.deltaOf(lang).fold(e => fail(e.map(_.render).mkString), identity)
    val m0 = Module(List("a" -> Stlc.tru))
    val authority = Keypair.dev("ckc-parity-authority")
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val authorities = Map(authority.name -> authority.publicBytes)

    node.append(authority, authorities, List(authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes))))
      .fold(e => fail(e), identity)
    val federationId = node.chainDigests.head

    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).fold(e => fail(e), identity)
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest("org-a-app", machine.machine.digest, List(appLanguage), Nil)
    val owner = Keypair.dev("org-a-owner-ckc")
    val trustManifest = NamespaceTrustManifest.of("org-a", List(owner.name -> owner.publicBytes)).fold(e => fail(e), identity)
    val release = EcosystemBundles.sign("org-a", SemanticVersion(1, 0, 0), appManifest.digest,
      EcosystemRootKind.Application, Nil, Nil, authority)

    val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ledger-stand-in-ckc-parity"))
    val genesisEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    cas.put(ledgerStandIn)
    cas.put(genesisEpoch.artifact)
    val baseGenesisState = FederationState.genesis(ledgerStandIn.digest, replicaSet.digest)
    cas.put(baseGenesisState.artifact)
    List(replicaSet.artifact, appManifest.artifact, grammar, trustManifest.artifact, release.artifact)
      .foreach(cas.put)
    (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact)).foreach(cas.put)

    def buildGeneration(changeSrc: String, previousEpoch: ReplicatedGcEpoch, number: Long, priorState: FederationState): (FederationFinality.FederationFinalityCertificate, FederationState, ReplicatedGcEpoch) =
      val change = Parser.parse(dl.grammar, changeSrc).fold(e => fail(e), identity)
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
        graph.gcRoots ++ Set(graph.digest, appManifest.digest, trustManifest.digest, release.digest),
        Some(previousEpoch.digest))
      val commit = FederationCommit("org-a", "main", graph.digest, branchView.artifact.digest, evidence.digest,
        runtime.digest, appManifest.digest, release.digest, trustManifest.digest, epoch.digest)
      val repoIndex = RepositoryIndex(Map("org-a" -> graph.digest))
      val appIndex = ApplicationIndex(Map("org-a" -> appManifest.digest))
      val nsIndex = NamespaceIndex(Map("org-a" -> trustManifest.digest))
      val newState = FederationState(ledgerStandIn.digest, repoIndex.digest, appIndex.digest, nsIndex.digest,
        replicaSet.digest, epoch.digest)
      List(vcs.artifact, m0.artifact, result.artifact, evidence.artifact, graph.artifact, branchView.artifact,
        commit.artifact, epoch.artifact)
        .foreach(cas.put)
      List(repoIndex.artifact, appIndex.artifact, nsIndex.artifact).foreach(cas.put)
      cas.put(newState.artifact)
      val coord = FederationTransactionCoordinator(dir.resolve(s"home-$number"), cas, node, Map.empty, replicaSet, federationId)
      val (cert, _) = coord.publishLocalTestOnly(replicas, List(commit), priorState, newState, epoch = number, authority, authorities)
        .fold(e => fail(e), identity)
      cas.put(cert.artifact)
      (cert, newState, epoch)

    val (_, state1, epoch1) = buildGeneration("{ add extra = true ; }", genesisEpoch, 1L, baseGenesisState)
    val (_, state2, _) = buildGeneration("{ add extra = true ; add second = false ; }", epoch1, 2L, state1)

    val replayGenesisState =
      val transitionDigests = FederationGc.orderedTransitionDigests(node).fold(e => fail(e), identity)
      val firstTransitionArtifact = cas.getByDigest(transitionDigests.head).fold(e => fail(e), identity)
      val firstTransition = FederationTransition.fromArtifact(firstTransitionArtifact).fold(e => fail(e), identity)
      firstTransition.before

    Fixture(
      casRoot = dir.resolve("cas"),
      nodeRoot = dir.resolve("ledger"),
      federationId = federationId,
      genesisState = replayGenesisState,
      resolveDigestG0 = state1.digest,
      resolveDigestG1 = state2.digest,
    )

  private def buildCertFixture(): CertFixture =
    val dir = Files.createTempDirectory("cairn-ckc-cert")
    val cas = DiskCas(dir.resolve("cas"))
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val federationId = Digest.of(Canon.CStr("ckc-parity-cert-federation"))
    val previousState = Digest.of(Canon.CStr("ckc-parity-cert-previous-state"))
    val state1 = Digest.of(Canon.CStr("ckc-parity-cert-state-1"))
    val state2 = Digest.of(Canon.CStr("ckc-parity-cert-state-2"))
    val transition1 = Digest.of(Canon.CStr("ckc-parity-cert-transition-1"))
    val transition2 = Digest.of(Canon.CStr("ckc-parity-cert-transition-2"))

    val proposal1 = FederationFinality.FederationProposal(federationId, transition1, previousState, state1, 1L, manifest.replicaSetDigest)
    val proposal2 = FederationFinality.FederationProposal(federationId, transition2, previousState, state2, 1L, manifest.replicaSetDigest)
    val cert1 = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, proposal1)
      .fold(e => fail(e), identity)
    val cert2 = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, proposal2)
      .fold(e => fail(e), identity)
    List(manifest.artifact, proposal1.artifact, proposal2.artifact, cert1.artifact, cert2.artifact).foreach(cas.put)

    CertFixture(
      casRoot = dir.resolve("cas"),
      manifestDigest = manifest.digest,
      cert1 = cert1,
      cert2 = cert2,
    )

  private def runCapture(cmd: Seq[String], cwd: Path): (Int, String) =
    val out = new StringBuilder
    val err = new StringBuilder
    val code = Process(cmd, cwd.toFile).!(ProcessLogger(out.append(_).append('\n'), err.append(_).append('\n')))
    (code, out.toString + err.toString)

  private def leanAvailable: Boolean =
    Process(Seq("bash", "-lc", "command -v lake >/dev/null"), repoRoot.toFile).! == 0

  private def cargoAvailable: Boolean =
    Process(Seq("bash", "-lc", "command -v cargo >/dev/null"), repoRoot.toFile).! == 0

  private def evidenceToken(output: String): Option[String] =
    raw"evidence=([^\s]+)".r.findFirstMatchIn(output).map(_.group(1))

  private def classifyRust(output: String): String =
    if output.contains("ok:") then "valid"
    else if output.contains("missing closure:") then "missing"
    else if output.contains("exhausted:") then "exhausted"
    else "invalid"

  private def classifyLean(output: String): String =
    if output.startsWith("valid:") then "valid"
    else if output.startsWith("missing:") then "missing"
    else if output.startsWith("exhausted:") then "exhausted"
    else "invalid"

  private def classifyScala(result: CKC.KernelResult): String = result match
    case CKC.KernelResult.Valid(_, _) => "valid"
    case CKC.KernelResult.Missing(_) => "missing"
    case CKC.KernelResult.Exhausted(_) => "exhausted"
    case CKC.KernelResult.Invalid(_) => "invalid"

  private def rust(cmd: Seq[String]): (String, Option[String]) =
    val full = Seq("cargo", "run", "--quiet", "--manifest-path", rustManifest.toString, "--") ++ cmd
    val (_, out) = runCapture(full, repoRoot)
    (classifyRust(out), evidenceToken(out))

  private def lean(cmd: Seq[String]): (String, Option[String]) =
    val full = Seq("lake", "exe", "verifier-lean") ++ cmd
    val (_, out) = runCapture(full, leanDir)
    (classifyLean(out), evidenceToken(out))

  private def scalaResolve(result: CKC.KernelResult): Option[String] = result match
    case CKC.KernelResult.Valid(_, evidence) => Some(evidence.hex)
    case _ => None

  test("PR34 parity: real corpus agrees on resolve, cert binding, replay, and verdict classes"):
    assume(cargoAvailable, "cargo not on PATH")
    assume(leanAvailable, "lake not on PATH")

    val replayFixture = buildFixture()
    val certFixture = buildCertFixture()
    def scalaRun(query: CKC.Query, budget: CKC.Budget = scalaBudget): CKC.KernelResult =
      CKC.derive(scalaConstitution, budget, query)

    val validResolveG0 = scalaRun(CKC.Query.Resolve(replayFixture.casRoot.toString, replayFixture.resolveDigestG0))
    assertEquals(classifyScala(validResolveG0), "valid")
    val (rustResolveG0Kind, rustResolveG0Evidence) = rust(Seq("resolve", "--cas", replayFixture.casRoot.toString, "--digest", replayFixture.resolveDigestG0.hex))
    val (leanResolveG0Kind, leanResolveG0Evidence) = lean(Seq("resolve", replayFixture.casRoot.toString, replayFixture.resolveDigestG0.hex))
    assertEquals(rustResolveG0Kind, "valid")
    assertEquals(leanResolveG0Kind, "valid")
    assertEquals(scalaResolve(validResolveG0), rustResolveG0Evidence)
    assertEquals(leanResolveG0Evidence, scalaResolve(validResolveG0))

    val validResolveG1 = scalaRun(CKC.Query.Resolve(replayFixture.casRoot.toString, replayFixture.resolveDigestG1))
    assertEquals(classifyScala(validResolveG1), "valid")
    assertNotEquals(replayFixture.resolveDigestG0, replayFixture.resolveDigestG1)
    val (rustResolveG1Kind, rustResolveG1Evidence) = rust(Seq("resolve", "--cas", replayFixture.casRoot.toString, "--digest", replayFixture.resolveDigestG1.hex))
    val (leanResolveG1Kind, leanResolveG1Evidence) = lean(Seq("resolve", replayFixture.casRoot.toString, replayFixture.resolveDigestG1.hex))
    assertEquals(rustResolveG1Kind, "valid")
    assertEquals(leanResolveG1Kind, "valid")
    assertEquals(scalaResolve(validResolveG1), rustResolveG1Evidence)
    assertEquals(leanResolveG1Evidence, scalaResolve(validResolveG1))

    val missingDigest = Digest.of(Canon.CStr("ckc-parity-missing-resolve"))
    assertEquals(classifyScala(scalaRun(CKC.Query.Resolve(replayFixture.casRoot.toString, missingDigest))), "missing")
    assertEquals(rust(Seq("resolve", "--cas", replayFixture.casRoot.toString, "--digest", missingDigest.hex))._1, "missing")
    assertEquals(lean(Seq("resolve", replayFixture.casRoot.toString, missingDigest.hex))._1, "missing")

    val validCert = scalaRun(CKC.Query.VerifyCertBinding(certFixture.casRoot.toString, certFixture.cert1.digest, certFixture.cert1.proposal, certFixture.manifestDigest))
    assertEquals(classifyScala(validCert), "valid")
    assertEquals(rust(Seq("verify-cert", "--cas", certFixture.casRoot.toString, "--cert", certFixture.cert1.digest.hex, "--proposal", certFixture.cert1.proposal.hex, "--manifest", certFixture.manifestDigest.hex))._1, "valid")
    assertEquals(lean(Seq("verify-cert", certFixture.casRoot.toString, certFixture.cert1.digest.hex, certFixture.cert1.proposal.hex, certFixture.manifestDigest.hex))._1, "valid")

    val invalidCert = scalaRun(CKC.Query.VerifyCertBinding(certFixture.casRoot.toString, certFixture.cert1.digest, certFixture.cert2.proposal, certFixture.manifestDigest))
    assertEquals(classifyScala(invalidCert), "invalid")
    assertEquals(rust(Seq("verify-cert", "--cas", certFixture.casRoot.toString, "--cert", certFixture.cert1.digest.hex, "--proposal", certFixture.cert2.proposal.hex, "--manifest", certFixture.manifestDigest.hex))._1, "invalid")
    assertEquals(lean(Seq("verify-cert", certFixture.casRoot.toString, certFixture.cert1.digest.hex, certFixture.cert2.proposal.hex, certFixture.manifestDigest.hex))._1, "invalid")

    val validReplay = scalaRun(CKC.Query.ReplayHistory(replayFixture.nodeRoot.toString, replayFixture.federationId, replayFixture.genesisState))
    assertEquals(classifyScala(validReplay), "valid")
    val (rustReplayKind, rustReplayEvidence) = rust(Seq("verify-history", "--node-root", replayFixture.nodeRoot.toString, "--federation-id", replayFixture.federationId.hex, "--genesis-state", replayFixture.genesisState.hex))
    val (leanReplayKind, leanReplayEvidence) = lean(Seq("replay-history", replayFixture.nodeRoot.toString, replayFixture.federationId.hex, replayFixture.genesisState.hex))
    assertEquals(rustReplayKind, "valid")
    assertEquals(leanReplayKind, "valid")
    assertEquals(rustReplayEvidence, scalaResolve(validReplay))
    assertEquals(leanReplayEvidence, scalaResolve(validReplay))
    validReplay match
      case CKC.KernelResult.Valid(CKC.Value.ReplayedState(report), _) =>
        assertEquals(report.finalEpoch, 2L)
        assertEquals(report.finalState, replayFixture.resolveDigestG1)
      case other => fail(s"expected replay state, got $other")

    val exhaustedReplay = scalaRun(CKC.Query.ReplayHistory(replayFixture.nodeRoot.toString, replayFixture.federationId, replayFixture.genesisState), CKC.Budget(maxSteps = 0))
    assertEquals(classifyScala(exhaustedReplay), "exhausted")
    assertEquals(rust(Seq("verify-history", "--node-root", replayFixture.nodeRoot.toString, "--federation-id", replayFixture.federationId.hex, "--genesis-state", replayFixture.genesisState.hex, "--max-steps", "0"))._1, "exhausted")
    assertEquals(lean(Seq("--max-steps", "0", "replay-history", replayFixture.nodeRoot.toString, replayFixture.federationId.hex, replayFixture.genesisState.hex))._1, "exhausted")

    val wrongReplay = scalaRun(CKC.Query.ReplayHistory(replayFixture.nodeRoot.toString, Digest.of(Canon.CStr("ckc-parity-wrong-federation")), replayFixture.genesisState))
    assertEquals(classifyScala(wrongReplay), "invalid")
    assertEquals(rust(Seq("verify-history", "--node-root", replayFixture.nodeRoot.toString, "--federation-id", Digest.of(Canon.CStr("ckc-parity-wrong-federation")).hex, "--genesis-state", replayFixture.genesisState.hex))._1, "invalid")
    assertEquals(lean(Seq("replay-history", replayFixture.nodeRoot.toString, Digest.of(Canon.CStr("ckc-parity-wrong-federation")).hex, replayFixture.genesisState.hex))._1, "invalid")

  test("PR34 staircase scaffold parity: Rust and Lean agree on successor-link validation"):
    assume(cargoAvailable, "cargo not on PATH")
    assume(leanAvailable, "lake not on PATH")

    val replayFixture = buildFixture()
    val g0 = replayFixture.resolveDigestG0
    val g1 = replayFixture.resolveDigestG1
    val delta = Digest.of(Canon.CStr("pr34-stair-delta"))

    val g0Env = Pr34VerdictEnvelope(
      kernelConstitution = Digest.of(Canon.CStr("pr34-k0")),
      graphPackage = g0,
      verdictClass = Pr34VerdictClass.Valid,
      state = Some(g0),
      evidence = Some(Digest.of(Canon.CStr("pr34-e0"))),
      resourceUse = Pr34ResourceUse(steps = 1, bytesRead = 1, wallMicros = 1),
    )
    val g1Env = Pr34VerdictEnvelope(
      kernelConstitution = Digest.of(Canon.CStr("pr34-k1")),
      graphPackage = g1,
      verdictClass = Pr34VerdictClass.Valid,
      state = Some(g1),
      evidence = Some(Digest.of(Canon.CStr("pr34-e1"))),
      resourceUse = Pr34ResourceUse(steps = 1, bytesRead = 1, wallMicros = 1),
    )
    val link = Pr34SuccessorLink(
      predecessorPackage = g0,
      successorPackage = g1,
      upgradeDelta = delta,
    )
    assert(Pr34Staircase.validateTwoStep(g0Env, g1Env, link).isRight)

    val rustValid = rust(Seq("staircase-check", "--g0", g0.hex, "--g1", g1.hex, "--delta", delta.hex))._1
    val leanValid = lean(Seq("staircase-check", g0.hex, g1.hex, delta.hex))._1
    assertEquals(rustValid, "valid")
    assertEquals(leanValid, "valid")

    val rustInvalid = rust(Seq("staircase-check", "--g0", g0.hex, "--g1", g0.hex, "--delta", delta.hex))._1
    val leanInvalid = lean(Seq("staircase-check", g0.hex, g0.hex, delta.hex))._1
    assertEquals(rustInvalid, "invalid")
    assertEquals(leanInvalid, "invalid")

    val badPredecessor = Digest.of(Canon.CStr("pr34-stair-bad-predecessor"))
    val rustMismatch = rust(Seq(
      "staircase-check",
      "--g0", g0.hex,
      "--g1", g1.hex,
      "--delta", delta.hex,
      "--link-predecessor", badPredecessor.hex,
    ))._1
    val leanMismatch = lean(Seq(
      "staircase-check",
      g0.hex,
      g1.hex,
      delta.hex,
      badPredecessor.hex,
      g1.hex,
    ))._1
    assertEquals(rustMismatch, "invalid")
    assertEquals(leanMismatch, "invalid")

    val badSuccessor = Digest.of(Canon.CStr("pr34-stair-bad-successor"))
    val rustSuccessorMismatch = rust(Seq(
      "staircase-check",
      "--g0", g0.hex,
      "--g1", g1.hex,
      "--delta", delta.hex,
      "--link-successor", badSuccessor.hex,
    ))._1
    val leanSuccessorMismatch = lean(Seq(
      "staircase-check",
      g0.hex,
      g1.hex,
      delta.hex,
      g0.hex,
      badSuccessor.hex,
    ))._1
    assertEquals(rustSuccessorMismatch, "invalid")
    assertEquals(leanSuccessorMismatch, "invalid")

    val malformedDigest = "not-a-64-hex-digest"
    val rustMalformed = rust(Seq(
      "staircase-check",
      "--g0", malformedDigest,
      "--g1", g1.hex,
      "--delta", delta.hex,
    ))._1
    val leanMalformed = lean(Seq(
      "staircase-check",
      malformedDigest,
      g1.hex,
      delta.hex,
    ))._1
    assertEquals(rustMalformed, "invalid")
    assertEquals(leanMalformed, "invalid")

    val rustMalformedOverride = rust(Seq(
      "staircase-check",
      "--g0", g0.hex,
      "--g1", g1.hex,
      "--delta", delta.hex,
      "--link-successor", malformedDigest,
    ))._1
    val leanMalformedOverride = lean(Seq(
      "staircase-check",
      g0.hex,
      g1.hex,
      delta.hex,
      g0.hex,
      malformedDigest,
    ))._1
    assertEquals(rustMalformedOverride, "invalid")
    assertEquals(leanMalformedOverride, "invalid")
