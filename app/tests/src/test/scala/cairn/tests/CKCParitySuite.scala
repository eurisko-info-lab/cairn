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

  private def hexToBytes(hex: String): Vector[Byte] =
    if hex.length % 2 != 0 then fail(s"invalid hex length: ${hex.length}")
    hex.grouped(2).toVector.map(h => Integer.parseInt(h, 16).toByte)

  private def pinnedKeypair(name: String, publicHex: String, privateHex: String): Keypair =
    Keypair.fromEncoded(name, hexToBytes(publicHex), hexToBytes(privateHex))

  private val repoRoot: Path = Path.of("").toAbsolutePath.normalize
  private val rustManifest: Path = repoRoot.resolve("verifier-rust/Cargo.toml")
  private val leanDir: Path = repoRoot.resolve("verifier-lean")

  private val scalaConstitution = CKC.KernelConstitution()
  private val scalaBudget = CKC.Budget()
  private val fixtureAuthority = pinnedKeypair(
    name = "ckc-parity-authority",
    publicHex = "302a300506032b6570032100a87d85722941aae3b82028cb38ed20702b1922292049b16a8c57d0ed2316e6c1",
    privateHex = "302e020100300506032b657004220420c24fec16782cc229f3f0d2b2b3c440d8527b7718e48734338444db1851e98c4b",
  )
  private val fixtureReplicas = List(
    pinnedKeypair(
      name = "r0",
      publicHex = "302a300506032b6570032100d2a2d19c55dd11dea2b7f6e4aebae459f94a863d4eb8f058f33fb8c737586109",
      privateHex = "302e020100300506032b657004220420f3eb189e11702901f5be61b194e7da39eb5fbd593c97f42180149ad3860c9afa",
    ),
    pinnedKeypair(
      name = "r1",
      publicHex = "302a300506032b6570032100d070cd4acbf54b9128d93be53b217f1559e860c8bd65b852dcb488cb5b332279",
      privateHex = "302e020100300506032b657004220420801a621b495fef4acf156fb9367fb65172d735caba22f116c4fa84880c67dd77",
    ),
    pinnedKeypair(
      name = "r2",
      publicHex = "302a300506032b65700321009811558e9769e23c13aea290d68d4a6e6b9e5fc8fb4ccce37eef242452c338bb",
      privateHex = "302e020100300506032b6570042204209e13c3e970da4ca0a6621b7e2d4d3e79c0325730967cc6d7f18ff3df9b91271e",
    ),
    pinnedKeypair(
      name = "r3",
      publicHex = "302a300506032b6570032100ad5935d267ea6d1c52f539ec225a96eab971bf4d802377518764f328221103b6",
      privateHex = "302e020100300506032b657004220420fd57d833ec02aac4897728a6224ab8d51c96ba5eb3bc76eace6fd81a84398eaf",
    ),
  )
  private val fixtureOwner = pinnedKeypair(
    name = "org-a-owner-ckc",
    publicHex = "302a300506032b6570032100b5140a1f1c7db623289dfdf720dcc8446a10b7ce1d0002bf630c63e9f61fd7cf",
    privateHex = "302e020100300506032b6570042204205fddc0e71b0bbba105079f2ee4ef30bade5c6a2572618215cb5c23c356864196",
  )

  private final case class Fixture(
      casRoot: Path,
      nodeRoot: Path,
      federationId: Digest,
      genesisState: Digest,
      languageDigest: Digest,
      grammarDigest: Digest,
      authorityName: String,
      replicaSetDigest: Digest,
      releaseDigest: Digest,
      resolveDigestG0: Digest,
      resolveDigestG1: Digest,
      governedDeltaG0ToG1: Digest,
      runtimeDigest: Digest,
      machineDigest: Digest,
      acceptanceDigest: Digest,
  )

  private final case class CertFixture(
      casRoot: Path,
      manifestDigest: Digest,
      cert1: FederationFinality.FederationFinalityCertificate,
      cert2: FederationFinality.FederationFinalityCertificate,
  )

  private final case class PromotedFoundation(
      kernelId: String,
      replayMaxSteps: Long,
      languageDigest: Digest,
      grammarDigest: Digest,
      authorityName: String,
      replicaSetDigest: Digest,
      releaseDigest: Digest,
      predecessorPackage: Digest,
      successorPackage: Digest,
      governedDelta: Digest,
      runtimeDigest: Digest,
      machineDigest: Digest,
      acceptanceDigest: Digest,
      federationId: Digest,
      genesisState: Digest,
      finalStateDigest: Digest,
      finalEpoch: Long,
      verifiedTransitions: Int,
      manifestDigest: Digest,
      cert1Digest: Digest,
      cert2Digest: Digest,
      resolveEvidenceG0: Digest,
      resolveEvidenceG1: Digest,
      replayEvidenceG1: Digest,
  ):
    def canon: Canon = Canon.CTag("pr34-foundation-handoff-v1", Canon.cmap(
      "kernelId" -> Canon.CStr(kernelId),
      "replayMaxSteps" -> Canon.CInt(replayMaxSteps),
      "languageDigest" -> Canon.CStr(languageDigest.hex),
      "grammarDigest" -> Canon.CStr(grammarDigest.hex),
      "authorityName" -> Canon.CStr(authorityName),
      "replicaSetDigest" -> Canon.CStr(replicaSetDigest.hex),
      "releaseDigest" -> Canon.CStr(releaseDigest.hex),
      "predecessorPackage" -> Canon.CStr(predecessorPackage.hex),
      "successorPackage" -> Canon.CStr(successorPackage.hex),
      "governedDelta" -> Canon.CStr(governedDelta.hex),
      "runtimeDigest" -> Canon.CStr(runtimeDigest.hex),
      "machineDigest" -> Canon.CStr(machineDigest.hex),
      "acceptanceDigest" -> Canon.CStr(acceptanceDigest.hex),
      "federationId" -> Canon.CStr(federationId.hex),
      "genesisState" -> Canon.CStr(genesisState.hex),
      "finalStateDigest" -> Canon.CStr(finalStateDigest.hex),
      "finalEpoch" -> Canon.CInt(finalEpoch),
      "verifiedTransitions" -> Canon.CInt(verifiedTransitions),
      "manifestDigest" -> Canon.CStr(manifestDigest.hex),
      "cert1Digest" -> Canon.CStr(cert1Digest.hex),
      "cert2Digest" -> Canon.CStr(cert2Digest.hex),
      "resolveEvidenceG0" -> Canon.CStr(resolveEvidenceG0.hex),
      "resolveEvidenceG1" -> Canon.CStr(resolveEvidenceG1.hex),
      "replayEvidenceG1" -> Canon.CStr(replayEvidenceG1.hex),
    ))
    def digest: Digest = Digest.of(canon)

  private def buildFixture(): Fixture =
    val dir = Files.createTempDirectory("cairn-ckc-parity")
    val cas = DiskCas(dir.resolve("cas"))
    val node = Node(dir.resolve("ledger"), EffectContexts.forLedger())

    val lang = Stlc.language
    val dl = Delta.deltaOf(lang).fold(e => fail(e.map(_.render).mkString), identity)
    val m0 = Module(List("a" -> Stlc.tru))
    val authority = fixtureAuthority
    val replicas = fixtureReplicas
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
    val owner = fixtureOwner
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
      if transitionDigests.length < 2 then fail(s"expected at least two finalized transitions, got ${transitionDigests.length}")
      val firstTransitionArtifact = cas.getByDigest(transitionDigests.head).fold(e => fail(e), identity)
      val firstTransition = FederationTransition.fromArtifact(firstTransitionArtifact).fold(e => fail(e), identity)
      firstTransition.before

    val governedDeltaG0ToG1 =
      val transitionDigests = FederationGc.orderedTransitionDigests(node).fold(e => fail(e), identity)
      val secondTransitionDigest = transitionDigests(1)
      val secondTransitionArtifact = cas.getByDigest(secondTransitionDigest).fold(e => fail(e), identity)
      val secondTransition = FederationTransition.fromArtifact(secondTransitionArtifact).fold(e => fail(e), identity)
      assertEquals(secondTransition.before, state1.digest)
      assertEquals(secondTransition.after, state2.digest)
      secondTransitionDigest

    Fixture(
      casRoot = dir.resolve("cas"),
      nodeRoot = dir.resolve("ledger"),
      federationId = federationId,
      genesisState = replayGenesisState,
      languageDigest = lang.digest,
      grammarDigest = grammar.digest,
      authorityName = authority.name,
      replicaSetDigest = replicaSet.digest,
      releaseDigest = release.digest,
      resolveDigestG0 = state1.digest,
      resolveDigestG1 = state2.digest,
      governedDeltaG0ToG1 = governedDeltaG0ToG1,
      runtimeDigest = runtime.digest,
      machineDigest = machine.machine.digest,
      acceptanceDigest = constitution.digest,
    )

  private def buildCertFixture(): CertFixture =
    val dir = Files.createTempDirectory("cairn-ckc-cert")
    val cas = DiskCas(dir.resolve("cas"))
    val replicas = fixtureReplicas
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

  private def buildPromotedFoundation(): PromotedFoundation =
    val replayFixture = buildFixture()
    val certFixture = buildCertFixture()
    val resolveG0 = CKC.derive(scalaConstitution, scalaBudget,
      CKC.Query.Resolve(replayFixture.casRoot.toString, replayFixture.resolveDigestG0))
    val resolveG1 = CKC.derive(scalaConstitution, scalaBudget,
      CKC.Query.Resolve(replayFixture.casRoot.toString, replayFixture.resolveDigestG1))
    val replayBudget = CKC.Budget(maxSteps = scalaBudget.maxSteps)
    val replayG1 = CKC.derive(scalaConstitution, replayBudget,
      CKC.Query.ReplayHistory(replayFixture.nodeRoot.toString, replayFixture.federationId, replayFixture.genesisState))
    val replayReportValue = replayReport(replayG1)

    PromotedFoundation(

      kernelId = scalaConstitution.kernelId,
      replayMaxSteps = replayBudget.maxSteps,
      languageDigest = replayFixture.languageDigest,
      grammarDigest = replayFixture.grammarDigest,
      authorityName = replayFixture.authorityName,
      replicaSetDigest = replayFixture.replicaSetDigest,
      releaseDigest = replayFixture.releaseDigest,
      predecessorPackage = replayFixture.resolveDigestG0,
      successorPackage = replayFixture.resolveDigestG1,
      governedDelta = replayFixture.governedDeltaG0ToG1,
      runtimeDigest = replayFixture.runtimeDigest,
      machineDigest = replayFixture.machineDigest,
      acceptanceDigest = replayFixture.acceptanceDigest,
      federationId = replayFixture.federationId,
      genesisState = replayFixture.genesisState,
      manifestDigest = certFixture.manifestDigest,
      finalStateDigest = replayReportValue.finalState,
      finalEpoch = replayReportValue.finalEpoch,
      verifiedTransitions = replayReportValue.verifiedTransitions,
      cert1Digest = certFixture.cert1.digest,
      cert2Digest = certFixture.cert2.digest,
      resolveEvidenceG0 = validEvidence(resolveG0),
      resolveEvidenceG1 = validEvidence(resolveG1),
      replayEvidenceG1 = validEvidence(replayG1),
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

  private def validEvidence(result: CKC.KernelResult): Digest = result match
    case CKC.KernelResult.Valid(_, evidence) => evidence
    case other => fail(s"expected valid result with evidence, got $other")

  private def replayReport(result: CKC.KernelResult): CKC.HistoryReport = result match
    case CKC.KernelResult.Valid(CKC.Value.ReplayedState(report), _) => report
    case other => fail(s"expected replay report, got $other")

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
    val delta = replayFixture.governedDeltaG0ToG1

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

  test("PR34 successor world fixture is independently auditable"):
    val replayFixture = buildFixture()
    val node = Node(replayFixture.nodeRoot, EffectContexts.forLedger())
    val verified = FederationHistory
      .auditPublishedTransition(node, node.cas, replayFixture.governedDeltaG0ToG1, replayFixture.federationId)
      .fold(e => fail(e), identity)
    assertEquals(verified.transition.before, replayFixture.resolveDigestG0)
    assertEquals(verified.transition.after, replayFixture.resolveDigestG1)

  test("PR34 staircase fixture digests are reproducible"):
    val a = buildFixture()
    val b = buildFixture()
    val expectedFederationId = "4e9330155c00de9d5122866d30002185726acc4a64aa28953bb6d47f53afdd96"
    val expectedGenesisState = "2572d018e52b0027127b2299fece3bb58390d450f982e8647e604819479dfb28"
    val expectedResolveG0 = "81b66603400140329bd60ad0f8e3d3b815b24068021e0118d1bda3fe8d1c3581"
    val expectedResolveG1 = "6707bb0a84b82cc04f088faecb297435d10d5aff9226c7aff6a27c7de154de5e"
    val expectedGovernedDelta = "8bd4708267492fb9b45ae6e49a658bd779e5373fa031066339f62e62ed491280"
    assertEquals(a.federationId, b.federationId)
    assertEquals(a.genesisState, b.genesisState)
    assertEquals(a.resolveDigestG0, b.resolveDigestG0)
    assertEquals(a.resolveDigestG1, b.resolveDigestG1)
    assertEquals(a.governedDeltaG0ToG1, b.governedDeltaG0ToG1)
    assertEquals(a.federationId.hex, expectedFederationId)
    assertEquals(a.genesisState.hex, expectedGenesisState)
    assertEquals(a.resolveDigestG0.hex, expectedResolveG0)
    assertEquals(a.resolveDigestG1.hex, expectedResolveG1)
    assertEquals(a.governedDeltaG0ToG1.hex, expectedGovernedDelta)

  test("PR34 fixture uses pinned key material"):
    val authorityAgain = pinnedKeypair(
      name = "ckc-parity-authority",
      publicHex = "302a300506032b6570032100a87d85722941aae3b82028cb38ed20702b1922292049b16a8c57d0ed2316e6c1",
      privateHex = "302e020100300506032b657004220420c24fec16782cc229f3f0d2b2b3c440d8527b7718e48734338444db1851e98c4b",
    )
    assertEquals(authorityAgain.publicBytes, fixtureAuthority.publicBytes)
    assertEquals(authorityAgain.privateBytes, fixtureAuthority.privateBytes)

  test("PR34 cert fixture digests are reproducible"):
    val a = buildCertFixture()
    val b = buildCertFixture()
    val expectedManifest = "6757960c891274d6fdc025a4eba54d3e9fb711e80048b747700e1ade201c3626"
    val expectedCert1 = "59792e4bb96b2f34ad88e85f09d0fbefd9cb3780d9b3fd1800bdaa021fa713c8"
    val expectedCert2 = "0ec2e8c9342407b83a1837c3d7a9d3b57fa35e6b1f9bd45e2453f697a495065b"
    assertEquals(a.manifestDigest, b.manifestDigest)
    assertEquals(a.cert1.digest, b.cert1.digest)
    assertEquals(a.cert2.digest, b.cert2.digest)
    assertEquals(a.manifestDigest.hex, expectedManifest)
    assertEquals(a.cert1.digest.hex, expectedCert1)
    assertEquals(a.cert2.digest.hex, expectedCert2)

  test("PR34 first promoted foundation artifact set is reproducible"):
    val a = buildPromotedFoundation()
    val b = buildPromotedFoundation()
    val expectedFoundationDigest = "063114ddd638aa642ecb84eb8e674657253e9d688a63915900462266cfe051ba"
    assertEquals(a.kernelId, b.kernelId)
    assertEquals(a.replayMaxSteps, b.replayMaxSteps)
    assertEquals(a.languageDigest, b.languageDigest)
    assertEquals(a.grammarDigest, b.grammarDigest)
    assertEquals(a.authorityName, b.authorityName)
    assertEquals(a.replicaSetDigest, b.replicaSetDigest)
    assertEquals(a.releaseDigest, b.releaseDigest)
    assertEquals(a.predecessorPackage, b.predecessorPackage)
    assertEquals(a.successorPackage, b.successorPackage)
    assertEquals(a.governedDelta, b.governedDelta)
    assertEquals(a.runtimeDigest, b.runtimeDigest)
    assertEquals(a.machineDigest, b.machineDigest)
    assertEquals(a.acceptanceDigest, b.acceptanceDigest)
    assertEquals(a.federationId, b.federationId)
    assertEquals(a.genesisState, b.genesisState)
    assertEquals(a.manifestDigest, b.manifestDigest)
    assertEquals(a.cert1Digest, b.cert1Digest)
    assertEquals(a.cert2Digest, b.cert2Digest)
    assertEquals(a.resolveEvidenceG0, b.resolveEvidenceG0)
    assertEquals(a.resolveEvidenceG1, b.resolveEvidenceG1)
    assertEquals(a.replayEvidenceG1, b.replayEvidenceG1)
    assertEquals(a.digest, b.digest)
    assertEquals(a.digest.hex, expectedFoundationDigest)

  test("PR34 promoted foundation reconstructs G1 independently in Scala and Rust"):
    assume(cargoAvailable, "cargo not on PATH")

    val replayFixture = buildFixture()
    val promoted = buildPromotedFoundation()

    assertEquals(promoted.successorPackage, replayFixture.resolveDigestG1)
    assertEquals(promoted.federationId, replayFixture.federationId)
    assertEquals(promoted.genesisState, replayFixture.genesisState)

    val (rustResolveG1Kind, rustResolveG1Evidence) =
      rust(Seq("resolve", "--cas", replayFixture.casRoot.toString, "--digest", replayFixture.resolveDigestG1.hex))
    val (rustReplayKind, rustReplayEvidence) =
      rust(Seq("verify-history", "--node-root", replayFixture.nodeRoot.toString,
        "--federation-id", replayFixture.federationId.hex,
        "--genesis-state", replayFixture.genesisState.hex,
        "--max-steps", promoted.replayMaxSteps.toString))

    assertEquals(rustResolveG1Kind, "valid")
    assertEquals(rustReplayKind, "valid")
    assertEquals(rustResolveG1Evidence, Some(promoted.resolveEvidenceG1.hex))
    assertEquals(rustReplayEvidence, Some(promoted.replayEvidenceG1.hex))
