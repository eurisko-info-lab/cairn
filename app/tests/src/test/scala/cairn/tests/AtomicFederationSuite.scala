package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{Keypair, MemCas, Node}
import java.nio.file.Files

class AtomicFederationSuite extends munit.FunSuite:
  private val authority = Keypair.dev("federation-authority")
  private val authorities = Map(authority.name -> authority.publicBytes)

  private def fixture(): (MemCas, FederationCommit) =
    val cas = MemCas()
    val capabilities = LanguageCapabilities.standard(Stlc.language)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(Stlc.language.grammar))
    val appLanguage = ApplicationLanguage("stlc", Stlc.language.digest, grammar.digest,
      capabilities.descriptor.digest, Some(runtime.digest))
    val manifest = ApplicationManifest("federated-app", machine.machine.digest, List(appLanguage), Nil)
    val evidence = AcceptanceEvidence(Stlc.language.digest, Stlc.base.digest, None, Stlc.base.digest,
      constitution.digest, "open", capabilities.changeModel.digest, constitution = Some(constitution.digest),
      runtime = Some(runtime.digest), publicationRequested = true)
    val graph = NativeRepository.empty
    val view = BranchManifest("main", None, Nil, acceptanceEvidence = Some(evidence.digest),
      domainRuntime = Some(runtime.digest), repositoryGraph = Some(graph.digest))
    val release = EcosystemBundles.sign("org.demo", SemanticVersion(1, 0, 0), manifest.digest,
      EcosystemRootKind.Application, Nil, Nil, authority)
    // Placeholder until PR31 slice 3 (NamespaceTrustManifest) lands — a bare
    // signed-canon-blob artifact under the same ArtifactKind.Certificate
    // family, standing in for the namespace's active trust manifest digest.
    val namespaceTrust = Artifact(ArtifactKind.Certificate, Canon.CStr("namespace-trust-placeholder"))
    val roots = graph.gcRoots ++ Set(graph.digest, view.artifact.digest, evidence.digest, runtime.digest,
      manifest.digest, release.digest, namespaceTrust.digest)
    val epoch = ReplicatedGcEpoch(1, roots, None)
    val allArtifacts = (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact,
      manifest.artifact, evidence.artifact, graph.artifact, view.artifact, release.artifact,
      namespaceTrust, epoch.artifact, grammar))
      .distinctBy(_.digest)
    allArtifacts.foreach(cas.put)
    val commit = FederationCommit("org.demo", "main", graph.digest, view.artifact.digest,
      evidence.digest, runtime.digest, manifest.digest, release.digest, namespaceTrust.digest, epoch.digest)
    cas.put(commit.artifact)
    cas -> commit

  test("PR31 crash before ledger publication preserves the old visible generation"):
    val (source, commit) = fixture()
    val node = Node(Files.createTempDirectory("cairn-fed-ledger-a"), EffectContexts.forLedger())
    val home = Files.createTempDirectory("cairn-fed-home-a")
    val federation = AtomicFederation(home, source, node)
    assert(federation.publish(commit, authority, authorities, FederationCrashPoint.AfterPrepare).isLeft)
    assertEquals(node.state(authorities).toOption.get.heads, Map.empty)
    assertEquals(federation.recover(authorities), Right(None))

  test("PR31 crash after atomic ledger append recovers exactly the certified generation"):
    val (source, commit) = fixture()
    val node = Node(Files.createTempDirectory("cairn-fed-ledger-b"), EffectContexts.forLedger())
    val home = Files.createTempDirectory("cairn-fed-home-b")
    val federation = AtomicFederation(home, source, node)
    val pubResult = federation.publish(commit, authority, authorities, FederationCrashPoint.AfterLedger)
    assert(pubResult.isLeft)
    assertEquals(federation.current, Right(None))
    val state = node.state(authorities).toOption.get
    assertEquals(state.heads(s"${commit.namespace}/${commit.branch}"), commit.artifact.key)
    assertEquals(federation.recover(authorities), Right(Some(commit.digest)))
    assertEquals(federation.current, Right(Some(commit.digest)))
    assert(ArtifactApplicationResolver(node.cas).audit(commit.digest).isRight)

  test("PR31 refuses a generation whose GC epoch omits a live constitutional root"):
    val (source, commit) = fixture()
    val badEpoch = ReplicatedGcEpoch(2, Set(commit.repositoryGraph), Some(commit.gcEpoch))
    source.put(badEpoch.artifact)
    val forged = commit.copy(gcEpoch = badEpoch.digest)
    source.put(forged.artifact)
    val node = Node(Files.createTempDirectory("cairn-fed-ledger-c"), EffectContexts.forLedger())
    val federation = AtomicFederation(Files.createTempDirectory("cairn-fed-home-c"), source, node)
    val rejected = federation.publish(forged, authority, authorities)
    assert(rejected.left.exists(_.contains("omits")), rejected.toString)
    assertEquals(node.state(authorities).toOption.get.heads, Map.empty)
