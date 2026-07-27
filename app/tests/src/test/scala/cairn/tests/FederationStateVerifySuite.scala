package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{DiskCas, Keypair, Node}
import java.nio.file.Files

/** PR31 slice 7: the atomic-visibility gate. `verifyFederationState`
  * composes NativeRepository.verifyFromRoots (per namespace),
  * ArtifactApplicationResolver.resolve (per namespace's application), and
  * ArtifactApplicationResolver.audit (the full transitive closure) —
  * generalizing AtomicFederation.verify from one namespace to every root a
  * FederationState names. Six negative tests (one per field) plus one
  * positive two-namespace case.
  */
class FederationStateVerifySuite extends munit.FunSuite:
  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).toOption.get
  private val m0 = Module(List("a" -> Stlc.tru))
  private val casCtx = EffectContexts.forBranches()
  private val authority = Keypair.dev("federation-verify-authority")
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)

  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  /** One fully-populated, internally-consistent namespace: a NativeRepository
    * with one resident causal change (its base/result modules and
    * acceptance evidence all real, persisted artifacts — matching
    * AtomicFederationSuite's fixture style rather than routing through
    * Branches.commitTip's legacy accept pipeline, a separate, already-tested
    * concern), an application/machine closure, and a genesis
    * NamespaceTrustManifest — all persisted into `cas`.
    */
  private def namespaceFixture(cas: DiskCas, name: String): (Digest, Digest, Digest) =
    val change = parseChange("{ add extra = true ; }")
    val (result, vcs) = Delta.apply(lang, m0, change).fold(e => fail(e), identity)
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest(s"$name-app", machine.machine.digest, List(appLanguage), Nil)
    val owner = Keypair.dev(s"$name-owner")
    val trustManifest = NamespaceTrustManifest.of(name, List(owner.name -> owner.publicBytes)).fold(e => fail(e), identity)
    val evidence = AcceptanceEvidence(lang.digest, m0.digest, Some(vcs.artifact.digest), result.digest,
      constitution.digest, "open", capabilities.changeModel.digest, constitution = Some(constitution.digest),
      runtime = Some(runtime.digest))
    val causal = CausalChange(vcs.artifact.digest, Set.empty, Nil, m0.digest, result.digest, runtime.digest,
      acceptanceEvidence = Some(evidence.digest))
    val graph = NativeRepository(changes = Map(causal.id -> causal), heads = Map(name -> Set(causal.id)))
    (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact, appManifest.artifact, grammar,
      trustManifest.artifact, vcs.artifact, m0.artifact, result.artifact, evidence.artifact, graph.artifact))
      .distinctBy(_.digest).foreach(cas.put)
    (graph.digest, appManifest.digest, trustManifest.digest)

  /** A complete, valid two-namespace FederationState, all persisted in one CAS. */
  private def validFederation(): (DiskCas, FederationState) =
    val dir = Files.createTempDirectory("cairn-fedverify")
    val cas = DiskCas(dir.resolve("cas"))
    val (repoA, appA, trustA) = namespaceFixture(cas, "org-a")
    val (repoB, appB, trustB) = namespaceFixture(cas, "org-b")
    val repoIndex = RepositoryIndex(Map("org-a" -> repoA, "org-b" -> repoB))
    val appIndex = ApplicationIndex(Map("org-a" -> appA, "org-b" -> appB))
    val nsIndex = NamespaceIndex(Map("org-a" -> trustA, "org-b" -> trustB))
    val replicaSet = cairn.systemhandler.BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val epoch = ReplicatedGcEpoch(1, Set.empty, None)
    val ledgerNode = Node(dir.resolve("ledger"), EffectContexts.forLedger())
    val authorities = Map(authority.name -> authority.publicBytes)
    ledgerNode.append(authority, authorities, List(authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes))))
      .fold(e => fail(e), identity)
    val ledgerBlock = ledgerNode.chainDigests.head
    List(repoIndex.artifact, appIndex.artifact, nsIndex.artifact, replicaSet.artifact, epoch.artifact).foreach(cas.put)
    // The ledger block itself isn't independently resolvable from `cas` (it lives on
    // the ledger's own node) — a lightweight stand-in artifact lets verifyFederationState's
    // cas.getByDigest(state.ledger) decode SOMETHING at that digest. Its real replay-validity
    // is the ledger's own concern (BftFinality.requireSealedBlock, tested elsewhere), not the
    // atomic-visibility gate's — verifyFederationState only needs it to decode.
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ledger-block-stand-in"))
    cas.put(ledgerStandIn)
    // trustRoots is the manifest's own ARTIFACT digest (content-addressable, decodable),
    // not .replicaSetDigest (a body-only matching key never itself a CAS key — see
    // FederationState.trustRoots's doc comment).
    val state = FederationState(ledgerStandIn.digest, repoIndex.digest, appIndex.digest, nsIndex.digest,
      replicaSet.digest, epoch.digest)
    cas.put(state.artifact)
    (cas, state)

  test("verifyFederationState: a complete, valid two-namespace federation state verifies"):
    val (cas, state) = validFederation()
    val closure = verifyFederationState(state, cas).fold(e => fail(e), identity)
    assert(closure.contains(state.repository))
    assert(closure.contains(state.applications))
    assert(closure.contains(state.namespaces))

  test("verifyFederationState rejects a ledger digest absent from CAS"):
    val (cas, state) = validFederation()
    val bad = state.copy(ledger = Digest.of(Canon.CStr("never-persisted")))
    assert(verifyFederationState(bad, cas).isLeft)

  test("verifyFederationState rejects a repository index digest absent from CAS"):
    val (cas, state) = validFederation()
    val bad = state.copy(repository = Digest.of(Canon.CStr("never-persisted")))
    assert(verifyFederationState(bad, cas).isLeft)

  test("verifyFederationState rejects an applications index digest absent from CAS"):
    val (cas, state) = validFederation()
    val bad = state.copy(applications = Digest.of(Canon.CStr("never-persisted")))
    assert(verifyFederationState(bad, cas).isLeft)

  test("verifyFederationState rejects a namespaces index digest absent from CAS"):
    val (cas, state) = validFederation()
    val bad = state.copy(namespaces = Digest.of(Canon.CStr("never-persisted")))
    assert(verifyFederationState(bad, cas).isLeft)

  test("verifyFederationState rejects a trustRoots digest absent from CAS"):
    val (cas, state) = validFederation()
    val bad = state.copy(trustRoots = Digest.of(Canon.CStr("never-persisted")))
    assert(verifyFederationState(bad, cas).isLeft)

  test("verifyFederationState rejects a gcEpoch digest absent from CAS"):
    val (cas, state) = validFederation()
    val bad = state.copy(gcEpoch = Digest.of(Canon.CStr("never-persisted")))
    assert(verifyFederationState(bad, cas).isLeft)
