package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{BftFinality, CasEffects, DiskCas, FederationFinality, Keypair}
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
    val state = FederationState(
      ledger = Digest.of(Canon.CStr("ledger")), repository = Digest.of(Canon.CStr("repository")),
      applications = Digest.of(Canon.CStr("applications")), namespaces = Digest.of(Canon.CStr("namespaces")),
      trustRoots = manifest.digest, gcEpoch = epoch.digest)
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = state.digest, epoch = 1L,
      previousState = Digest.of(Canon.CStr("genesis")), federationId = federationId).fold(e => fail(e), identity)

    val report = FederationGc.reclaimAgainstFinalizedEpoch(
      casRoot, state, cas, cert, manifest, federationId, casCtx).fold(e => fail(e), identity)
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
    val certForDifferentState = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = differentState.digest, epoch = 1L,
      previousState = Digest.of(Canon.CStr("genesis")), federationId = federationId).fold(e => fail(e), identity)
    val rejected = FederationGc.reclaimAgainstFinalizedEpoch(
      dir.resolve("cas"), state, cas, certForDifferentState, manifest, federationId, casCtx)
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
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = state.digest, epoch = 1L,
      previousState = Digest.of(Canon.CStr("genesis")), federationId = federationId).fold(e => fail(e), identity)
    val wrongFederation = Digest.of(Canon.CStr("some-other-federation"))
    val rejected = FederationGc.reclaimAgainstFinalizedEpoch(
      dir.resolve("cas"), state, cas, cert, manifest, wrongFederation, casCtx)
    assert(rejected.left.exists(_.contains("federation id mismatch")), rejected.toString)
