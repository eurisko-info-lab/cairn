package cairn.tests

import cairn.kernel.*
import cairn.core.*

/** PR31: canonical identity for the six-digest [[FederationState]] and the
  * [[FederationTransition]]/index artifacts that populate it. Purely
  * structural at this slice — no BFT/GC/namespace behavior yet.
  */
class FederationStateSuite extends munit.FunSuite:
  private def digestOf(s: String): Digest = Digest.of(Canon.CStr(s))

  private val ledger = digestOf("ledger")
  private val repository = digestOf("repository")
  private val applications = digestOf("applications")
  private val namespaces = digestOf("namespaces")
  private val trustRoots = digestOf("trustRoots")
  private val gcEpoch = digestOf("gcEpoch")

  private def state = FederationState(ledger, repository, applications, namespaces, trustRoots, gcEpoch)

  test("FederationState: canon/artifact round-trip"):
    val s = state
    val back = FederationState.fromArtifact(s.artifact).fold(e => fail(e), identity)
    assertEquals(back, s)
    assertEquals(back.digest, s.digest)

  test("FederationState: dependencies is exactly the six roots, in field order"):
    assertEquals(state.dependencies, List(ledger, repository, applications, namespaces, trustRoots, gcEpoch))

  test("FederationState: changing any one field changes the digest"):
    val base = state
    assert(base.copy(ledger = digestOf("other")).digest != base.digest)
    assert(base.copy(repository = digestOf("other")).digest != base.digest)
    assert(base.copy(applications = digestOf("other")).digest != base.digest)
    assert(base.copy(namespaces = digestOf("other")).digest != base.digest)
    assert(base.copy(trustRoots = digestOf("other")).digest != base.digest)
    assert(base.copy(gcEpoch = digestOf("other")).digest != base.digest)

  test("FederationState.genesis: empty indices, generation-0 GC epoch"):
    val chainGenesis = digestOf("chain-genesis")
    val replicaSet = digestOf("replica-set")
    val g = FederationState.genesis(chainGenesis, replicaSet)
    assertEquals(g.ledger, chainGenesis)
    assertEquals(g.trustRoots, replicaSet)
    assertEquals(g.repository, RepositoryIndex(Map.empty).digest)
    assertEquals(g.applications, ApplicationIndex(Map.empty).digest)
    assertEquals(g.namespaces, NamespaceIndex(Map.empty).digest)
    assertEquals(g.gcEpoch, ReplicatedGcEpoch(0, Set.empty, None).digest)

  test("FederationState.fromArtifact rejects a non-federation-state artifact"):
    assert(FederationState.fromArtifact(Artifact(ArtifactKind.ChangeSet, Canon.CStr("nope"))).isLeft)

  test("FederationTransition: canon/artifact round-trip, dependencies include before/after/transactions/approvals/finality"):
    val before = digestOf("before")
    val after = digestOf("after")
    val tx1 = digestOf("tx1")
    val tx2 = digestOf("tx2")
    val approval = digestOf("approval")
    val cert = digestOf("cert")
    val t = FederationTransition(before, List(tx1, tx2), after, List(approval), Some(cert))
    val back = FederationTransition.fromArtifact(t.artifact).fold(e => fail(e), identity)
    assertEquals(back, t)
    assertEquals(t.dependencies.toSet, Set(before, after, tx1, tx2, approval, cert))

  test("FederationTransition: finality = None round-trips"):
    val t = FederationTransition(digestOf("before"), Nil, digestOf("after"), Nil, None)
    val back = FederationTransition.fromArtifact(t.artifact).fold(e => fail(e), identity)
    assertEquals(back, t)
    assertEquals(back.finality, None)

  test("RepositoryIndex/ApplicationIndex/NamespaceIndex: canon round-trip and dependencies"):
    val a = digestOf("ns-a")
    val b = digestOf("ns-b")
    val repoIdx = RepositoryIndex(Map("a" -> a, "b" -> b))
    assertEquals(RepositoryIndex.fromArtifact(repoIdx.artifact), Right(repoIdx))
    assertEquals(repoIdx.dependencies, List(a, b).sortBy(_.hex))

    val appIdx = ApplicationIndex(Map("a" -> a, "b" -> b))
    assertEquals(ApplicationIndex.fromArtifact(appIdx.artifact), Right(appIdx))

    val nsIdx = NamespaceIndex(Map("a" -> a, "b" -> b))
    assertEquals(NamespaceIndex.fromArtifact(nsIdx.artifact), Right(nsIdx))

  test("index artifacts of the same entries under different kinds are NOT interchangeable"):
    val a = digestOf("x")
    val repoIdx = RepositoryIndex(Map("ns" -> a))
    val appIdx = ApplicationIndex(Map("ns" -> a))
    assert(ApplicationIndex.fromArtifact(repoIdx.artifact).isLeft)
    assert(RepositoryIndex.fromArtifact(appIdx.artifact).isLeft)

  test("ArtifactDependencies.direct resolves all five new kinds"):
    val s = state
    assertEquals(ArtifactDependencies.direct(s.artifact), Right(s.dependencies))
    val t = FederationTransition(digestOf("before"), Nil, digestOf("after"), Nil, None)
    assertEquals(ArtifactDependencies.direct(t.artifact).map(_.toSet), Right(t.dependencies.toSet))
    val repoIdx = RepositoryIndex(Map("ns" -> digestOf("x")))
    assertEquals(ArtifactDependencies.direct(repoIdx.artifact), Right(repoIdx.dependencies))
