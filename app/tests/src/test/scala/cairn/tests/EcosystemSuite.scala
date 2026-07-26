package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.systemhandler.{Keypair, MemCas, Node}
import java.nio.file.Files

class EcosystemSuite extends munit.FunSuite:
  private val alice = Keypair.dev("alice")
  private val bob = Keypair.dev("bob")
  private val app1 = ApplicationManifest("demo-1", Nil, Nil)
  private val app2 = ApplicationManifest("demo-2", Nil, Nil)
  private val lang1 = Digest.of(Canon.CStr("lang-1"))
  private val lang2 = Digest.of(Canon.CStr("lang-2"))
  private val lang3 = Digest.of(Canon.CStr("lang-3"))
  private val migration12 = LangMigration(lang1, lang2, Map.empty, Map.empty)
  private val migration23 = LangMigration(lang2, lang3, Map.empty, Map.empty)

  private def policy(published: Set[String] = Set.empty, requirePublished: Boolean = false) =
    EcosystemTrustPolicy(Map("alice" -> alice.publicBytes), Map("org.demo" -> Set("alice")),
      requirePublished = requirePublished)

  test("signed releases support trusted semantic-version discovery"):
    val cas = MemCas()
    List(app1.artifact, app2.artifact).foreach(cas.put)
    val v1 = EcosystemBundles.sign("org.demo", SemanticVersion(1, 0, 0), app1.digest,
      EcosystemRootKind.Application, Nil, Nil, alice)
    val v2 = EcosystemBundles.sign("org.demo", SemanticVersion(1, 2, 0), app2.digest,
      EcosystemRootKind.Application, Nil, List(v1.digest), alice)
    List(v1.artifact, v2.artifact).foreach(cas.put)
    val registry = EcosystemRegistry(cas, policy(), Set.empty)
    registry.ingest(v2.digest).fold(e => fail(e), identity)
    registry.ingest(v1.digest).fold(e => fail(e), identity)
    assertEquals(registry.versions("org.demo").map(_._1.render), List("1.0.0", "1.2.0"))
    assertEquals(registry.latest("org.demo").map(_.digest), Some(v2.digest))
    val equivocation = EcosystemBundles.sign("org.demo", SemanticVersion(1, 2, 0), app1.digest,
      EcosystemRootKind.Application, Nil, Nil, alice)
    cas.put(equivocation.artifact)
    assert(registry.ingest(equivocation.digest).left.exists(_.contains("equivocating")))

  test("trust policy rejects forged keys, namespace violations, and revocation"):
    val cas = MemCas()
    cas.put(app1.artifact)
    val byBob = EcosystemBundles.sign("org.demo", SemanticVersion(1, 0, 0), app1.digest,
      EcosystemRootKind.Application, Nil, Nil, bob)
    cas.put(byBob.artifact)
    assert(EcosystemRegistry(cas, policy(), Set.empty).ingest(byBob.digest).isLeft)

    val good = EcosystemBundles.sign("org.demo", SemanticVersion(1, 0, 0), app1.digest,
      EcosystemRootKind.Application, Nil, Nil, alice)
    cas.put(good.artifact)
    val revoked = policy().copy(revokedBundles = Set(good.digest))
    assert(EcosystemRegistry(cas, revoked, Set.empty).ingest(good.digest).isLeft)
    val forged = good.copy(signature = bob.sign(good.release.signingBytes))
    cas.put(forged.artifact)
    assert(EcosystemRegistry(cas, policy(), Set.empty).ingest(forged.digest).isLeft)

  test("migration discovery finds a trusted multi-release route"):
    val cas = MemCas()
    List(app1.artifact, migration12.artifact, migration23.artifact).foreach(cas.put)
    val v1 = EcosystemBundles.sign("org.demo", SemanticVersion(1, 0, 0), app1.digest,
      EcosystemRootKind.Application, List(migration12.artifact.digest), Nil, alice)
    val v2 = EcosystemBundles.sign("org.demo", SemanticVersion(2, 0, 0), app1.digest,
      EcosystemRootKind.Application, List(migration23.artifact.digest), List(v1.digest), alice)
    List(v1.artifact, v2.artifact).foreach(cas.put)
    val registry = EcosystemRegistry(cas, policy(), Set.empty)
    registry.ingest(v1.digest).toOption.get
    registry.ingest(v2.digest).toOption.get
    val route = registry.migrationRoute("org.demo", lang1, lang3).fold(e => fail(e), identity)
    assertEquals(route.map(m => m.fromLang -> m.toLang), List(lang1 -> lang2, lang2 -> lang3))
    assert(registry.migrationRoute("org.demo", lang3, lang1).isLeft)

  test("publication anchors the exact signed bundle in the ledger"):
    val node = Node(Files.createTempDirectory("cairn-ecosystem-node"), EffectContexts.forLedger())
    val authorities = Map(alice.name -> alice.publicBytes)
    node.cas.put(app1.artifact)
    val bundle = EcosystemBundles.sign("org.demo", SemanticVersion(1, 0, 0), app1.digest,
      EcosystemRootKind.Application, Nil, Nil, alice)
    EcosystemBundles.publish(bundle, node, alice, authorities).fold(e => fail(e), identity)
    val state = node.state(authorities).fold(e => fail(e), identity)
    assert(state.published.contains(bundle.artifact.key.render))
    val registry = EcosystemRegistry(node.cas, policy(state.published, requirePublished = true), state.published)
    assertEquals(registry.ingest(bundle.digest).map(_.digest), Right(bundle.digest))

  test("replication installs the signed bundle and its application closure"):
    val origin = MemCas()
    origin.put(app1.artifact)
    val bundle = EcosystemBundles.sign("org.demo", SemanticVersion(1, 0, 0), app1.digest,
      EcosystemRootKind.Application, Nil, Nil, alice)
    origin.put(bundle.artifact)
    val replica = MemCas()
    val graph = EcosystemReplication.pull(bundle.digest, origin, replica).fold(e => fail(e), identity)
    assertEquals(graph, Set(bundle.digest, app1.digest))
    assert(replica.contains(bundle.digest) && replica.contains(app1.digest))
