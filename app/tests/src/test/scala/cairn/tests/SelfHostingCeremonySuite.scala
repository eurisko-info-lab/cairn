package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.proof.*
import cairn.runtime.*
import cairn.systemhandler.{DiskCas, Keypair, Node}
import java.nio.file.Files

class SelfHostingCeremonySuite extends munit.FunSuite:
  private val publisher = Keypair.dev("cairn-publisher")
  private val authorities = Map(publisher.name -> publisher.publicBytes)

  private def installLanguage(node: Node, language: ComposedLanguage, capabilities: ResolvedLanguageCapabilities): ApplicationLanguage =
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(language.grammar))
    language.fragments.foreach(f => node.cas.put(f.artifact))
    val selected = capabilities.validation.map(_.artifact).toList ++ capabilities.migrations ++
      capabilities.queries ++ capabilities.policies ++ capabilities.projections
    (List(language.artifact, grammar, capabilities.change.semantics.artifact,
      capabilities.change.surface.artifact, capabilities.descriptor.artifact) ++ selected).foreach(node.cas.put)
    ApplicationLanguage(language.name, language.digest, grammar.digest, capabilities.descriptor.digest)

  private def initial(node: Node): (SignedEcosystemBundle, ResolvedLanguageCapabilities, StudioProfileSurface) =
    val change = ChangeCapability.standard
    val validation = ValidationModel(Meta.language.digest, Nil, Nil)
    val migration = LangMigration(Meta.language.digest, Meta.language.digest, Map.empty, Map.empty)
    val profileSemantics = StudioProfileSemantics(Meta.language.digest, List("Fragment"), Nil, Nil, Nil, Nil)
    val profileSurface = StudioProfileSurface(profileSemantics.digest, Map.empty,
      Map("language" -> "Cairn language"), Nil, Map.empty, Nil)
    val descriptor = LanguageCapabilities(Meta.language.digest, change.semantics.digest, change.surface.digest,
      Some(validation.digest), List(migration.artifact.digest), Nil, Nil,
      List(profileSemantics.artifact.digest, profileSurface.artifact.digest))
    val meta = ResolvedLanguageCapabilities.check(descriptor, Meta.language, change, Some(validation),
      List(migration.artifact), Nil, Nil, List(profileSemantics.artifact, profileSurface.artifact)).toOption.get
    val grammar = LanguageCapabilities.standard(Meta.grammarLanguage)
    val manifest = ApplicationManifest("cairn", List(
      installLanguage(node, Meta.language, meta).copy(name = "meta"),
      installLanguage(node, Meta.grammarLanguage, grammar).copy(name = "grammar")), Nil)
    node.cas.put(manifest.artifact)
    val bundle = EcosystemBundles.sign("org.cairn", SemanticVersion(1, 0, 0), manifest.digest,
      EcosystemRootKind.Application, List(migration.artifact.digest), Nil, publisher)
    EcosystemBundles.publish(bundle, node, publisher, authorities).toOption.get
    (bundle, meta, profileSurface)

  test("PR25 acceptance ceremony closes the signed two-node self-hosting loop"):
    val node = Node(Files.createTempDirectory("cairn-pr25-ceremony"), EffectContexts.forLedger())
    val (bundle, meta, oldProfile) = initial(node)
    val revisedGrammar = Meta.language.grammar.copy(tokens = Meta.language.grammar.tokens.copy(
      keywords = Meta.language.grammar.tokens.keywords :+ "pr25-closed"))
    val newProfile = oldProfile.copy(labels = oldProfile.labels.updated("language", "Cairn self-hosted language"))
    val newDescriptor = meta.descriptor.copy(projections = meta.descriptor.projections.map {
      case d if d == oldProfile.artifact.digest => newProfile.artifact.digest
      case d => d })
    val edits = List(
      LanguageStudioEdit(LanguageAssetId(LanguageAssetKind.Grammar, "meta"),
        Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(revisedGrammar))),
      LanguageStudioEdit(LanguageAssetId(LanguageAssetKind.StudioProfileSurface, "default"), newProfile.artifact),
      LanguageStudioEdit(LanguageAssetId(LanguageAssetKind.LanguageCapabilities, "meta"), newDescriptor.artifact))

    val statement = Cst.node("holds", Cst.Leaf("cairn-successor"))
    val judgment = JudgmentDef("holds", List(InferRule("holds-intro", Nil, Cst.node("holds", Cst.Leaf("$x")), Nil)))
    val proof = ProofProjectionWorkspace().addGoal(
      ProofGoal("successor-closure", "holds", statement, bundle.digest), CheckerCfg(List(judgment))).toOption.get
    val projection = ProjectionEvidence(bundle.digest, "audit", "reports/successor.txt",
      "verified successor".getBytes("UTF-8").toVector, Nil)
    val evidence = proof.evidenceArtifacts ++ List(projection.artifact)
    val policy = EcosystemTrustPolicy(Map(publisher.name -> publisher.publicBytes),
      Map("org.cairn" -> Set(publisher.name)), requirePublished = true)

    val firstRoot = Files.createTempDirectory("cairn-pr25-first")
    val secondRoot = Files.createTempDirectory("cairn-pr25-second")
    val first = DiskCas(firstRoot)
    val second = DiskCas(secondRoot)
    val result = SelfHostingCeremony.run(bundle.digest, "meta", edits, evidence,
      publisher, node, authorities, policy, first, second).fold(e => fail(e), identity)
    assert(result.reproducible)
    assert(result.continuationWitness != result.witness)
    assertEquals(result.firstAudit, result.secondAudit)
    assert(result.firstClosure.contains(result.successorBundle))
    val state = node.state(authorities).toOption.get
    assert(state.published.exists(_.contains(result.successorBundle.hex)))

    // New store and resolver objects over the same on-disk bytes model a process restart.
    val restartedSecond = DiskCas(secondRoot)
    val restartedApp = ArtifactApplicationResolver(restartedSecond).resolve(result.successorApplication)
      .fold(e => fail(e), identity)
    assertEquals(ArtifactLanguageStudio.project(restartedApp, restartedSecond, "meta").map(_.digest),
      Right(result.reopenedProject))

    val replica = cairn.systemhandler.MemCas()
    EcosystemReplication.pull(result.successorBundle, node.cas, replica).fold(e => fail(e), identity)
    val report = ApplicationHardeningAuditor(replica, ArtifactApplicationResolver(replica))
      .audit(result.successorBundle).fold(e => fail(e), identity)
    assert(report.trustedClosure.checkedEvidence.exists(_.basis == TrustBasis.Signed))
    assert(report.trustedClosure.checkedEvidence.exists(_.basis == TrustBasis.IndependentlyChecked))
    assert(report.trustedClosure.checkedEvidence.exists(_.basis == TrustBasis.DigestBound))
    assert(report.trustedClosure.hostInterpreters.forall(i => i.interfaceDigest != i.implementationDigest))

  test("optimized dependency discovery carries a model/interpreter/input equivalence witness"):
    val app = ApplicationManifest("equivalence", Nil, Nil).artifact
    val cache = ArtifactDependencyCache()
    val witness = cache.verify(app).fold(e => fail(e), identity)
    assert(witness.valid)
    assertEquals(witness.input, app.digest)
    assertEquals(witness.artifact.kind, ArtifactKind.Trace)
