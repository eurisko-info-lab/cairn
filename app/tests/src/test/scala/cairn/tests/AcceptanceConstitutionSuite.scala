package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** PR15: every acceptance dimension is one canonical decision and one
  * independently replayable evidence object. */
class AcceptanceConstitutionSuite extends munit.FunSuite:
  private val language = Stlc.language
  private val model = ChangeModel.default
  private val validation = ValidationModel(language.digest, Nil, Nil)
  private val gate = ModuleGate.fromValidationModel("stlc", validation, _ => None)
  private val policy = AcceptancePolicy.gated(gate)
  private val agreement = Digest.of(Canon.CStr("domain-agreement"))
  private val migration = Digest.of(Canon.CStr("migration"))
  private val certificate = Digest.of(Canon.CStr("certificate"))
  private val constitution = AcceptanceConstitution(
    model.digest, Some(validation.digest), Some(agreement),
    List(CertificateRequirement(ArtifactKind.Certificate, Some("review-board"), 1)),
    AuthorityRules(Set("alice"), threshold = 1),
    MigrationRules(Set(migration), required = true),
    PublicationRules.Required)
  private val facts = AcceptanceFacts(
    Some(agreement), List(CertificateFact(certificate, ArtifactKind.Certificate, Some("review-board"))),
    Set("alice"), Some(migration), publicationRequested = true)

  test("constitution is a decodable, content-addressed policy artifact"):
    val artifact = Artifact.decode(Canon.encode(constitution.artifact.canon)).fold(e => fail(e), identity)
    assertEquals(AcceptanceConstitution.fromArtifact(artifact), Right(constitution))
    assertEquals(artifact.kind, ArtifactKind.AcceptanceConstitution)

  test("one evaluator covers validation, ancestry, certificates, authority, migration and publication"):
    val result = Module(List("x" -> Stlc.tru))
    assertEquals(AcceptanceConstitutionEvaluator.check(
      constitution, gate, model.digest, result, facts), Right(()))
    assert(AcceptanceConstitutionEvaluator.check(constitution, gate, model.digest, result,
      facts.copy(domainAgreement = None)).isLeft)
    assert(AcceptanceConstitutionEvaluator.check(constitution, gate, model.digest, result,
      facts.copy(certificates = Nil)).isLeft)
    assert(AcceptanceConstitutionEvaluator.check(constitution, gate, model.digest, result,
      facts.copy(authorities = Set.empty)).isLeft)
    assert(AcceptanceConstitutionEvaluator.check(constitution, gate, model.digest, result,
      facts.copy(migration = None)).isLeft)
    assert(AcceptanceConstitutionEvaluator.check(constitution, gate, model.digest, result,
      facts.copy(publicationRequested = false)).isLeft)

  test("AcceptedTip emits complete evidence and a second node re-evaluates the same constitution"):
    val base = Module(Nil)
    val delta = Delta.deltaOf(language).fold(es => fail(es.map(_.render).mkString("\n")), identity)
    val change = Parser.parse(delta.grammar, "{ add x = true ; }").fold(e => fail(e), identity)
    val result = Delta.apply(language, base, change, model).fold(e => fail(e), identity)._1
    val proposed = SemanticRepository.Tip(base, result, change)
    val accepted = AcceptedTip.checkTip(language, proposed, policy, constitution, facts, model)
      .fold(e => fail(e), identity)
    val evidence = accepted.evidence
    assertEquals(evidence.constitution, Some(constitution.digest))
    assertEquals(evidence.domainAgreement, Some(agreement))
    assertEquals(evidence.certificates, List(certificate))
    assertEquals(evidence.authorities, List("alice"))
    assertEquals(evidence.migration, Some(migration))
    assert(evidence.publicationRequested)
    assertEquals(AcceptanceEvidence.verifyComplete(
      language, base, Some(accepted.vcs), policy, constitution, facts,
      result, evidence, model), Right(()))
