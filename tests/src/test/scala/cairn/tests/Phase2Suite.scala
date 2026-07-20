package cairn.tests

import cairn.kernel.*
import cairn.compute.*
import cairn.proof.*
import cairn.examples.stlc.Stlc
import cairn.examples.claims.Claims

/** Phase 2 acceptance (S19–S24): typing derivation checks; forged proof
  * rejected; claim+tests path works without proofs.
  */
class Phase2Suite extends munit.FunSuite:
  val judgments = Claims.typingJudgments

  test("proof artifacts round-trip (S19)"):
    val d = Claims.idAppliedDerivation
    assertEquals(Derivation.fromCanon(d.canon), d)
    val (claim, suite) = Claims.doubleIdClaim
    assert(claim.artifact.digest != suite.artifact.digest)

  test("STLC typing derivation checks (S21/S22 acceptance)"):
    assertEquals(Checker.check(judgments, Claims.idAppliedDerivation), Right(()))

  test("wrong-type derivation fails (S22)"):
    val bad = Claims.idAppliedDerivation.copy(
      conclusion = Cst.node("hasType", Cst.node("ctxNil"),
        Stlc.app1(Stlc.idBool, Stlc.tru), Cst.node("arrow", Cst.node("tyBool"), Cst.node("tyBool"))))
    val res = Checker.check(judgments, bad)
    assert(res.isLeft)
    // premise metavariable U was already forced to tyBool by the sub-derivations
    assert(res.swap.exists(_.render.nonEmpty))

  test("forged proof rejected: tampered rule instance (S23)"):
    val d = Claims.idAppliedDerivation
    // claim true : Bool via t-false — not an instance
    val forged = Derivation("t-false", Cst.node("hasType", Cst.node("ctxNil"), Cst.node("true"), Cst.node("tyBool")), Nil)
    assert(Checker.check(judgments, forged).isLeft)
    // tampered bytes change the digest => certificate no longer matches
    val (thm, cert) = Certify.byProof(judgments, 
      Claim("id-app-typing", d.conclusion, Stlc.language.digest), d).toOption.get
    val tampered = d.copy(rule = "t-if")
    assert(tampered.artifact.digest != thm.proof)
    assert(Checker.check(judgments, tampered).isLeft)

  test("unknown rule rejected (S21)"):
    val d = Derivation("t-fake", Cst.node("hasType"), Nil)
    assert(Checker.check(judgments, d).swap.exists(_.render.contains("unknown inference rule")))

  test("certified path issues theorem + certificate (S22)"):
    val d = Claims.idAppliedDerivation
    val claim = Claim("id-app-typing", d.conclusion, Stlc.language.digest)
    Certify.byProof(judgments, claim, d) match
      case Right((thm, cert)) =>
        assertEquals(cert.method, "proof-term")
        assertEquals(thm.proof, d.artifact.digest)
        assertEquals(cert.claim, claim.artifact.digest)
      case Left(e) => fail(e)

  test("proof/claim mismatch rejected (S23)"):
    val d = Claims.idAppliedDerivation
    val wrongClaim = Claim("wrong", Cst.node("hasType", Cst.node("ctxNil"), Stlc.tru, Cst.node("tyBool")), Stlc.language.digest)
    assert(Certify.byProof(judgments, wrongClaim, d).isLeft)

  test("proof-free claim with test certificate (S24 acceptance)"):
    val (claim, suite) = Claims.doubleIdClaim
    val eval = (t: Cst) => TreeEngine.normalize(Stlc.language, t)
    Certify.byTests(claim, suite, eval) match
      case Right(cert) =>
        assertEquals(cert.method, "test-suite")
        assertEquals(cert.evidence, suite.artifact.digest)
      case Left(e) => fail(e)

  test("failing suite yields no certificate (S24)"):
    val (claim, _) = Claims.doubleIdClaim
    val badSuite = TestSuite("bad", claim.subject,
      List(TestCase("wrong", Stlc.app1(Stlc.idBool, Stlc.tru), Stlc.fls)))
    val eval = (t: Cst) => TreeEngine.normalize(Stlc.language, t)
    assert(Certify.byTests(claim, badSuite, eval).isLeft)
