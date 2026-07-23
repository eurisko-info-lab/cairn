package cairn.examples.claims

import cairn.kernel.*
import cairn.proof.*
import cairn.core.Module

/** Claims pack (S24, §5): proof-free properties over STLC programs backed by
  * test certificates, alongside the certified proof path.
  */
object Claims:
  import cairn.examples.stlc.Stlc

  /** A golden STLC typing derivation (S22):
    * ⊢ (fun x : Bool . x) true : Bool
    */
  def idAppliedDerivation: Derivation =
    def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)
    val ctxNil = n("ctxNil")
    val idTerm = Stlc.idBool
    val conclusion = n("hasType", ctxNil, Stlc.app1(idTerm, Stlc.tru), n("tyBool"))
    Derivation("t-app", conclusion, List(
      Derivation("t-abs",
        n("hasType", ctxNil, idTerm, n("arrow", n("tyBool"), n("tyBool"))),
        List(
          // t-var's lookup premise is computational ($ctx-lookup): no sub-derivation
          Derivation("t-var",
            n("hasType", n("ctxCons", Cst.Leaf("x"), n("tyBool"), ctxNil), Stlc.v("x"), n("tyBool")),
            Nil))),
      Derivation("t-true", n("hasType", ctxNil, n("true"), n("tyBool")), Nil)))

  def typingJudgments: List[JudgmentDef] =
    Stlc.language.judgments.values.toList

  /** Proof-free claim: "idBool applied twice is identity on booleans",
    * evidenced by a test suite run under the generic tree engine.
    */
  def doubleIdClaim: (Claim, TestSuite) =
    val subject = Module(List("id" -> Stlc.idBool)).digest
    val claim = Claim("double-id-is-id",
      Cst.node("claimEq",
        Cst.node("app", Stlc.idBool, Cst.node("app", Stlc.idBool, Cst.Leaf("$b"))),
        Cst.Leaf("$b")),
      subject)
    val suite = TestSuite("double-id-tests", subject, List(
      TestCase("on-true", Stlc.app1(Stlc.idBool, Stlc.app1(Stlc.idBool, Stlc.tru)), Stlc.tru),
      TestCase("on-false", Stlc.app1(Stlc.idBool, Stlc.app1(Stlc.idBool, Stlc.fls)), Stlc.fls)))
    (claim, suite)
