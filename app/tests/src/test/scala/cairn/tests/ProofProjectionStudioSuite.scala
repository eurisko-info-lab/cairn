package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.proof.*
import cairn.user.quicksort.QuickSort2

class ProofProjectionStudioSuite extends munit.FunSuite:
  private val statement = Cst.node("holds", Cst.Leaf("subject"))
  private val judgment = JudgmentDef("holds", List(InferRule("holds-intro", Nil,
    Cst.node("holds", Cst.Leaf("$x")), Nil)))
  private val cfg = CheckerCfg(List(judgment))
  private val subject = Digest.of(Canon.CStr("proof-subject"))

  test("goals become checked derivations, proof terms, theorems, and certificates"):
    val goal = ProofGoal("subject-holds", "holds", statement, subject)
    val workspace = ProofProjectionWorkspace().addGoal(goal, cfg).fold(e => fail(e), identity)
    val proved = workspace.proved.head
    assertEquals(proved.derivation.conclusion, statement)
    assertEquals(proved.validate(cfg), Right(()))
    assertEquals(proved.certificate.method, "proof-term")
    assertEquals(proved.certificate.evidence, proved.derivation.artifact.digest)
    assertEquals(workspace.evidenceArtifacts.map(_.kind).toSet,
      Set(ArtifactKind.ProofGoal, ArtifactKind.ProofTerm, ArtifactKind.Theorem, ArtifactKind.Certificate))

  test("Rosetta outputs are evidence with explicit obligations, not proof certificates"):
    val workspace = ProofProjectionWorkspace().project(QuickSort2.module).fold(e => fail(e), identity)
    val targets = workspace.projections.map(_.target).toSet
    assert(Set("scala", "lean", "haskell", "rust").subsetOf(targets))
    val lean = workspace.projections.find(_.target == "lean").get
    assert(String(lean.bytes.toArray, "UTF-8").contains("sorry"))
    assert(lean.obligations.contains("quicksort_sorted"))
    assertEquals(workspace.proved, Nil)
    assert(workspace.projections.forall(_.artifact.kind == ArtifactKind.ProjectionEvidence))

  test("Lean and HVM agreement certificates attach only after envelope checking"):
    val outcome = Agreement.outcome("ok")
    val lean = Agreement.certify(Agreement.leanCore, "lean-case", subject, outcome, outcome,
      Agreement.NativeSource.Golden).toOption.get
    val hvm = Agreement.certify(Agreement.hvmIc, "hvm-case", subject, outcome, outcome,
      Agreement.NativeSource.Live("hvm", "fixture")).toOption.get
    val workspace = ProofProjectionWorkspace().attachAgreement(lean).flatMap(_.attachAgreement(hvm))
      .fold(e => fail(e), identity)
    assertEquals(workspace.agreements.map(_.envelopeId), List("lean-core", "hvm-ic"))
    assert(workspace.evidenceArtifacts.count(_.kind == ArtifactKind.AgreementCertificate) == 2)

    val forged = lean.copy(nativeResult = Agreement.outcome("different"), agreed = true)
    assert(ProofProjectionWorkspace().attachAgreement(forged).isLeft)

  test("a failed or forged derivation cannot mint a workspace certificate"):
    val impossible = ProofGoal("impossible", "other", Cst.node("other", Cst.Leaf("x")), subject)
    assert(ProofProjectionWorkspace().addGoal(impossible, cfg, depth = 4).isLeft)
