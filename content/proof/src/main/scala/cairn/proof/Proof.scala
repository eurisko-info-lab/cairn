package cairn.proof

import cairn.kernel.*

/** Claims, theorems, test suites and certificates (S19, S24, §2 Claim vs proof).
  *
  * MIGRATION-PLAN.md Phase 2 (first slice): `Derivation`/`CheckerCfg`/
  * `Checker` moved to `kernel` (the independent validator), `Search`/
  * `Tactics` moved to `core` (untrusted proposers) — both already imported
  * via `cairn.kernel.*` above; `Certify.byProof` calls `Checker.check`
  * directly and needs nothing from `core`. What's left here — `Claim`,
  * `Theorem`, `TestSuite`, `Certificate`, `Certify` — sits above both and
  * doesn't fit cleanly into either side.
  * A Claim may exist proof-free; a Theorem carries a checked proof term.
  */
final case class Claim(name: String, statement: Cst, subject: Digest):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "statement" -> Cst.toCanon(statement),
    "subject" -> Canon.CStr(subject.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Claim, canon)

final case class Theorem(claim: Claim, proof: Digest):
  def canon: Canon = Canon.cmap("claim" -> claim.canon, "proof" -> Canon.CStr(proof.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Theorem, canon)

final case class TestCase(name: String, input: Cst, expected: Cst)
final case class TestSuite(name: String, subject: Digest, cases: List[TestCase]):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "subject" -> Canon.CStr(subject.hex),
    "cases" -> Canon.CList(cases.map(tc => Canon.cmap(
      "name" -> Canon.CStr(tc.name),
      "input" -> Cst.toCanon(tc.input),
      "expected" -> Cst.toCanon(tc.expected)))))
  def artifact: Artifact = Artifact(ArtifactKind.TestSuite, canon)

/** Kernel-issued record that a claim was validated by a given method. */
final case class Certificate(claim: Digest, method: String, evidence: Digest):
  def canon: Canon = Canon.cmap(
    "claim" -> Canon.CStr(claim.hex),
    "method" -> Canon.CStr(method), // "proof-term" | "test-suite"
    "evidence" -> Canon.CStr(evidence.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)

object Certify:
  /** Certified path: check the proof term, then issue a certificate. */
  def byProof(judgments: List[JudgmentDef], claim: Claim, proof: Derivation): Either[String, (Theorem, Certificate)] =
    if proof.conclusion != claim.statement then
      Left(s"proof concludes ${proof.conclusion.render}, claim states ${claim.statement.render}")
    else
      Checker.check(judgments, proof)
        .left.map(_.render)
        .map { _ =>
          val thm = Theorem(claim, proof.artifact.digest)
          (thm, Certificate(claim.artifact.digest, "proof-term", proof.artifact.digest)) }

  /** Proof-free path: run a test suite with a supplied evaluator (§6 Phase 2). */
  def byTests(claim: Claim, suite: TestSuite, eval: Cst => Either[String, Cst]): Either[String, Certificate] =
    val failures = suite.cases.flatMap { tc =>
      eval(tc.input) match
        case Right(v) if v == tc.expected => Nil
        case Right(v)  => List(s"${tc.name}: got ${v.render}, expected ${tc.expected.render}")
        case Left(err) => List(s"${tc.name}: evaluation failed: $err")
    }
    if failures.nonEmpty then Left(failures.mkString("; "))
    else Right(Certificate(claim.artifact.digest, "test-suite", suite.artifact.digest))
