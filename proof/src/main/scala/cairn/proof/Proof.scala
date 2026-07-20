package cairn.proof

import cairn.kernel.*

/** L2 proof kernel (S19–S23).
  *
  * A [[Derivation]] is a proof term: a tree of named rule instances. The
  * independent [[Checker.check]] driver is small and decidable: for each node
  * it matches the rule's conclusion pattern against the claimed conclusion and
  * the rule's premise patterns against the sub-derivations' conclusions,
  * threading one metavariable environment (non-linear metavariables must agree).
  * No tactic engine is required to CHECK a proof (§6 Phase 2).
  *
  * Untrusted machinery (search, elaborators, UIs) may PROPOSE derivations;
  * only this checker certifies them (§4.6).
  */
final case class Derivation(rule: String, conclusion: Cst, premises: List[Derivation]):
  def canon: Canon = Canon.cmap(
    "rule" -> Canon.CStr(rule),
    "conclusion" -> Cst.toCanon(conclusion),
    "premises" -> Canon.CList(premises.map(_.canon)))
  def artifact: Artifact = Artifact(ArtifactKind.ProofTerm, canon)

object Derivation:
  def fromCanon(c: Canon): Derivation =
    Derivation(
      c.field("rule").asStr,
      Cst.fromCanon(c.field("conclusion")),
      c.field("premises").asList.map(fromCanon))

final case class CheckError(path: List[String], msg: String):
  def render: String = s"check failure at [${path.mkString(" > ")}]: $msg"

object Checker:
  private type Env = Map[String, Cst]

  private def matchPat(pat: Cst, actual: Cst, env: Env): Option[Env] = (pat, actual) match
    case (Cst.Leaf(p), _) if p.startsWith("$") =>
      env.get(p.drop(1)) match
        case Some(bound) => if bound == actual then Some(env) else None
        case None        => Some(env + (p.drop(1) -> actual))
    case (Cst.Leaf(p), Cst.Leaf(a)) => if p == a then Some(env) else None
    case (Cst.Node(pc, ps), Cst.Node(ac, as)) if pc == ac && ps.length == as.length =>
      ps.zip(as).foldLeft[Option[Env]](Some(env)) { case (acc, (p, a)) => acc.flatMap(matchPat(p, a, _)) }
    case _ => None

  /** Check a derivation against an inference system (a set of judgments). */
  def check(judgments: List[JudgmentDef], d: Derivation): Either[CheckError, Unit] =
    val rules: Map[String, InferRule] =
      judgments.flatMap(_.rules).map(r => r.name -> r).toMap
    def go(d: Derivation, path: List[String]): Either[CheckError, Unit] =
      rules.get(d.rule) match
        case None => Left(CheckError(path, s"unknown inference rule '${d.rule}'"))
        case Some(rule) =>
          if rule.premises.length != d.premises.length then
            Left(CheckError(path, s"rule '${d.rule}' expects ${rule.premises.length} premises, derivation has ${d.premises.length}"))
          else
            matchPat(rule.conclusion, d.conclusion, Map.empty) match
              case None => Left(CheckError(path,
                s"conclusion ${d.conclusion.render} is not an instance of rule '${d.rule}' (${rule.conclusion.render})"))
              case Some(env0) =>
                val premCheck = rule.premises.zip(d.premises).zipWithIndex
                  .foldLeft[Either[CheckError, Env]](Right(env0)) {
                    case (acc, ((pat, sub), i)) =>
                      acc.flatMap { env =>
                        matchPat(pat, sub.conclusion, env) match
                          case Some(env2) => Right(env2)
                          case None => Left(CheckError(path :+ s"${d.rule}#$i",
                            s"premise ${sub.conclusion.render} is not an instance of ${pat.render} under bindings so far"))
                      }
                  }
                premCheck.flatMap { _ =>
                  d.premises.zipWithIndex.foldLeft[Either[CheckError, Unit]](Right(())) {
                    case (acc, (sub, i)) => acc.flatMap(_ => go(sub, path :+ s"${d.rule}#$i")) }
                }
    go(d, List("root"))

/** Claims, theorems, test suites and certificates (S19, S24, §2 Claim vs proof).
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
