package cairn.proof

import cairn.kernel.*
import cairn.core.*
import java.nio.charset.StandardCharsets.UTF_8

final case class ProofGoal(name: String, judgment: String, statement: Cst, subject: Digest):
  require(name.nonEmpty && judgment.nonEmpty, "proof goal name and judgment are required")
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name), "judgment" -> Canon.CStr(judgment),
    "statement" -> Cst.toCanon(statement), "subject" -> Canon.CStr(subject.hex))
  def artifact: Artifact = Artifact(ArtifactKind.ProofGoal, canon)

final case class ProjectionEvidence(
    source: Digest, target: String, path: String, bytes: Vector[Byte], obligations: List[String],
):
  def canon: Canon = Canon.cmap(
    "source" -> Canon.CStr(source.hex), "target" -> Canon.CStr(target),
    "path" -> Canon.CStr(path), "bytes" -> Canon.CBytes(bytes),
    "output" -> Canon.CStr(Digest.ofBytes(bytes.toArray).hex),
    "obligations" -> Canon.cstrs(obligations.sorted))
  def artifact: Artifact = Artifact(ArtifactKind.ProjectionEvidence, canon)

final case class ProvedGoal(goal: ProofGoal, derivation: Derivation, theorem: Theorem, certificate: Certificate):
  def validate(cfg: CheckerCfg): Either[String, Unit] = for
    _ <- Either.cond(derivation.conclusion == goal.statement, (), "derivation does not conclude the goal")
    _ <- Checker.check(cfg, derivation).left.map(_.render)
    _ <- Either.cond(theorem.claim.artifact.digest == certificate.claim, (), "certificate names another claim")
    _ <- Either.cond(theorem.proof == derivation.artifact.digest && certificate.evidence == derivation.artifact.digest, (),
      "theorem/certificate do not cite the checked proof term")
  yield ()

final case class ProofProjectionWorkspace(
    proved: List[ProvedGoal] = Nil,
    projections: List[ProjectionEvidence] = Nil,
    agreements: List[Agreement.AgreementCertificate] = Nil,
):
  def evidenceArtifacts: List[Artifact] =
    proved.flatMap(p => List(p.goal.artifact, p.derivation.artifact, p.theorem.artifact, p.certificate.artifact)) ++
      projections.map(_.artifact) ++ agreements.map(_.artifact)
  def artifact: Artifact = Artifact(ArtifactKind.Provenance, Canon.CTag("proof-projection-workspace", Canon.cmap(
    "proved" -> Canon.cstrs(proved.map(_.certificate.artifact.digest.hex)),
    "projections" -> Canon.cstrs(projections.map(_.artifact.digest.hex)),
    "agreements" -> Canon.cstrs(agreements.map(_.artifact.digest.hex)))))

  def addGoal(goal: ProofGoal, cfg: CheckerCfg, depth: Int = 64): Either[String, ProofProjectionWorkspace] =
    for
      derivation <- Search.prove(cfg, goal.statement, depth)
      issued <- Certify.byProof(cfg.judgments, Claim(goal.name, goal.statement, goal.subject), derivation)
      (theorem, certificate) = issued
      result = ProvedGoal(goal, derivation, theorem, certificate)
      _ <- result.validate(cfg)
    yield copy(proved = proved :+ result)

  /** Rosetta output remains projection evidence. In particular, Lean `sorry`
    * obligations never become proof certificates. */
  def project(module: RosettaModule2): Either[String, ProofProjectionWorkspace] =
    ScaffoldPlan.plan(module).map { (_, writes) =>
      val theoremNames = module.theorems.map(_.name)
      val evidence = writes.map { (path, text) =>
        val target = path.takeWhile(_ != '/')
        ProjectionEvidence(module.artifact.digest, target, path, text.getBytes(UTF_8).toVector,
          if path == "obligations.json" then theoremNames else theoremNames)
      }
      copy(projections = projections ++ evidence)
    }

  def attachAgreement(cert: Agreement.AgreementCertificate): Either[String, ProofProjectionWorkspace] =
    val envelope = cert.envelopeId match
      case Agreement.leanCore.id => Some(Agreement.leanCore)
      case Agreement.hvmIc.id => Some(Agreement.hvmIc)
      case _ => None
    for
      selected <- envelope.toRight(s"unknown agreement envelope '${cert.envelopeId}'")
      checked <- Agreement.check(cert, Some(selected))
    yield copy(agreements = agreements :+ checked)
