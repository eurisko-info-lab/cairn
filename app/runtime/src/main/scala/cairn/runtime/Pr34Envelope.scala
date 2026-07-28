package cairn.runtime

import cairn.kernel.*

/** Canonical input package for PR34's closed-world replay target.
  *
  * This is the frozen object `G` projected into canonical bytes.
  */
final case class Pr34GraphPackage(
    kernelConstitution: Digest,
    artifactClosure: Digest,
    machineClosure: Digest,
    runtimeClosure: Digest,
    acceptanceClosure: Digest,
    repositoryRoot: Digest,
    finalizedHistory: Digest,
    evidenceClosure: Digest,
):
  def canon: Canon = Canon.CTag("pr34-graph-package-v1", Canon.cmap(
    "kernelConstitution" -> Canon.CStr(kernelConstitution.hex),
    "artifactClosure" -> Canon.CStr(artifactClosure.hex),
    "machineClosure" -> Canon.CStr(machineClosure.hex),
    "runtimeClosure" -> Canon.CStr(runtimeClosure.hex),
    "acceptanceClosure" -> Canon.CStr(acceptanceClosure.hex),
    "repositoryRoot" -> Canon.CStr(repositoryRoot.hex),
    "finalizedHistory" -> Canon.CStr(finalizedHistory.hex),
    "evidenceClosure" -> Canon.CStr(evidenceClosure.hex),
  ))

  def digest: Digest = Digest.of(canon)

object Pr34GraphPackage:
  def fromCanon(c: Canon): Either[String, Pr34GraphPackage] =
    c match
      case Canon.CTag("pr34-graph-package-v1", body) =>
        try Right(Pr34GraphPackage(
          kernelConstitution = Digest(body.field("kernelConstitution").asStr),
          artifactClosure = Digest(body.field("artifactClosure").asStr),
          machineClosure = Digest(body.field("machineClosure").asStr),
          runtimeClosure = Digest(body.field("runtimeClosure").asStr),
          acceptanceClosure = Digest(body.field("acceptanceClosure").asStr),
          repositoryRoot = Digest(body.field("repositoryRoot").asStr),
          finalizedHistory = Digest(body.field("finalizedHistory").asStr),
          evidenceClosure = Digest(body.field("evidenceClosure").asStr),
        ))
        catch case e: Exception => Left(s"invalid pr34 graph package: ${e.getMessage}")
      case _ => Left("expected pr34-graph-package-v1 body")

enum Pr34VerdictClass:
  case Valid
  case Invalid
  case Missing
  case Exhausted

  def wire: String = this match
    case Valid => "valid"
    case Invalid => "invalid"
    case Missing => "missing"
    case Exhausted => "exhausted"

object Pr34VerdictClass:
  def parse(s: String): Either[String, Pr34VerdictClass] =
    s match
      case "valid" => Right(Pr34VerdictClass.Valid)
      case "invalid" => Right(Pr34VerdictClass.Invalid)
      case "missing" => Right(Pr34VerdictClass.Missing)
      case "exhausted" => Right(Pr34VerdictClass.Exhausted)
      case other => Left(s"invalid pr34 verdict class: $other")

final case class Pr34ResourceUse(
    steps: Long,
    bytesRead: Long,
    wallMicros: Long,
):
  def canon: Canon = Canon.cmap(
    "steps" -> Canon.CInt(steps),
    "bytesRead" -> Canon.CInt(bytesRead),
    "wallMicros" -> Canon.CInt(wallMicros),
  )

object Pr34ResourceUse:
  def fromCanon(c: Canon): Either[String, Pr34ResourceUse] =
    try Right(Pr34ResourceUse(
      steps = c.field("steps").asInt,
      bytesRead = c.field("bytesRead").asInt,
      wallMicros = c.field("wallMicros").asInt,
    ))
    catch case e: Exception => Left(s"invalid pr34 resource use: ${e.getMessage}")

/** Canonical verdict envelope for cross-implementation identity checks.
  *
  * `state` and `evidence` are optional to support non-valid verdict classes.
  */
final case class Pr34VerdictEnvelope(
    kernelConstitution: Digest,
    graphPackage: Digest,
    verdictClass: Pr34VerdictClass,
    state: Option[Digest],
    evidence: Option[Digest],
    resourceUse: Pr34ResourceUse,
):
  def canon: Canon = Canon.CTag("pr34-verdict-envelope-v1", Canon.cmap(
    "kernelConstitution" -> Canon.CStr(kernelConstitution.hex),
    "graphPackage" -> Canon.CStr(graphPackage.hex),
    "verdictClass" -> Canon.CStr(verdictClass.wire),
    "state" -> state.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "evidence" -> evidence.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "resourceUse" -> resourceUse.canon,
  ))

  def digest: Digest = Digest.of(canon)

object Pr34VerdictEnvelope:
  private def readOptionDigest(c: Canon): Either[String, Option[Digest]] =
    c match
      case Canon.CTag("none", _) => Right(None)
      case Canon.CTag("some", Canon.CStr(h)) => Right(Some(Digest(h)))
      case _ => Left("expected option digest")

  def fromCanon(c: Canon): Either[String, Pr34VerdictEnvelope] =
    c match
      case Canon.CTag("pr34-verdict-envelope-v1", body) =>
        for
          verdict <- Pr34VerdictClass.parse(body.field("verdictClass").asStr)
          state <- readOptionDigest(body.field("state"))
          evidence <- readOptionDigest(body.field("evidence"))
          resourceUse <- Pr34ResourceUse.fromCanon(body.field("resourceUse"))
        yield Pr34VerdictEnvelope(
          kernelConstitution = Digest(body.field("kernelConstitution").asStr),
          graphPackage = Digest(body.field("graphPackage").asStr),
          verdictClass = verdict,
          state = state,
          evidence = evidence,
          resourceUse = resourceUse,
        )
      case _ => Left("expected pr34-verdict-envelope-v1 body")
