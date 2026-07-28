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

  def artifact: Artifact = Artifact(ArtifactKind.Trace, canon)
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

  def fromArtifact(a: Artifact): Either[String, Pr34GraphPackage] =
    if a.kind != ArtifactKind.Trace then Left("expected trace artifact for pr34 graph package")
    else fromCanon(a.body)

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

  def artifact: Artifact = Artifact(ArtifactKind.Trace, canon)
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

  def fromArtifact(a: Artifact): Either[String, Pr34VerdictEnvelope] =
    if a.kind != ArtifactKind.Trace then Left("expected trace artifact for pr34 verdict envelope")
    else fromCanon(a.body)

object Pr34EnvelopeInterop:
  private def verdictClassOf(result: CKC.KernelResult): Pr34VerdictClass =
    result match
      case CKC.KernelResult.Valid(_, _) => Pr34VerdictClass.Valid
      case CKC.KernelResult.Invalid(_) => Pr34VerdictClass.Invalid
      case CKC.KernelResult.Missing(_) => Pr34VerdictClass.Missing
      case CKC.KernelResult.Exhausted(_) => Pr34VerdictClass.Exhausted

  private def stateOf(result: CKC.KernelResult): Option[Digest] =
    result match
      case CKC.KernelResult.Valid(CKC.Value.ReplayedState(report), _) => Some(report.finalState)
      case _ => None

  private def evidenceOf(result: CKC.KernelResult): Option[Digest] =
    result match
      case CKC.KernelResult.Valid(_, evidence) => Some(evidence)
      case _ => None

  /** Bridge current CKC output into the canonical PR34 verdict envelope.
    *
    * `graphPackage` must be the digest of `Pr34GraphPackage.canon`.
    */
  def fromCkc(
      constitution: CKC.KernelConstitution,
      graphPackage: Digest,
      result: CKC.KernelResult,
      resourceUse: Pr34ResourceUse,
  ): Pr34VerdictEnvelope =
    Pr34VerdictEnvelope(
      kernelConstitution = Digest.of(Canon.CStr(constitution.kernelId)),
      graphPackage = graphPackage,
      verdictClass = verdictClassOf(result),
      state = stateOf(result),
      evidence = evidenceOf(result),
      resourceUse = resourceUse,
    )

/** Minimal staircase link: one constituted upgrade from G0 to G1.
  *
  * This is a scaffold object for the first load-bearing successor step.
  */
final case class Pr34SuccessorLink(
    predecessorPackage: Digest,
    successorPackage: Digest,
    upgradeDelta: Digest,
):
  def canon: Canon = Canon.CTag("pr34-successor-link-v1", Canon.cmap(
    "predecessorPackage" -> Canon.CStr(predecessorPackage.hex),
    "successorPackage" -> Canon.CStr(successorPackage.hex),
    "upgradeDelta" -> Canon.CStr(upgradeDelta.hex),
  ))

  def artifact: Artifact = Artifact(ArtifactKind.Trace, canon)
  def digest: Digest = Digest.of(canon)

object Pr34SuccessorLink:
  def fromCanon(c: Canon): Either[String, Pr34SuccessorLink] =
    c match
      case Canon.CTag("pr34-successor-link-v1", body) =>
        try Right(Pr34SuccessorLink(
          predecessorPackage = Digest(body.field("predecessorPackage").asStr),
          successorPackage = Digest(body.field("successorPackage").asStr),
          upgradeDelta = Digest(body.field("upgradeDelta").asStr),
        ))
        catch case e: Exception => Left(s"invalid pr34 successor link: ${e.getMessage}")
      case _ => Left("expected pr34-successor-link-v1 body")

  def fromArtifact(a: Artifact): Either[String, Pr34SuccessorLink] =
    if a.kind != ArtifactKind.Trace then Left("expected trace artifact for pr34 successor link")
    else fromCanon(a.body)

object Pr34Staircase:
  /** Validate the first load-bearing stair over two independently reconstructed worlds.
    *
    * This checks structure only; deeper semantic checks are added in later slices.
    */
  def validateTwoStep(
      g0: Pr34VerdictEnvelope,
      g1: Pr34VerdictEnvelope,
      link: Pr34SuccessorLink,
  ): Either[String, Unit] =
    if g0.verdictClass != Pr34VerdictClass.Valid then
      Left("g0 verdict is not valid")
    else if g1.verdictClass != Pr34VerdictClass.Valid then
      Left("g1 verdict is not valid")
    else if g0.graphPackage != link.predecessorPackage then
      Left("g0 package does not match successor link predecessor")
    else if g1.graphPackage != link.successorPackage then
      Left("g1 package does not match successor link successor")
    else if g0.state.isEmpty then
      Left("g0 valid verdict is missing state")
    else if g1.state.isEmpty then
      Left("g1 valid verdict is missing state")
    else if g0.evidence.isEmpty then
      Left("g0 valid verdict is missing evidence")
    else if g1.evidence.isEmpty then
      Left("g1 valid verdict is missing evidence")
    else if link.predecessorPackage == link.successorPackage then
      Left("successor package must differ from predecessor package")
    else Right(())
