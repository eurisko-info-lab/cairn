package cairn.core

import cairn.kernel.*

/** Durable identity of an actor. Private key material is deliberately absent;
  * signatures are verified by the runtime crypto boundary. */
final case class WorkspaceActor(name: String, publicKey: Vector[Byte]):
  require(name.nonEmpty, "workspace actor name is required")
  def canon: Canon = Canon.cmap("name" -> Canon.CStr(name), "publicKey" -> Canon.CBytes(publicKey))
  def digest: Digest = Digest.of(canon)

object WorkspaceActor:
  def fromCanon(c: Canon): Either[String, WorkspaceActor] =
    try c.field("publicKey") match
      case Canon.CBytes(bytes) => Right(WorkspaceActor(c.field("name").asStr, bytes))
      case _ => Left("invalid workspace actor public key")
    catch case e: Exception => Left(s"invalid workspace actor: ${e.getMessage}")

/** An offline-capable Studio proposal. `change` is ordinary ΔL; the branch is
  * only a publication target and never the draft's identity. `previous`
  * creates an immutable revision chain suitable for restart and replication. */
final case class WorkspaceDraft(
    language: Digest,
    branch: String,
    base: Digest,
    constitution: Digest,
    actor: WorkspaceActor,
    change: Cst,
    result: Digest,
    validatedChange: Digest,
    previous: Option[Digest] = None,
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex), "branch" -> Canon.CStr(branch),
    "base" -> Canon.CStr(base.hex), "constitution" -> Canon.CStr(constitution.hex),
    "actor" -> actor.canon, "change" -> Cst.toCanon(change),
    "result" -> Canon.CStr(result.hex), "validatedChange" -> Canon.CStr(validatedChange.hex),
    "previous" -> previous.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))))
  def artifact: Artifact = Artifact(ArtifactKind.WorkspaceDraft, canon)
  def digest: Digest = artifact.digest

object WorkspaceDraft:
  def fromArtifact(a: Artifact): Either[String, WorkspaceDraft] =
    if a.kind != ArtifactKind.WorkspaceDraft then Left("artifact is not a workspace draft")
    else try Right(WorkspaceDraft(
      Digest(a.body.field("language").asStr), a.body.field("branch").asStr,
      Digest(a.body.field("base").asStr), Digest(a.body.field("constitution").asStr),
      WorkspaceActor.fromCanon(a.body.field("actor")).fold(e => throw CodecError(e), identity),
      Cst.fromCanon(a.body.field("change")), Digest(a.body.field("result").asStr),
      Digest(a.body.field("validatedChange").asStr), optionalDigest(a.body.field("previous"))))
    catch case e: Exception => Left(s"invalid workspace draft: ${e.getMessage}")

  private[core] def optionalDigest(c: Canon): Option[Digest] = c match
    case Canon.CTag("some", Canon.CStr(value)) => Some(Digest(value))
    case _ => None

enum WorkspaceReviewDecision:
  case RequestChanges, RecommendApproval

final case class WorkspaceReview(
    draft: Digest, reviewer: WorkspaceActor, decision: WorkspaceReviewDecision,
    note: String, reviewedResult: Digest, signature: Vector[Byte],
):
  def unsignedCanon: Canon = Canon.cmap(
    "domain" -> Canon.CStr("cairn-workspace-review-v1"),
    "draft" -> Canon.CStr(draft.hex), "reviewer" -> reviewer.canon,
    "decision" -> Canon.CStr(decision.toString), "note" -> Canon.CStr(note),
    "reviewedResult" -> Canon.CStr(reviewedResult.hex))
  def signingBytes: Array[Byte] = Canon.encode(unsignedCanon)
  def canon: Canon = Canon.cmap("unsigned" -> unsignedCanon, "signature" -> Canon.CBytes(signature))
  def artifact: Artifact = Artifact(ArtifactKind.WorkspaceReview, canon)

object WorkspaceReview:
  def fromArtifact(a: Artifact): Either[String, WorkspaceReview] =
    if a.kind != ArtifactKind.WorkspaceReview then Left("artifact is not a workspace review")
    else try
      val u = a.body.field("unsigned")
      if u.field("domain").asStr != "cairn-workspace-review-v1" then Left("wrong workspace review domain")
      else Right(WorkspaceReview(Digest(u.field("draft").asStr),
      WorkspaceActor.fromCanon(u.field("reviewer")).fold(e => throw CodecError(e), identity),
      WorkspaceReviewDecision.values.find(_.toString == u.field("decision").asStr)
        .getOrElse(throw CodecError("unknown review decision")), u.field("note").asStr,
      Digest(u.field("reviewedResult").asStr), bytes(a.body.field("signature"))))
    catch case e: Exception => Left(s"invalid workspace review: ${e.getMessage}")

final case class WorkspaceApproval(
    draft: Digest, review: Option[Digest], approver: WorkspaceActor,
    constitution: Digest, approvedResult: Digest, signature: Vector[Byte],
):
  def unsignedCanon: Canon = Canon.cmap(
    "domain" -> Canon.CStr("cairn-workspace-approval-v1"),
    "draft" -> Canon.CStr(draft.hex),
    "review" -> review.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "approver" -> approver.canon, "constitution" -> Canon.CStr(constitution.hex),
    "approvedResult" -> Canon.CStr(approvedResult.hex))
  def signingBytes: Array[Byte] = Canon.encode(unsignedCanon)
  def canon: Canon = Canon.cmap("unsigned" -> unsignedCanon, "signature" -> Canon.CBytes(signature))
  def artifact: Artifact = Artifact(ArtifactKind.WorkspaceApproval, canon)

object WorkspaceApproval:
  def fromArtifact(a: Artifact): Either[String, WorkspaceApproval] =
    if a.kind != ArtifactKind.WorkspaceApproval then Left("artifact is not a workspace approval")
    else try
      val u = a.body.field("unsigned")
      if u.field("domain").asStr != "cairn-workspace-approval-v1" then Left("wrong workspace approval domain")
      else Right(WorkspaceApproval(Digest(u.field("draft").asStr),
      WorkspaceDraft.optionalDigest(u.field("review")),
      WorkspaceActor.fromCanon(u.field("approver")).fold(e => throw CodecError(e), identity),
      Digest(u.field("constitution").asStr), Digest(u.field("approvedResult").asStr), bytes(a.body.field("signature"))))
    catch case e: Exception => Left(s"invalid workspace approval: ${e.getMessage}")

/** Domain-separated signature over a durable workspace artifact. */
final case class WorkspaceHandoff(subject: Digest, from: WorkspaceActor, to: WorkspaceActor, signature: Vector[Byte]):
  def unsignedCanon: Canon = Canon.cmap(
    "domain" -> Canon.CStr("cairn-workspace-handoff-v1"), "subject" -> Canon.CStr(subject.hex),
    "from" -> from.canon, "to" -> to.canon)
  def signingBytes: Array[Byte] = Canon.encode(unsignedCanon)
  def canon: Canon = Canon.cmap("unsigned" -> unsignedCanon, "signature" -> Canon.CBytes(signature))
  def artifact: Artifact = Artifact(ArtifactKind.WorkspaceHandoff, canon)

object WorkspaceHandoff:
  def fromArtifact(a: Artifact): Either[String, WorkspaceHandoff] =
    if a.kind != ArtifactKind.WorkspaceHandoff then Left("artifact is not a workspace handoff")
    else try
      val u = a.body.field("unsigned")
      if u.field("domain").asStr != "cairn-workspace-handoff-v1" then Left("wrong workspace handoff domain")
      else for
        from <- WorkspaceActor.fromCanon(u.field("from"))
        to <- WorkspaceActor.fromCanon(u.field("to"))
        signature <- a.body.field("signature") match
          case Canon.CBytes(bytes) => Right(bytes)
          case _ => Left("invalid workspace handoff signature")
      yield WorkspaceHandoff(Digest(u.field("subject").asStr), from, to, signature)
    catch case e: Exception => Left(s"invalid workspace handoff: ${e.getMessage}")

private def bytes(c: Canon): Vector[Byte] = c match
  case Canon.CBytes(value) => value
  case _ => throw CodecError("expected bytes")
