package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{Ed25519, Signer}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}

/** CAS-backed distributed Studio workspaces. Mutable files are merely durable
  * aliases to immutable draft digests; a workspace can always be reopened,
  * handed off, or replicated using artifact identity alone. */
final class DurableWorkspaces(cas: Cas, refsDir: Path, branches: Branches):
  private def actor(signer: Signer): WorkspaceActor = WorkspaceActor(signer.name, signer.publicBytes)

  private def validAlias(alias: String): Either[String, Unit] = Either.cond(
    alias.nonEmpty && alias.forall(c => c.isLetterOrDigit || c == '-' || c == '_'), (),
    s"invalid workspace alias '$alias'")

  private def ref(alias: String): Path = refsDir.resolve(alias)

  private def writeRef(alias: String, digest: Digest): Either[String, Unit] =
    validAlias(alias).flatMap { _ =>
      try
        Files.createDirectories(refsDir)
        val temporary = Files.createTempFile(refsDir, s"$alias-", ".tmp")
        Files.writeString(temporary, digest.hex + "\n", UTF_8)
        try Files.move(temporary, ref(alias), StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch case _: java.nio.file.AtomicMoveNotSupportedException =>
          Files.move(temporary, ref(alias), StandardCopyOption.REPLACE_EXISTING)
        Right(())
      catch case e: Exception => Left(s"workspace ref write failed: ${e.getMessage}")
    }

  def resolve(alias: String): Either[String, Digest] = validAlias(alias).flatMap { _ =>
    try
      if !Files.isRegularFile(ref(alias)) then Left(s"workspace '$alias' does not exist")
      else Digest.parse(Files.readString(ref(alias), UTF_8).trim)
    catch case e: Exception => Left(s"workspace ref read failed: ${e.getMessage}")
  }

  def load(digest: Digest): Either[String, WorkspaceDraft] =
    cas.getByDigest(digest).flatMap(WorkspaceDraft.fromArtifact)

  def reopen(alias: String): Either[String, WorkspaceDraft] = resolve(alias).flatMap(load)

  /** Persist the complete staged proposal. This is safe while disconnected:
    * no branch mutation or network operation occurs. */
  def save(alias: String, session: StudioSession, signer: Signer): Either[String, WorkspaceDraft] =
    for
      proposal <- session.workspace.proposal.toRight("Studio workspace has no validated proposal")
      _ <- Either.cond(session.authority == signer.name, (), "workspace signer does not match Studio authority")
      previous <- resolve(alias).map(Some(_)).orElse(Right(None))
      _ = List(session.base.artifact, proposal.result.artifact, proposal.validatedChange.artifact,
        session.constitution.artifact).foreach(cas.put)
      draft = WorkspaceDraft(session.capabilities.language.digest, session.branch, session.base.digest,
        session.constitution.digest, actor(signer), proposal.change, proposal.result.digest,
        proposal.validatedChange.artifact.digest, previous)
      _ = cas.put(draft.artifact)
      _ <- writeRef(alias, draft.digest)
    yield draft

  def review(draftDigest: Digest, signer: Signer, decision: WorkspaceReviewDecision, note: String): Either[String, WorkspaceReview] =
    for
      draft <- load(draftDigest)
      unsigned = WorkspaceReview(draftDigest, actor(signer), decision, note, draft.result, Vector.empty)
      signed = unsigned.copy(signature = signer.sign(unsigned.signingBytes))
      _ = cas.put(signed.artifact)
    yield signed

  def approval(draftDigest: Digest, reviewDigest: Option[Digest], signer: Signer): Either[String, WorkspaceApproval] =
    for
      draft <- load(draftDigest)
      _ <- reviewDigest.fold[Either[String, Unit]](Right(())) { digest =>
        cas.getByDigest(digest).flatMap(WorkspaceReview.fromArtifact).flatMap { review =>
          Either.cond(review.draft == draftDigest && review.reviewedResult == draft.result &&
            review.decision == WorkspaceReviewDecision.RecommendApproval && verify(review), (),
            "workspace review does not validly recommend this draft")
        }
      }
      unsigned = WorkspaceApproval(draftDigest, reviewDigest, actor(signer), draft.constitution,
        draft.result, Vector.empty)
      signed = unsigned.copy(signature = signer.sign(unsigned.signingBytes))
      _ = cas.put(signed.artifact)
    yield signed

  def handoff(subject: Digest, from: Signer, to: WorkspaceActor): Either[String, WorkspaceHandoff] =
    cas.getByDigest(subject).flatMap { _ =>
      val unsigned = WorkspaceHandoff(subject, actor(from), to, Vector.empty)
      val signed = unsigned.copy(signature = from.sign(unsigned.signingBytes))
      cas.put(signed.artifact)
      Right(signed)
    }

  def verify(review: WorkspaceReview): Boolean =
    Ed25519.verify(review.reviewer.publicKey, review.signingBytes, review.signature)
  def verify(approval: WorkspaceApproval): Boolean =
    Ed25519.verify(approval.approver.publicKey, approval.signingBytes, approval.signature)
  def verify(handoff: WorkspaceHandoff): Boolean =
    Ed25519.verify(handoff.from.publicKey, handoff.signingBytes, handoff.signature)

  /** Replays the ordinary pending ΔL over the live head. A successful rebase
    * is a new immutable draft revision; the old draft remains available. */
  def rebase(
      alias: String, draftDigest: Digest, capabilities: ResolvedLanguageCapabilities,
      constitution: AcceptanceConstitution, signer: Signer,
  ): Either[String, WorkspaceDraft] =
    for
      draft <- load(draftDigest)
      _ <- Either.cond(draft.actor.name == signer.name, (), "only the draft actor may rebase it")
      _ <- Either.cond(draft.language == capabilities.language.digest, (), "workspace language bundle mismatch")
      _ <- Either.cond(draft.constitution == constitution.digest, (), "workspace constitution mismatch")
      live <- branches.headModule(draft.branch)
      applied <- Delta.apply(capabilities.language, live, draft.change, capabilities.changeModel)
      (result, vcs) = applied
      _ <- capabilities.moduleGate().check(result).left.map(_.toString)
      _ = List(result.artifact, vcs.artifact, constitution.artifact).foreach(cas.put)
      rebased = WorkspaceDraft(draft.language, draft.branch, live.digest, draft.constitution,
        draft.actor, draft.change, result.digest, vcs.artifact.digest, Some(draftDigest))
      _ = cas.put(rebased.artifact)
      _ <- writeRef(alias, rebased.digest)
    yield rebased

  /** Pull every artifact reachable from `root` into this workspace's CAS.
    * This works for removable-media/offline handoff as well as network CASes. */
  def replicateFrom(source: Cas, root: Digest): Either[String, List[Digest]] =
    def dependencies(a: Artifact): List[Digest] = a.kind match
      case ArtifactKind.WorkspaceDraft => WorkspaceDraft.fromArtifact(a).toOption.toList.flatMap { d =>
        List(d.base, d.constitution, d.result, d.validatedChange) ++ d.previous.toList }
      case ArtifactKind.WorkspaceReview => WorkspaceReview.fromArtifact(a).toOption.toList.flatMap(r => List(r.draft, r.reviewedResult))
      case ArtifactKind.WorkspaceApproval => WorkspaceApproval.fromArtifact(a).toOption.toList.flatMap(a =>
        List(a.draft, a.constitution, a.approvedResult) ++ a.review.toList)
      case ArtifactKind.WorkspaceHandoff => WorkspaceHandoff.fromArtifact(a).toOption.toList.map(_.subject)
      case _ => Nil
    def pull(todo: List[Digest], seen: Set[Digest]): Either[String, Set[Digest]] = todo match
      case Nil => Right(seen)
      case digest :: rest if seen.contains(digest) => pull(rest, seen)
      case digest :: rest =>
        for
          bytes <- source.getBytes(digest)
          _ <- Either.cond(Digest.ofBytes(bytes) == digest, (), s"replicated blob ${digest.short} failed digest verification")
          artifact <- Artifact.decode(bytes)
          _ = cas.putBytes(bytes)
          result <- pull(dependencies(artifact) ++ rest, seen + digest)
        yield result
    pull(List(root), Set.empty).map(_.toList.sortBy(_.hex))
