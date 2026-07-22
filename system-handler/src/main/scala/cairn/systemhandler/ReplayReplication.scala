package cairn.systemhandler

import cairn.kernel.{Artifact, ArtifactKind, Canon, Digest}
import cairn.systeminterface.Cas

/** Thin replication protocol for issuer-scoped [[ReplayStore]] snapshots plus
  * capability revocation — digest-merge sync, **not** consensus / BFT.
  *
  * Design:
  *   1. Nodes publish [[ReplayStore.Snapshot]] as CAS `replay-snapshot` digests
  *      ([[ReplayStore.publish]]).
  *   2. Peers absorb via [[ReplayStore.mergeFromCas]] (union of issuer-scoped
  *      nonce / requestId sets — merge, not agreement).
  *   3. Revocation is an append-only CAS `capability-revocation` artifact listing
  *      revoked grant ids; [[RevocationLog.absorbFromCas]] unions digests into a
  *      local set. Gates consult [[RevocationLog.isRevoked]] before accepting a
  *      capability proof ([[ReplayReplication.checkGrant]]).
  *
  * Deferred: BFT quorum, ordered broadcast, cross-issuer conflict resolution.
  * Studio UI is unrelated — this is CAS digest exchange only.
  */
object ReplayReplication:
  /** Want/have exchange: requester lists digests it already has; peer returns
    * missing snapshot digests (content transfer is ordinary CAS get/put).
    */
  final case class WantHave(
      haveSnapshots: Set[Digest],
      haveRevocations: Set[Digest],
  ):
    def missingSnapshots(available: Set[Digest]): Set[Digest] =
      available.diff(haveSnapshots)
    def missingRevocations(available: Set[Digest]): Set[Digest] =
      available.diff(haveRevocations)

  final case class SyncPlan(
      snapshotsToFetch: List[Digest],
      revocationsToFetch: List[Digest],
  )

  def plan(
      local: WantHave,
      peerSnapshots: Set[Digest],
      peerRevocations: Set[Digest]
  ): SyncPlan =
    SyncPlan(
      snapshotsToFetch = local.missingSnapshots(peerSnapshots).toList.sortBy(_.hex),
      revocationsToFetch = local.missingRevocations(peerRevocations).toList.sortBy(_.hex))

  /** Apply a sync plan: merge each snapshot; absorb each revocation artifact. */
  def applyPlan(
      store: ReplayStore,
      revocations: RevocationLog,
      cas: Cas,
      ctx: EffectContext,
      plan: SyncPlan
  ): Either[String, Unit] =
    for
      _ <- plan.snapshotsToFetch.foldLeft[Either[String, Unit]](Right(())) { (acc, d) =>
        acc.flatMap(_ => ReplayStore.mergeFromCas(store, cas, d, ctx))
      }
      _ <- plan.revocationsToFetch.foldLeft[Either[String, Unit]](Right(())) { (acc, d) =>
        acc.flatMap(_ => revocations.absorbFromCas(cas, d, ctx))
      }
    yield ()

  /** Gate helper: reject a grant id that appears in the local revocation set. */
  def checkGrant(revocations: RevocationLog, grantId: String): Either[String, Unit] =
    if revocations.isRevoked(grantId) then Left(s"grant revoked: $grantId")
    else Right(())

/** Append-only capability revocation log (thin). Grant ids are opaque strings
  * (capability proof digests or issuer+nonce keys). Stored as CAS artifacts
  * under tag `capability-revocation`; local set is the union of absorbed blobs.
  * Publish both mutates the local set and returns a CAS digest peers can want/have.
  */
final class RevocationLog:
  private val revoked = scala.collection.mutable.HashSet.empty[String]
  private val seen = scala.collection.mutable.HashSet.empty[Digest]

  def digests: Set[Digest] = seen.toSet
  def isRevoked(grantId: String): Boolean = revoked.contains(grantId)
  def snapshot: Set[String] = revoked.toSet

  def revoke(grantId: String): Unit = revoked += grantId

  def artifact(grantIds: Iterable[String]): Artifact =
    Artifact(
      ArtifactKind.Certificate,
      Canon.CTag(
        "capability-revocation",
        Canon.cmap("grants" -> Canon.cstrs(grantIds.toList.sorted))))

  def publish(cas: Cas, ctx: EffectContext, grantIds: Iterable[String]): Either[String, Digest] =
    val ids = grantIds.toList
    ids.foreach(revoke)
    CasEffects.put(cas, artifact(ids), ctx).left.map(_.toString).map { key =>
      seen += key.valueHash
      key.valueHash
    }

  def absorbFromCas(cas: Cas, digest: Digest, ctx: EffectContext): Either[String, Unit] =
    CasEffects.get(cas, digest, ctx).left.map(_.toString).flatMap { a =>
      a.body match
        case Canon.CTag("capability-revocation", body) =>
          try
            val grants = body.field("grants").asList.map(_.asStr)
            grants.foreach(revoke)
            seen += digest
            Right(())
          catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString))
        case _ => Left("capability-revocation: missing tag")
    }

/** Append-only capability-delegation log (thin explorer / sync surface).
  * Each entry is a hop summary published as CAS `capability-delegation`
  * certificates. Callers that hold real [[Authority.Delegation]] chains should
  * validate them first; this log is the digest-merge / UI ledger — not BFT.
  * Studio deferred.
  */
object DelegationLog:
  final case class Entry(
      grantor: String,
      grantee: String,
      action: String,
      resourceKind: String,
      resourcePath: String,
      depth: Int,
      digest: Option[Digest] = None,
  )

final class DelegationLog:
  import DelegationLog.Entry

  private val entries = scala.collection.mutable.ArrayBuffer.empty[Entry]
  private val seen = scala.collection.mutable.HashSet.empty[Digest]

  def digests: Set[Digest] = seen.toSet
  def snapshot: List[Entry] = entries.toList

  def record(entry: Entry): Unit = entries += entry

  def fromDelegation(d: cairn.kernel.Authority.Delegation): Either[String, Entry] =
    cairn.kernel.Authority.Delegation.validate(d).map { _ =>
      Entry(
        grantor = d.grantor.id,
        grantee = d.grantee.id,
        action = d.child.action.id,
        resourceKind = d.child.resource.kind,
        resourcePath = d.child.resource.path,
        depth = d.child.delegationDepth)
    }

  def artifact(entry: Entry): Artifact =
    Artifact(
      ArtifactKind.Certificate,
      Canon.CTag(
        "capability-delegation",
        Canon.cmap(
          "grantor" -> Canon.CStr(entry.grantor),
          "grantee" -> Canon.CStr(entry.grantee),
          "action" -> Canon.CStr(entry.action),
          "resourceKind" -> Canon.CStr(entry.resourceKind),
          "resourcePath" -> Canon.CStr(entry.resourcePath),
          "depth" -> Canon.CInt(entry.depth.toLong))))

  /** Publish a hop summary (already validated or UI-authored). */
  def publish(cas: Cas, ctx: EffectContext, entry: Entry): Either[String, Digest] =
    CasEffects.put(cas, artifact(entry), ctx).left.map(_.toString).map { key =>
      seen += key.valueHash
      record(entry.copy(digest = Some(key.valueHash)))
      key.valueHash
    }

  def publishDelegation(
      cas: Cas,
      ctx: EffectContext,
      d: cairn.kernel.Authority.Delegation
  ): Either[String, Digest] =
    fromDelegation(d).flatMap(publish(cas, ctx, _))

  def absorbFromCas(cas: Cas, digest: Digest, ctx: EffectContext): Either[String, Unit] =
    CasEffects.get(cas, digest, ctx).left.map(_.toString).flatMap { a =>
      a.body match
        case Canon.CTag("capability-delegation", body) =>
          try
            val e = Entry(
              grantor = body.field("grantor").asStr,
              grantee = body.field("grantee").asStr,
              action = body.field("action").asStr,
              resourceKind = body.field("resourceKind").asStr,
              resourcePath = body.field("resourcePath").asStr,
              depth = body.field("depth").asInt.toInt,
              digest = Some(digest))
            record(e)
            seen += digest
            Right(())
          catch case ex: Exception => Left(Option(ex.getMessage).getOrElse(ex.toString))
        case _ => Left("capability-delegation: missing tag")
    }
