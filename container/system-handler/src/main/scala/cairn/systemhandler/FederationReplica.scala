package cairn.systemhandler

import cairn.kernel.*
import java.nio.file.{Files, Path}

/** PR33: per-process federation-state BFT replica, the network-capable
  * analogue of [[BftReplica]] for [[FederationFinality.FederationProposal]]
  * agreement rather than block finality. Deliberately a PARALLEL
  * implementation, not a generalization of `BftReplica` in place — see
  * [[FederationFinality]]'s own doc comment for why: `BftReplica`'s one
  * non-quorum-arithmetic check (`requireSealedBlock`) is natively a `Node`/
  * `Block` concept; this replica's equivalent check is inherently
  * `content/core`/`app/runtime` shaped (`FederationState`/
  * `FederationTransition`/`ArtifactApplicationResolver`), bridged here via
  * an injected callback (see [[FederationReplica.VerifyProposal]]) because
  * `container/system-handler` cannot depend on those modules — the same
  * constraint `FederationFinality` itself already lives under.
  *
  * Reuses [[BftQuorum]]'s digest-generic quorum math (`deliver`/
  * `deliverViewChange`/`deliverNewView`) and [[BftFinality]]'s signing/
  * `SignedMsg` conventions directly — no duplication of protocol math or
  * signature handling, only new persistence/verification orchestration
  * around them.
  */
object FederationReplica:
  import BftQuorum.*

  /** Rate limit on how often THIS replica will *start* a view-change,
    * mirroring [[BftReplica.MinViewChangeIntervalMs]]. */
  val MinViewChangeIntervalMs: Long = 250L

  /** Deliberately larger than [[BftReplica.PrimaryTimeoutMs]] (300ms): every
    * honest replica pays the cost of PR30's full deep re-certification
    * (ΔL replay, access-trace recomputation, acceptance-constitution rerun)
    * before it may vote, real CPU work a block-finality replica never pays.
    * Sized so a busy-but-healthy primary is never mistaken for an
    * unreachable one at the exit ceremony's test scale.
    */
  val FederationPrimaryTimeoutMs: Long = 2000L

  /** Outcome of the LOCAL, network-free proposal check a replica runs
    * before ever entering the quorum protocol for a `PrePrepare` — mirrors
    * `BftReplica`'s `bind`-before-`deliver` structure. `MissingClosure` is
    * deliberately not a bare error string: the HTTP transport layer (not
    * this class — see PR33 plan §2) pattern-matches on it to fetch exactly
    * those digests from the proposer and retry once. `receive` itself
    * never performs network I/O.
    */
  enum VerifyOutcome:
    case Verified
    case MissingClosure(digests: Set[Digest])
    case Rejected(reason: String)

  /** PR33.1 slice 2: durable record of the last generation THIS replica has
    * itself finalized (either by helping mint the certificate, or by
    * adopting one late — see `adoptCertificate`). A replica votes for a
    * proposal only when it genuinely extends this cursor (`proposal.before
    * == cursor.state && proposal.epoch > cursor.epoch`) — otherwise it is a
    * fork relative to what this replica already knows was finalized, and
    * must be refused before publication rather than merely caught later by
    * history replay. `transition` is carried for callers that need it
    * (e.g. a future audit trail); only `state`/`epoch` participate in the
    * extension check itself. Persisted fail-closed alongside `state` (not
    * best-effort like `NamespaceCertCache`): silently reverting to an
    * earlier cursor after a lost/corrupted read would let this replica
    * vote for a proposal that forks from a generation it already finalized.
    */
  final case class FinalizedFederationCursor(state: Digest, transition: Option[Digest], epoch: Long):
    def extendedBy(proposal: FederationFinality.FederationProposal): Boolean =
      proposal.before == state && proposal.epoch > epoch

    /** Never regresses: `None` if `cert` doesn't itself represent forward
      * progress from this cursor — the caller (`mintCertificateIfReady`/
      * `adoptCertificate`) simply keeps the existing cursor in that case.
      */
    def advancedBy(cert: FederationFinality.FederationFinalityCertificate): Option[FinalizedFederationCursor] =
      Option.when(cert.epoch > epoch)(FinalizedFederationCursor(cert.stateDigest, Some(cert.transition), cert.epoch))

    def canon: Canon = Canon.CTag("federation-finalized-cursor", Canon.cmap(
      "state" -> Canon.CStr(state.hex),
      "transition" -> transition.fold(Canon.CTag("none", Canon.CInt(0)))(t => Canon.CTag("some", Canon.CStr(t.hex))),
      "epoch" -> Canon.CInt(epoch)))

  object FinalizedFederationCursor:
    def genesis(state: Digest): FinalizedFederationCursor = FinalizedFederationCursor(state, None, 0L)
    def fromCanon(c: Canon): Either[String, FinalizedFederationCursor] =
      c match
        case Canon.CTag("federation-finalized-cursor", m) =>
          try
            val transition = m.field("transition") match
              case Canon.CTag("some", Canon.CStr(hex)) => Some(Digest(hex))
              case _                                   => None
            Right(FinalizedFederationCursor(Digest(m.field("state").asStr), transition, m.field("epoch").asInt))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not a federation-finalized-cursor: $other")

  /** PR33.1 slice 3: what [[FederationReplica.adoptCertificate]] returns on
    * success — the generation this replica now considers locally finalized,
    * independent of whether it ever cast a vote for it.
    */
  final case class AdoptedGeneration(state: Digest, transition: Digest, epoch: Long)

  /** PR33 slice 7: durable record of which namespaces this replica has
    * independently deep-certified, and whether it has ever completed the
    * join-time bootstrap (certifying every namespace live at the time,
    * not just the ones a later round's commits happen to touch — see
    * `FederationReplicaVerification.verifyWithCache`'s doc comment for why
    * a newly-joined replica needs this and a continuously-running one
    * doesn't). Never itself a correctness input: the verify callback
    * always independently re-derives everything from CAS content, so a
    * lost, corrupted, or simply absent cache only ever costs redundant
    * re-certification work, never a wrongly-accepted vote — restore/persist
    * failures here are deliberately non-fatal, unlike `state`/`certStore`.
    */
  final case class NamespaceCertCache(bootstrapped: Boolean, certified: Map[String, Digest]):
    def canon: Canon = Canon.CTag("federation-ns-cert-cache", Canon.cmap(
      "bootstrapped" -> Canon.CInt(if bootstrapped then 1 else 0),
      "certified" -> Canon.CList(certified.toList.sortBy(_._1).map { (ns, d) =>
        Canon.cmap("namespace" -> Canon.CStr(ns), "digest" -> Canon.CStr(d.hex))
      })))

  object NamespaceCertCache:
    val empty: NamespaceCertCache = NamespaceCertCache(false, Map.empty)
    def fromCanon(c: Canon): Either[String, NamespaceCertCache] =
      c match
        case Canon.CTag("federation-ns-cert-cache", m) =>
          try
            val bootstrapped = m.field("bootstrapped").asInt != 0
            val certified = m.field("certified").asList.map { row =>
              row.field("namespace").asStr -> Digest(row.field("digest").asStr)
            }.toMap
            Right(NamespaceCertCache(bootstrapped, certified))
          catch case e: CodecError => Left(e.getMessage)
        case other => Left(s"not a federation-ns-cert-cache: $other")

  /** Local-CAS-only proposal check, injected at construction time. The real
    * implementation (wrapping `VerifiedFederationTransition.verify` plus
    * per-changed-namespace deep re-certification) lives in `app/runtime`
    * (`FederationReplicaVerification`, PR33 slice 4) — this seam exists
    * because system-handler cannot depend on content/core, not because the
    * check is conceptually generic. Threads the namespace-certification
    * cache through functionally (in, updated-out) rather than via mutable
    * shared state, since the callback itself lives in a different module
    * than the durable store that persists it.
    */
  type VerifyProposal = (ReplicaId, FederationFinality.FederationProposal, NamespaceCertCache) => (VerifyOutcome, NamespaceCertCache)

  private val missingClosurePrefix = "federation: missing closure "

  /** Whether a `receive`/`propose` rejection was `MissingClosure`, and if so
    * which digests — the HTTP layer's fetch-then-retry hook (slice 6).
    */
  def missingClosureDigests(error: String): Option[Set[Digest]] =
    if error.startsWith(missingClosurePrefix) then
      Some(error.stripPrefix(missingClosurePrefix).split(",").filter(_.nonEmpty).map(Digest(_)).toSet)
    else None

  // --- Durable-state canon codecs. PreparedCert/ViewChangeEvidence codecs
  // are private to BftFinality (not package-private), so equivalents are
  // re-derived here rather than shared — same "parallel, not shared"
  // precedent as the rest of this file. ---

  private def preparedCanon(ps: List[PreparedCert]): Canon =
    Canon.CList(ps.map { p =>
      Canon.cmap(
        "seq" -> Canon.CInt(p.seq),
        "digest" -> Canon.CStr(p.valueDigest.hex),
        "preparedView" -> Canon.CInt(p.preparedView),
        "value" -> p.value.fold(Canon.CTag("none", Canon.CInt(0)))(v =>
          Canon.CTag("some", Canon.CBytes(v.bytes))),
        "prepareVotes" -> Canon.CList(p.prepareVotes.map { (rid, seal) =>
          Canon.cmap("from" -> Canon.CStr(rid.id), "seal" -> Canon.CBytes(seal))
        }),
        "prePrepareSeal" -> p.prePrepareSeal.fold(Canon.CTag("none", Canon.CInt(0)))(s =>
          Canon.CTag("some", Canon.CBytes(s))),
        "prePrepareFrom" -> p.prePrepareFrom.fold(Canon.CTag("none", Canon.CInt(0)))(r =>
          Canon.CTag("some", Canon.CStr(r.id))))
    })

  private def parsePrepared(c: Canon): List[PreparedCert] =
    import Canon.*
    c.asList.map { row =>
      val fields = row.asMap
      val value = fields.get("value") match
        case Some(CTag("some", CBytes(bs))) => Some(Value(bs))
        case _                              => None
      val votes = fields.get("prepareVotes") match
        case Some(CList(xs)) =>
          xs.map { r =>
            ReplicaId(r.field("from").asStr) -> (r.field("seal") match
              case CBytes(bs) => bs
              case _ => throw CodecError("prepare vote seal"))
          }
        case _ => Nil
      val ppSeal = fields.get("prePrepareSeal") match
        case Some(CTag("some", CBytes(bs))) => Some(bs)
        case _                              => None
      val ppFrom = fields.get("prePrepareFrom") match
        case Some(CTag("some", CStr(id))) => Some(ReplicaId(id))
        case _                            => None
      PreparedCert(
        row.field("seq").asInt.toInt, Digest(row.field("digest").asStr),
        row.field("preparedView").asInt.toInt, value, votes, ppSeal, ppFrom)
    }

  private def viewChangeEvidenceCanon(ev: ViewChangeEvidence): Canon =
    Canon.cmap(
      "vc" -> BftFinality.msgCanon(ev.vc), "seal" -> Canon.CBytes(ev.seal),
      "replicaSet" -> Canon.CStr(ev.replicaSet.hex), "federationId" -> Canon.CStr(ev.chainId.hex))

  private def parseViewChangeEvidence(c: Canon): ViewChangeEvidence =
    import Canon.*
    BftFinality.parseMsg(c.field("vc")) match
      case Right(vc: Msg.ViewChange) =>
        val seal = c.field("seal") match
          case CBytes(bs) => bs
          case _ => throw CodecError("view-change evidence seal")
        ViewChangeEvidence(vc, seal, Digest(c.field("replicaSet").asStr), Digest(c.field("federationId").asStr))
      case Right(_) => throw CodecError("view-change evidence must wrap ViewChange")
      case Left(e) => throw CodecError(e)

  private def encodeState(
      state: ReplicaState, replicaSet: Digest, federationId: Digest,
      commitSeals: Map[(Int, Int, String, String), Vector[Byte]],
      prepareSeals: Map[(Int, Int, String, String), Vector[Byte]],
      prePrepareSeals: Map[(Int, Int, String), (ReplicaId, Vector[Byte], Value)],
      viewChangeEvidence: List[ViewChangeEvidence],
      cursor: FinalizedFederationCursor,
  ): Canon =
    val slots = state.slots.toList.map { case ((view, seq), slot) =>
      Canon.cmap(
        "view" -> Canon.CInt(view), "seq" -> Canon.CInt(seq),
        "prePrepare" -> slot.prePrepare.fold(Canon.CTag("none", Canon.CInt(0)))(pp =>
          Canon.CTag("some", BftFinality.msgCanon(pp))),
        "prepares" -> Canon.CList(slot.prepares.toList.map { (id, d) =>
          Canon.cmap("from" -> Canon.CStr(id.id), "digest" -> Canon.CStr(d.hex)) }),
        "commits" -> Canon.CList(slot.commits.toList.map { (id, d) =>
          Canon.cmap("from" -> Canon.CStr(id.id), "digest" -> Canon.CStr(d.hex)) }),
        "decided" -> slot.decided.fold(Canon.CTag("none", Canon.CInt(0)))(v =>
          Canon.CTag("some", Canon.CBytes(v.bytes))))
    }
    Canon.CTag("federation-replica-state", Canon.cmap(
      "id" -> Canon.CStr(state.id.id), "n" -> Canon.CInt(state.n), "view" -> Canon.CInt(state.view),
      "replicaSet" -> Canon.CStr(replicaSet.hex), "federationId" -> Canon.CStr(federationId.hex),
      "slots" -> Canon.CList(slots),
      "commitSeals" -> Canon.CList(commitSeals.toList.map { case ((v, s, hex, rid), seal) =>
        Canon.cmap("view" -> Canon.CInt(v), "seq" -> Canon.CInt(s), "digest" -> Canon.CStr(hex),
          "replica" -> Canon.CStr(rid), "seal" -> Canon.CBytes(seal)) }),
      "prepareSeals" -> Canon.CList(prepareSeals.toList.map { case ((v, s, hex, rid), seal) =>
        Canon.cmap("view" -> Canon.CInt(v), "seq" -> Canon.CInt(s), "digest" -> Canon.CStr(hex),
          "replica" -> Canon.CStr(rid), "seal" -> Canon.CBytes(seal)) }),
      "prePrepareSeals" -> Canon.CList(prePrepareSeals.toList.map { case ((v, s, hex), (rid, seal, value)) =>
        Canon.cmap("view" -> Canon.CInt(v), "seq" -> Canon.CInt(s), "digest" -> Canon.CStr(hex),
          "replica" -> Canon.CStr(rid.id), "seal" -> Canon.CBytes(seal), "value" -> Canon.CBytes(value.bytes)) }),
      "viewChangeEvidence" -> Canon.CList(viewChangeEvidence.map(viewChangeEvidenceCanon)),
      "locks" -> Canon.CList(state.locks.toList.map { case (seq, lock) =>
        Canon.cmap("seq" -> Canon.CInt(seq), "preparedView" -> Canon.CInt(lock.preparedView),
          "digest" -> Canon.CStr(lock.valueDigest.hex),
          "value" -> lock.value.fold(Canon.CTag("none", Canon.CInt(0)))(v => Canon.CTag("some", Canon.CBytes(v.bytes))),
          "proof" -> preparedCanon(lock.proof.toList)) }),
      "cursor" -> cursor.canon))

  private final case class DecodedState(
      state: ReplicaState, replicaSet: Digest, federationId: Digest,
      commitSeals: Map[(Int, Int, String, String), Vector[Byte]],
      prepareSeals: Map[(Int, Int, String, String), Vector[Byte]],
      prePrepareSeals: Map[(Int, Int, String), (ReplicaId, Vector[Byte], Value)],
      viewChangeEvidence: List[ViewChangeEvidence],
      cursor: FinalizedFederationCursor,
  )

  private def decodeState(c: Canon): Either[String, DecodedState] =
    import Canon.*
    c match
      case CTag("federation-replica-state", m) =>
        try
          val fields = m.asMap
          val id = ReplicaId(m.field("id").asStr)
          val n = m.field("n").asInt.toInt
          val view = fields.get("view").map(_.asInt.toInt).getOrElse(0)
          val replicaSet = Digest(m.field("replicaSet").asStr)
          val federationId = Digest(m.field("federationId").asStr)
          val slots = m.field("slots").asList.map { row =>
            val v = row.field("view").asInt.toInt
            val s = row.field("seq").asInt.toInt
            val pp = row.field("prePrepare") match
              case CTag("some", body) => BftFinality.parseMsg(body).toOption.collect { case p: Msg.PrePrepare => p }
              case _ => None
            val prepares = row.field("prepares").asList.map { r =>
              ReplicaId(r.field("from").asStr) -> Digest(r.field("digest").asStr) }.toMap
            val commits = row.field("commits").asList.map { r =>
              ReplicaId(r.field("from").asStr) -> Digest(r.field("digest").asStr) }.toMap
            val decided = row.field("decided") match
              case CTag("some", CBytes(bs)) => Some(Value(bs))
              case _ => None
            (v, s) -> Slot(pp, prepares, commits, decided)
          }.toMap
          val commitSeals = m.field("commitSeals").asList.map { row =>
            (row.field("view").asInt.toInt, row.field("seq").asInt.toInt, row.field("digest").asStr, row.field("replica").asStr) ->
              (row.field("seal") match { case CBytes(bs) => bs; case _ => throw CodecError("commit seal") })
          }.toMap
          val prepareSeals = m.field("prepareSeals").asList.map { row =>
            (row.field("view").asInt.toInt, row.field("seq").asInt.toInt, row.field("digest").asStr, row.field("replica").asStr) ->
              (row.field("seal") match { case CBytes(bs) => bs; case _ => throw CodecError("prepare seal") })
          }.toMap
          val prePrepareSeals = m.field("prePrepareSeals").asList.map { row =>
            (row.field("view").asInt.toInt, row.field("seq").asInt.toInt, row.field("digest").asStr) ->
              (ReplicaId(row.field("replica").asStr),
                (row.field("seal") match { case CBytes(bs) => bs; case _ => throw CodecError("pre-prepare seal") }),
                (row.field("value") match { case CBytes(bs) => Value(bs); case _ => throw CodecError("pre-prepare value") }))
          }.toMap
          val evidence = m.field("viewChangeEvidence").asList.map(parseViewChangeEvidence)
          val viewChanges = evidence.foldLeft(Map.empty[Int, Map[ReplicaId, Msg.ViewChange]]) { (acc, ev) =>
            val forView = acc.getOrElse(ev.vc.newView, Map.empty) + (ev.vc.from -> ev.vc)
            acc + (ev.vc.newView -> forView)
          }
          val locks = m.field("locks").asList.map { row =>
            val value = row.asMap.get("value") match
              case Some(CTag("some", CBytes(bs))) => Some(Value(bs))
              case _ => None
            val proof = parsePrepared(row.field("proof")).headOption
            row.field("seq").asInt.toInt -> PreparedLock(row.field("preparedView").asInt.toInt, Digest(row.field("digest").asStr), value, proof)
          }.toMap
          val cursor = FinalizedFederationCursor.fromCanon(m.field("cursor")).fold(e => throw CodecError(e), identity)
          Right(DecodedState(
            ReplicaState(id, n, faulty = false, view = view, slots = slots, viewChanges = viewChanges, locks = locks),
            replicaSet, federationId, commitSeals, prepareSeals, prePrepareSeals, evidence, cursor))
        catch case e: CodecError => Left(e.getMessage)
      case other => Left(s"not federation-replica-state: $other")

  private def loadCerts(path: Path): Either[String, List[FederationFinality.FederationFinalityCertificate]] =
    if !Files.exists(path) then Right(Nil)
    else
      Canon.decode(Files.readAllBytes(path)).flatMap {
        case Canon.CList(xs) =>
          xs.foldLeft[Either[String, List[FederationFinality.FederationFinalityCertificate]]](Right(Nil)) { (acc, c) =>
            acc.flatMap(cs => FederationFinality.FederationFinalityCertificate.fromCanon(c).map(cs :+ _))
          }
        case other => Left(s"bad federation certs payload: $other")
      }

  /** Verifies the manifest's own seals and that `keypair`'s public key
    * genuinely matches its entry in the manifest (a replica can never be
    * instantiated under someone else's identity) — mirrors
    * [[BftReplica.certified]] exactly.
    */
  def certified(
      keypair: Keypair,
      manifest: ReplicaSetManifest,
      federationId: Digest,
      verify: VerifyProposal,
      genesisState: Digest,
      resolveUrl: ReplicaId => Option[String] = _ => None,
      stateStore: Option[Path] = None,
      certStore: Option[Path] = None,
      nsCacheStore: Option[Path] = None,
  ): Either[String, FederationReplica] =
    for
      _ <- ReplicaSetManifest.verifySeals(manifest, Ed25519.verify)
      expected <- manifest.authorities.get(keypair.name).toRight(s"federation: replica '${keypair.name}' not in replica-set manifest")
      _ <- Either.cond(expected == keypair.publicBytes, (),
        s"federation: local key for '${keypair.name}' does not match replica-set manifest")
    yield new FederationReplica(keypair, manifest, federationId, verify, genesisState, resolveUrl, stateStore, certStore, nsCacheStore)

/** See [[FederationReplica$]] (companion) for the rationale. Constructed
  * only via [[FederationReplica.certified]].
  */
final class FederationReplica private (
    val keypair: Keypair,
    private val manifest: ReplicaSetManifest,
    val federationId: Digest,
    private val verifyProposal: FederationReplica.VerifyProposal,
    genesisState: Digest,
    private val resolveUrl: BftQuorum.ReplicaId => Option[String],
    stateStore: Option[Path],
    certStore: Option[Path],
    nsCacheStore: Option[Path],
):
  import BftQuorum.*
  import FederationReplica.*

  def authorities: Map[String, Vector[Byte]] = manifest.authorities
  def replicaIds: List[ReplicaId] = manifest.ids.map(ReplicaId(_))
  private def n: Int = manifest.ids.length
  def id: ReplicaId = ReplicaId(keypair.name)
  def setDigest: Digest = manifest.replicaSetDigest

  private var state: ReplicaState = ReplicaState(id, n, faulty = false)
  private val outbound = scala.collection.mutable.ListBuffer.empty[BftFinality.SignedMsg]
  private val commitSeals = scala.collection.mutable.Map.empty[(Int, Int, String, String), Vector[Byte]]
  private val prepareSeals = scala.collection.mutable.Map.empty[(Int, Int, String, String), Vector[Byte]]
  // Keeps the full Value (not just its digest hex, already in the key) so a
  // later conflicting PrePrepare from the same replica can be reconstructed
  // into a real, independently-signature-verifiable Msg.PrePrepare for
  // EquivocationEvidence.detect, not just a rejected digest mismatch.
  private val prePrepareSeals = scala.collection.mutable.Map.empty[(Int, Int, String), (ReplicaId, Vector[Byte], Value)]
  private val viewChangeEvidence = scala.collection.mutable.Map.empty[(Int, String), ViewChangeEvidence]
  private val equivocations = scala.collection.mutable.ListBuffer.empty[EquivocationEvidence]
  // Not persisted: a proposal is re-learned on restart the same way it's
  // learned the first time (from `propose` locally, or the HTTP
  // `/federation/propose`/`/federation/msg` request body for every other
  // replica — PR33 slice 5) before any PrePrepare referencing it is
  // reprocessed; never a correctness input on its own (see `learnProposal`).
  // Keyed by the proposal's OWN artifact digest (PR33.1) — the same digest
  // `valueOfProposal` embeds in the wire `Value`, since that's now what a
  // vote is cryptographically over (see `FederationFinality.valueOfProposal`'s
  // doc comment for why keying by `proposal.after` alone was ambiguous).
  private val knownProposals = scala.collection.mutable.Map.empty[Digest, FederationFinality.FederationProposal]
  private var certificates: List[FederationFinality.FederationFinalityCertificate] = Nil
  private var ioError: Option[String] = None
  private var lastViewChangeStartedAt: Long = 0L
  private var lastPrimaryActivityAt: Long = System.currentTimeMillis()
  private var latestNewView: Option[Digest] = None
  private var nsCache: FederationReplica.NamespaceCertCache = FederationReplica.NamespaceCertCache.empty
  private var cursor: FinalizedFederationCursor = FinalizedFederationCursor.genesis(genesisState)

  certStore.foreach { path =>
    loadCerts(path) match
      case Right(certs) =>
        certificates = certs.filter(c => FederationFinality.FederationFinalityCertificate.verify(c, manifest).isRight)
      case Left(e) => ioError = Some(s"federation-certs restore failed: $e")
  }

  stateStore.foreach { path =>
    if Files.exists(path) && ioError.isEmpty then
      Canon.decode(Files.readAllBytes(path)).flatMap(decodeState) match
        case Right(decoded)
            if decoded.state.id.id == keypair.name && decoded.state.n == n &&
              decoded.replicaSet == manifest.replicaSetDigest && decoded.federationId == federationId =>
          state = decoded.state
          commitSeals ++= decoded.commitSeals
          prepareSeals ++= decoded.prepareSeals
          prePrepareSeals ++= decoded.prePrepareSeals
          decoded.viewChangeEvidence.foreach(ev => viewChangeEvidence((ev.vc.newView, ev.vc.from.id)) = ev)
          cursor = decoded.cursor
        case Right(decoded) =>
          ioError = Some(
            s"federation-state identity mismatch: file has ${decoded.state.id.id}/n=${decoded.state.n}, " +
              s"replica is ${keypair.name}/n=$n")
        case Left(e) => ioError = Some(s"federation-state restore failed: $e")
  }

  // Deliberately fail-OPEN, unlike state/certStore above: a lost or
  // corrupted namespace-cert cache degrades to "treat as never bootstrapped"
  // (redundant re-certification on the next round), never to "trust
  // without checking" — see `NamespaceCertCache`'s own doc comment.
  nsCacheStore.foreach { path =>
    if Files.exists(path) then
      Canon.decode(Files.readAllBytes(path)).flatMap(FederationReplica.NamespaceCertCache.fromCanon) match
        case Right(cache) => nsCache = cache
        case Left(_) => ()
  }

  /** Registers a proposal's content so a later `PrePrepare` naming its
    * `after` digest (all a bare `Msg.PrePrepare` carries — see
    * `FederationFinality`'s `valueOfState` doc comment) can be checked and
    * verified. Trusting an entry inserted here is never itself a safety
    * requirement: [[bindPrePrepare]]'s verify callback independently
    * re-derives everything from CAS content, so a forged entry (wrong
    * `transition` for the claimed `after`) only ever causes a clean
    * rejection, never a wrongly-accepted vote.
    */
  def learnProposal(proposal: FederationFinality.FederationProposal): Unit =
    knownProposals(proposal.digest) = proposal

  /** Lets a peer catch another replica up on a proposal it never received
    * over `/federation/propose` (e.g. it was down, or its own delivery was
    * lost) — the HTTP layer's `/federation/proposal/<hex>` endpoint serves
    * this, keyed by the proposal's own artifact digest (PR33.1 — the same
    * digest a `PrePrepare`'s `Value` now embeds, recoverable directly via
    * `proposalDigestOf`).
    */
  def knownProposal(proposalDigest: Digest): Option[FederationFinality.FederationProposal] =
    knownProposals.get(proposalDigest)

  private def proposalDigestOf(value: Value): Digest =
    Digest(String(value.bytes.toArray, java.nio.charset.StandardCharsets.US_ASCII))

  def drainOutbound(): List[BftFinality.SignedMsg] =
    val xs = outbound.toList
    outbound.clear()
    xs
  def finalityCerts: List[FederationFinality.FederationFinalityCertificate] = certificates
  def detectedEquivocations: List[EquivocationEvidence] = equivocations.toList
  def currentView: Int = state.view
  def finalizedCursor: FinalizedFederationCursor = cursor

  /** Signed snapshot of this replica's current view + latest installed
    * NewView digest — lets a client determine the cluster's actual view
    * via quorum evidence (multiple signed statuses agreeing), never from
    * HTTP fan-out response timing alone. Mirrors `BftReplica.viewStatus`.
    */
  def viewStatus: FederationFinality.FederationViewStatus =
    FederationFinality.FederationViewStatus.sign(keypair, state.view, latestNewView, federationId, setDigest)

  private def refuseIfCorrupt[A](op: => Either[String, A]): Either[String, A] =
    ioError match
      case Some(e) => Left(s"federation: refusing to operate after durable I/O failure ($e)")
      case None => op

  private def persistState(): Either[String, Unit] =
    stateStore match
      case None => Right(())
      case Some(path) =>
        val c = encodeState(state, setDigest, federationId, commitSeals.toMap, prepareSeals.toMap, prePrepareSeals.toMap, viewChangeEvidence.values.toList, cursor)
        DurableIo.writeConsensus(path, Canon.encode(c)) match
          case Left(e) => ioError = Some(e); Left(e)
          case Right(()) => Right(())

  private def persistCerts(): Either[String, Unit] =
    certStore match
      case None => Right(())
      case Some(path) =>
        val c = Canon.CList(certificates.map(_.canon))
        DurableIo.writeConsensus(path, Canon.encode(c)) match
          case Left(e) => ioError = Some(e); Left(e)
          case Right(()) => Right(())

  /** Best-effort only — see `NamespaceCertCache`'s doc comment for why a
    * write failure here must never trip fail-closed the way `persistState`/
    * `persistCerts` do: the cache is purely an optimization/bootstrap
    * marker, never a correctness input.
    */
  private def persistNsCache(): Unit =
    nsCacheStore.foreach(path => DurableIo.writeConsensus(path, Canon.encode(nsCache.canon)))

  def namespaceCertCache: FederationReplica.NamespaceCertCache = nsCache

  /** Real certificate minting: recovers the PROPOSAL digest this slot
    * decided on directly from its own `PrePrepare`'s `Value` bytes
    * (recoverable ASCII hex — see `FederationFinality.valueOfProposal`'s
    * doc comment), then looks up the full proposal via [[learnProposal]]
    * for the `after`/`previousState`/`epoch`/`transition` fields the
    * minted certificate needs beyond the bare digest every vote actually
    * signed.
    */
  private def mintCertificateIfReady(view: Int, seq: Int): Unit =
    (for
      slot <- state.slots.get((view, seq))
      decidedValue <- slot.decided
      proposal <- knownProposals.get(proposalDigestOf(decidedValue))
    yield (decidedValue, proposal)) match
      case None => ()
      case Some((decidedValue, proposal)) =>
        if certificates.exists(c => c.view == view && c.seq == seq) then ()
        else
          val commits = commitSeals.collect {
            case ((v, s, hex, rid), seal) if v == view && s == seq && hex == decidedValue.digest.hex => ReplicaId(rid) -> seal
          }.toList
          if commits.map(_._1.id).distinct.length >= quorumSize(n) then
            val cert = FederationFinality.FederationFinalityCertificate(proposal.digest, proposal.transition,
              proposal.after, view, seq, commits, setDigest, proposal.epoch, proposal.before, proposal.federationId)
            if FederationFinality.FederationFinalityCertificate.verify(cert, manifest).isRight then
              certificates = certificates :+ cert
              cursor.advancedBy(cert).foreach(next => cursor = next)

  /** PR33.1 slice 3: adopt a certificate this replica never voted on — the
    * late-arriving-vote gap the roadmap already flagged (a replica whose
    * own Commit lands after another quorum has already finished cannot,
    * before this, ever independently learn the resulting certificate).
    * `certificate`/`proposal` are expected to have been fetched by the
    * caller from a peer, keyed by `certificate.proposal` (the ordinary CAS/
    * `/federation/proposal/<hex>` route — no separate wire path needed).
    *
    * Deliberately mirrors `bindPrePrepare`'s checks rather than trusting the
    * certificate at face value: signatures alone prove a quorum voted for
    * `certificate.proposal`, not that `proposal` genuinely deep-verifies, so
    * adoption re-runs the same local-CAS-only `verifyProposal` callback
    * (same `MissingClosure`/`Rejected` outcomes, same `missingClosurePrefix`
    * convention) — the HTTP layer fetches and retries exactly as it already
    * does for `receive`. Never performs network I/O itself.
    */
  def adoptCertificate(
      certificate: FederationFinality.FederationFinalityCertificate,
      proposal: FederationFinality.FederationProposal,
  ): Either[String, AdoptedGeneration] =
    refuseIfCorrupt {
      val priorCerts = certificates
      val priorCursor = cursor
      val priorNsCache = nsCache
      def rollback(): Unit =
        certificates = priorCerts
        cursor = priorCursor
        nsCache = priorNsCache

      val already =
        if certificate.epoch < cursor.epoch then
          Some(Left(s"federation: certificate epoch ${certificate.epoch} is behind the finalized cursor (epoch ${cursor.epoch})"))
        else if certificate.epoch == cursor.epoch then
          Some(
            if cursor.state == certificate.stateDigest then
              Right(AdoptedGeneration(cursor.state, cursor.transition.getOrElse(certificate.transition), cursor.epoch))
            else
              Left(s"federation: certificate at epoch ${certificate.epoch} names a different state than this replica's finalized cursor"))
        else None

      already.getOrElse {
        val result =
          for
            _ <- Either.cond(certificate.proposal == proposal.digest, (),
              "federation: certificate does not name this proposal")
            _ <- FederationFinality.FederationFinalityCertificate.verify(certificate, manifest)
            _ <- Either.cond(proposal.federationId == federationId, (), "federation: proposal federationId mismatch")
            _ <- Either.cond(proposal.replicaSet == setDigest, (), "federation: proposal replicaSet mismatch")
            _ <- Either.cond(cursor.extendedBy(proposal), (),
              s"federation: proposal does not extend finalized cursor (cursor state=${cursor.state.short} epoch=${cursor.epoch}, " +
                s"proposal before=${proposal.before.short} epoch=${proposal.epoch})")
            outcome <-
              val (o, updatedCache) = verifyProposal(designatedPrimary(replicaIds, certificate.view).getOrElse(id), proposal, nsCache)
              nsCache = updatedCache
              o match
                case VerifyOutcome.Verified => Right(())
                case VerifyOutcome.MissingClosure(digests) => Left(s"$missingClosurePrefix${digests.map(_.hex).mkString(",")}")
                case VerifyOutcome.Rejected(reason) => Left(s"federation: proposal rejected: $reason")
          yield ()
        result match
          case Left(e) => rollback(); Left(e)
          case Right(()) =>
            if !certificates.exists(c => c.proposal == certificate.proposal) then
              certificates = certificates :+ certificate
            cursor.advancedBy(certificate).foreach(next => cursor = next)
            persistNsCache()
            (for
              _ <- persistState()
              _ <- persistCerts()
            yield AdoptedGeneration(certificate.stateDigest, certificate.transition, certificate.epoch)) match
              case Left(e) => rollback(); Left(e)
              case Right(gen) => Right(gen)
      }
    }

  /** Primary-only: propose a [[FederationFinality.FederationProposal]] for
    * agreement at `view`. `seq` is `proposal.epoch` (the shared clock
    * convention already established by [[FederationFinality.agreeForFederationStateLocalTestOnly]]:
    * `seq == epoch`).
    */
  def propose(view: Int, proposal: FederationFinality.FederationProposal): Either[String, List[BftFinality.SignedMsg]] =
    refuseIfCorrupt {
      for
        _ <- Either.cond(view == state.view, (), s"federation: propose view $view != current view ${state.view}")
        primary <- designatedPrimary(replicaIds, view).toRight("federation: no designated primary")
        _ <- Either.cond(primary.id == keypair.name, (),
          s"federation: this replica ${keypair.name} is not primary for view $view (${primary.id})")
        _ <- Either.cond(proposal.federationId == federationId, (), "federation: proposal federationId mismatch")
        _ <- Either.cond(proposal.replicaSet == setDigest, (), "federation: proposal replicaSet mismatch")
        _ = learnProposal(proposal)
        pp <- BftFinality.sign(keypair, Msg.PrePrepare(view, proposal.epoch.toInt, FederationFinality.valueOfProposal(proposal.digest), primary), setDigest, federationId)
        out <- receive(pp)
      yield pp :: out
    }

  /** Start a view-change toward the immediate successor view only — bounded
    * and rate-limited, mirroring [[BftReplica.requestViewChange]].
    */
  def requestViewChange(newView: Int): Either[String, List[BftFinality.SignedMsg]] =
    refuseIfCorrupt {
      val now = System.currentTimeMillis()
      if newView != state.view + 1 then
        Left(s"federation: only immediate successor view allowed (current=${state.view}, requested=$newView)")
      else if lastViewChangeStartedAt > 0L && now - lastViewChangeStartedAt < MinViewChangeIntervalMs then
        Left(s"federation: view-change rate limited (min interval ${MinViewChangeIntervalMs}ms)")
      else
        lastViewChangeStartedAt = now
        val prepared = preparedFromSlots(state).flatMap { pc =>
          val votes = prepareSeals.collect {
            case ((v, s, hex, rid), seal) if v == pc.preparedView && s == pc.seq && hex == pc.valueDigest.hex => ReplicaId(rid) -> seal
          }.toList
          val pp = prePrepareSeals.get((pc.preparedView, pc.seq, pc.valueDigest.hex))
          if pc.value.isEmpty || pp.isEmpty || votes.map(_._1.id).distinct.length < quorumSize(n) then None
          else Some(pc.copy(prepareVotes = votes, prePrepareSeal = pp.map(_._2), prePrepareFrom = pp.map(_._1)))
        }
        BftFinality.sign(keypair, Msg.ViewChange(newView, prepared, id), setDigest, federationId).flatMap { vc =>
          receive(vc).map(out => vc :: out)
        }
    }

  /** Remote pacemaker trigger: additionally requires this replica's OWN
    * elapsed time since it last observed the primary's activity to exceed
    * [[FederationPrimaryTimeoutMs]] — a single remote signer cannot make an
    * honest replica manufacture a vote on its say-so alone.
    */
  def requestViewChangeIfTimedOut(newView: Int): Either[String, List[BftFinality.SignedMsg]] =
    val elapsed = System.currentTimeMillis() - lastPrimaryActivityAt
    if elapsed < FederationPrimaryTimeoutMs then
      Left(s"federation: no local primary-timeout evidence yet (${elapsed}ms < ${FederationPrimaryTimeoutMs}ms)")
    else requestViewChange(newView)

  def receive(sm: BftFinality.SignedMsg): Either[String, List[BftFinality.SignedMsg]] =
    refuseIfCorrupt {
      val priorState = state
      val priorCommit = commitSeals.toMap
      val priorPrepare = prepareSeals.toMap
      val priorPp = prePrepareSeals.toMap
      val priorVc = viewChangeEvidence.toMap
      val priorCerts = certificates
      val priorCursor = cursor
      def rollback(): Unit =
        state = priorState
        commitSeals.clear(); commitSeals ++= priorCommit
        prepareSeals.clear(); prepareSeals ++= priorPrepare
        prePrepareSeals.clear(); prePrepareSeals ++= priorPp
        viewChangeEvidence.clear(); viewChangeEvidence ++= priorVc
        certificates = priorCerts
        cursor = priorCursor

      /** Real, potentially expensive: PR30 deep re-certification pays real
        * CPU work (ΔL replay, access-trace recomputation, constitution
        * rerun) per namespace. Gossip fan-out redelivers an already-known
        * PrePrepare to a replica through multiple paths routinely (every
        * `/federation/msg` success re-broadcasts to every peer, regardless
        * of whether the delivery was actually novel) — without this
        * short-circuit, each redundant delivery would re-pay that full
        * cost from scratch, compounding with cluster size and quickly
        * dominating wall-clock time under real network fan-out.
        */
      def bindPrePrepare(pp: Msg.PrePrepare): Either[String, Unit] =
        val proposalDigest = proposalDigestOf(pp.value)
        val alreadyBound = state.slots.get((pp.view, pp.seq)).flatMap(_.prePrepare).contains(pp)
        if alreadyBound then Right(())
        else
          for
            _ <- Either.cond(designatedPrimary(replicaIds, pp.view).contains(pp.from), (),
              s"federation: PrePrepare from ${pp.from.id} is not the designated primary for view ${pp.view}")
            proposal <- knownProposals.get(proposalDigest)
              .toRight(s"$missingClosurePrefix${proposalDigest.hex}")
            _ <- Either.cond(cursor.extendedBy(proposal), (),
              s"federation: proposal does not extend finalized cursor (cursor state=${cursor.state.short} epoch=${cursor.epoch}, " +
                s"proposal before=${proposal.before.short} epoch=${proposal.epoch})")
            _ <-
              val (outcome, updatedCache) = verifyProposal(pp.from, proposal, nsCache)
              if updatedCache != nsCache then
                nsCache = updatedCache
                persistNsCache()
              outcome match
                case VerifyOutcome.Verified => Right(())
                case VerifyOutcome.MissingClosure(digests) => Left(s"$missingClosurePrefix${digests.map(_.hex).mkString(",")}")
                case VerifyOutcome.Rejected(reason) => Left(s"federation: proposal rejected: $reason")
          yield ()

      /** A conflicting PrePrepare — same (view, seq, from), different value —
        * arriving after this replica already recorded one is first-class
        * evidence, not just a rejection: both the newly-arrived and the
        * already-recorded PrePrepare are reconstructed as real, independently
        * signature-verifiable `SignedMsg`s and handed to [[EquivocationEvidence.detect]],
        * mirroring [[BftReplica]]'s own conflict branch.
        */
      def detectEquivocation(pp: Msg.PrePrepare, seal: Vector[Byte]): Unit =
        prePrepareSeals.collectFirst {
          case ((v, s, hex), (rid, otherSeal, otherValue)) if v == pp.view && s == pp.seq && hex != pp.value.digest.hex && rid == pp.from =>
            (otherSeal, otherValue)
        }.foreach { (otherSeal, otherValue) =>
          val a = BftFinality.SignedMsg(pp, pp.from, seal, setDigest, federationId)
          val other = Msg.PrePrepare(pp.view, pp.seq, otherValue, pp.from)
          val b = BftFinality.SignedMsg(other, pp.from, otherSeal, setDigest, federationId)
          EquivocationEvidence.detect(a, b, authorities, setDigest, federationId).foreach(ev => equivocations += ev)
        }

      def step(m: BftFinality.SignedMsg): Either[String, List[Msg]] =
        BftFinality.verify(authorities, m, Some(setDigest), Some(federationId)).flatMap { _ =>
          m.msg match
            case pp: Msg.PrePrepare =>
              bindPrePrepare(pp).flatMap { _ =>
                detectEquivocation(pp, m.seal)
                val (st2, out) = deliver(state, pp)
                val advanced = st2.slots.get((pp.view, pp.seq)).exists(_.prePrepare.contains(pp))
                if advanced then
                  prePrepareSeals((pp.view, pp.seq, pp.value.digest.hex)) = (pp.from, m.seal, pp.value)
                  lastPrimaryActivityAt = System.currentTimeMillis()
                state = st2
                Right(out)
              }
            case p: Msg.Prepare =>
              val (st2, out) = deliver(state, p)
              state = st2
              prepareSeals((p.view, p.seq, p.valueDigest.hex, p.from.id)) = m.seal
              Right(out)
            case c: Msg.Commit =>
              val (st2, out) = deliver(state, c)
              state = st2
              commitSeals((c.view, c.seq, c.valueDigest.hex, c.from.id)) = m.seal
              mintCertificateIfReady(c.view, c.seq)
              Right(out)
            case vc: Msg.ViewChange =>
              viewChangeEvidence((vc.newView, vc.from.id)) = ViewChangeEvidence(vc, m.seal, m.replicaSet, m.chainId)
              val (st2, out) = deliverViewChange(state, vc, replicaIds)
              state = st2
              Right(out)
            case nv: Msg.NewView =>
              val (st2, out) = deliverNewView(state, nv, replicaIds)
              if st2.view > state.view then
                lastPrimaryActivityAt = System.currentTimeMillis()
                latestNewView = Some(Digest.of(BftFinality.msgCanon(nv)))
              state = st2
              Right(out)
        }

      def pump(pending: List[Msg], acc: List[BftFinality.SignedMsg]): Either[String, List[BftFinality.SignedMsg]] =
        pending match
          case Nil =>
            persistState().flatMap(_ =>
              persistCerts().map { _ =>
                outbound ++= acc
                acc
              })
          case h :: t =>
            BftFinality.sign(keypair, h, setDigest, federationId) match
              case Left(e) => Left(e)
              case Right(signed) =>
                step(signed) match
                  case Left(e) => Left(e)
                  case Right(more) => pump(t ++ more, acc :+ signed)

      step(sm) match
        case Left(e) => rollback(); Left(e)
        case Right(followOns) =>
          pump(followOns, Nil) match
            case Left(e) => rollback(); Left(e)
            case Right(out) => Right(out)
    }
