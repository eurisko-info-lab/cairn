package cairn.core

import cairn.kernel.*

/** Certified, independently-rotatable namespace ownership — one manifest per
  * namespace, mirroring [[cairn.kernel.ReplicaSetManifest]]'s exact amendment
  * shape (`replaces`/quorum-of-predecessor-approvals/strictly-increasing
  * activation point) but governing WHO MAY PUBLISH under one namespace,
  * not who runs BFT consensus.
  *
  * Namespaces are deliberately NOT unified under one shared manifest: PR31's
  * "authenticated federation" invariant is that namespace owners may rotate
  * trust "without becoming one global permissionless network" — a shared
  * manifest would force every namespace's owners to co-sign every other
  * namespace's rotation. `activationEpoch` shares the ledger-height clock
  * used by [[FederationState]]/[[ReplicatedGcEpoch]].
  *
  * Ownership rotation is a simple majority of the PREDECESSOR owner set
  * (not [[BftQuorum]]'s 3f+1 math) — namespace governance is not a
  * Byzantine-fault-tolerant replica protocol.
  */
final case class NamespaceTrustManifest(
    namespace: String,
    /** Owner name -> Ed25519 public key. */
    owners: List[(String, Vector[Byte])],
    /** Owner name -> seal over [[bodyCanon]] (this manifest's own endorsement). */
    seals: List[(String, Vector[Byte])] = Nil,
    /** Predecessor sealed-manifest digest (absent on genesis). */
    replaces: Option[Digest] = None,
    /** Ledger height at which this manifest becomes the active one for `namespace`. */
    activationEpoch: Long = 0L,
    /** Seals from the PREDECESSOR owner set over this manifest's [[bodyCanon]]. */
    predecessorApprovals: List[(String, Vector[Byte])] = Nil,
):
  def ids: List[String] = owners.map(_._1)
  def authorities: Map[String, Vector[Byte]] = owners.toMap
  def n: Int = owners.length

  /** Unsigned body: namespace + membership + transition metadata — the digest input. */
  def bodyCanon: Canon = Canon.cmap(
    "namespace" -> Canon.CStr(namespace),
    "owners" -> Canon.CList(owners.sortBy(_._1).map { (id, pk) =>
      Canon.cmap("id" -> Canon.CStr(id), "publicKey" -> Canon.CBytes(pk))
    }),
    "replaces" -> replaces.fold(Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "activationEpoch" -> Canon.CInt(activationEpoch))

  def bodyDigest: Digest = Digest.of(bodyCanon)

  def canon: Canon = Canon.CTag("namespace-trust-manifest-v1", Canon.cmap(
    "body" -> bodyCanon,
    "seals" -> Canon.CList(seals.sortBy(_._1).map { (id, seal) =>
      Canon.cmap("id" -> Canon.CStr(id), "seal" -> Canon.CBytes(seal))
    }),
    "predecessorApprovals" -> Canon.CList(predecessorApprovals.sortBy(_._1).map { (id, seal) =>
      Canon.cmap("id" -> Canon.CStr(id), "seal" -> Canon.CBytes(seal))
    })))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)
  def digest: Digest = artifact.digest

object NamespaceTrustManifest:
  private def parseRows(rows: List[Canon]): List[(String, Vector[Byte])] =
    rows.map { row =>
      row.field("id").asStr -> (row.field("publicKey") match
        case Canon.CBytes(bs) => bs
        case Canon.CStr(_) => throw CodecError("publicKey: expected bytes")
        case other => throw CodecError(s"publicKey: $other"))
    }

  private def parseSealRows(rows: List[Canon]): List[(String, Vector[Byte])] =
    rows.map { row =>
      row.field("id").asStr -> (row.field("seal") match
        case Canon.CBytes(bs) => bs
        case other => throw CodecError(s"seal: $other"))
    }

  def fromArtifact(artifact: Artifact): Either[String, NamespaceTrustManifest] =
    if artifact.kind != ArtifactKind.Certificate then Left("artifact is not a certificate")
    else artifact.body match
      case Canon.CTag("namespace-trust-manifest-v1", m) =>
        try
          val body = m.field("body")
          val namespace = body.field("namespace").asStr
          val owners = body.field("owners").asList
          val replaces = body.asMap.get("replaces") match
            case Some(Canon.CTag("some", Canon.CStr(d))) => Some(Digest(d))
            case _ => None
          val activationEpoch = body.field("activationEpoch").asInt
          val seals = m.asMap.get("seals") match
            case Some(Canon.CList(xs)) => parseSealRows(xs)
            case _ => Nil
          val preds = m.asMap.get("predecessorApprovals") match
            case Some(Canon.CList(xs)) => parseSealRows(xs)
            case _ => Nil
          val draft = NamespaceTrustManifest(namespace, parseRows(owners), seals, replaces, activationEpoch, preds)
          wellFormed(draft).map(_ => draft)
        catch case e: CodecError => Left(e.getMessage)
      case _ => Left("expected namespace-trust-manifest-v1 body")

  def wellFormed(m: NamespaceTrustManifest): Either[String, Unit] =
    val ids = m.ids
    if m.namespace.isEmpty then Left("namespace-trust: empty namespace")
    else if ids.isEmpty then Left("namespace-trust: empty owner set")
    else if ids.length != ids.distinct.length then Left(s"namespace-trust: duplicate owner ids: ${ids.mkString(",")}")
    else if m.owners.exists((_, pk) => pk.isEmpty) then Left("namespace-trust: empty owner public key")
    else if m.seals.map(_._1).distinct.length != m.seals.length then Left("namespace-trust: duplicate seals")
    else if m.predecessorApprovals.map(_._1).distinct.length != m.predecessorApprovals.length then
      Left("namespace-trust: duplicate predecessorApprovals")
    else if m.activationEpoch < 0 then Left("namespace-trust: activationEpoch must be >= 0")
    else if m.replaces.isEmpty && m.activationEpoch != 0 then Left("namespace-trust: genesis activationEpoch must be 0")
    else Right(())

  def of(
      namespace: String,
      owners: List[(String, Vector[Byte])],
      replaces: Option[Digest] = None,
      activationEpoch: Long = 0L,
      predecessorApprovals: List[(String, Vector[Byte])] = Nil,
  ): Either[String, NamespaceTrustManifest] =
    val m = NamespaceTrustManifest(namespace, owners.sortBy(_._1), Nil, replaces, activationEpoch, predecessorApprovals)
    wellFormed(m).map(_ => m)

  def seal(
      unsigned: NamespaceTrustManifest,
      signers: List[(String, Array[Byte] => Vector[Byte])],
  ): Either[String, NamespaceTrustManifest] =
    val payload = Canon.encode(unsigned.bodyCanon)
    val seals = signers.map { (id, sign) => id -> sign(payload) }
    val missing = unsigned.ids.toSet -- seals.map(_._1).toSet
    if missing.nonEmpty then Left(s"namespace-trust: missing seals for ${missing.toList.sorted.mkString(",")}")
    else if seals.map(_._1).toSet != unsigned.ids.toSet then Left("namespace-trust: seal ids must equal owner ids")
    else
      val next = unsigned.copy(seals = seals.sortBy(_._1))
      wellFormed(next).map(_ => next)

  def verifySeals(
      m: NamespaceTrustManifest,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
  ): Either[String, Unit] =
    if m.seals.isEmpty then Left("namespace-trust: missing owner seals (not endorsed)")
    else if m.seals.map(_._1).toSet != m.ids.toSet then Left("namespace-trust: seal coverage incomplete")
    else
      val payload = Canon.encode(m.bodyCanon)
      m.seals.foldLeft[Either[String, Unit]](Right(())) { case (acc, (id, seal)) =>
        acc.flatMap { _ =>
          m.authorities.get(id) match
            case None => Left(s"namespace-trust: seal for unknown owner '$id'")
            case Some(pk) => Either.cond(verify(pk, payload, seal), (), s"namespace-trust: bad seal from '$id'")
        }
      }

  /** Simple majority of the PREDECESSOR owner set — not [[BftQuorum]]'s 3f+1
    * math, since namespace governance is not itself a BFT replica protocol.
    */
  def majority(n: Int): Int = n / 2 + 1

  def verifyPredecessorApprovals(
      m: NamespaceTrustManifest,
      predecessor: NamespaceTrustManifest,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
  ): Either[String, Unit] =
    val q = majority(predecessor.n)
    val ids = m.predecessorApprovals.map(_._1)
    if ids.length != ids.distinct.length then Left("namespace-trust: duplicate predecessorApprovals")
    else if ids.exists(id => !predecessor.authorities.contains(id)) then
      Left("namespace-trust: predecessorApprovals cite unknown old owner")
    else if ids.distinct.length < q then
      Left(s"namespace-trust: predecessorApprovals ${ids.distinct.length} < majority $q")
    else
      val payload = Canon.encode(m.bodyCanon)
      m.predecessorApprovals.foldLeft[Either[String, Unit]](Right(())) { case (acc, (id, seal)) =>
        acc.flatMap { _ =>
          val pk = predecessor.authorities(id)
          Either.cond(verify(pk, payload, seal), (), s"namespace-trust: bad predecessorApproval from '$id'")
        }
      }

  /** Amendment policy: replaces + strictly-increasing activationEpoch (over
    * both the predecessor's own and any already-finalized epoch) + majority
    * of predecessor owner signatures. Mirrors
    * [[cairn.kernel.ReplicaSetManifest.allowsTransition]] exactly, with
    * majority in place of BFT quorum math.
    */
  def allowsTransition(
      proposed: NamespaceTrustManifest,
      live: Option[NamespaceTrustManifest],
      liveArtifactDigest: Option[Digest] = None,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean = (_, _, _) => true,
      minActivation: Long = -1L,
  ): Either[String, Unit] =
    live match
      case None =>
        if proposed.replaces.isDefined then Left("namespace-trust: genesis must not set replaces")
        else if proposed.predecessorApprovals.nonEmpty then Left("namespace-trust: genesis must not carry predecessorApprovals")
        else Right(())
      case Some(prev) =>
        val expected = liveArtifactDigest.getOrElse(prev.digest)
        if proposed.namespace != prev.namespace then
          Left(s"namespace-trust: amendment namespace '${proposed.namespace}' != predecessor '${prev.namespace}'")
        else if !proposed.replaces.contains(expected) then
          Left(s"namespace-trust: amendment must replace ${expected.short} (got ${proposed.replaces.map(_.short).getOrElse("none")})")
        else if proposed.activationEpoch <= prev.activationEpoch then
          Left(s"namespace-trust: activationEpoch ${proposed.activationEpoch} must exceed predecessor ${prev.activationEpoch}")
        else if proposed.activationEpoch <= minActivation then
          Left(s"namespace-trust: activationEpoch ${proposed.activationEpoch} must exceed finalized high-water $minActivation")
        else verifyPredecessorApprovals(proposed, prev, verify)

  /** Prefer verifying a full history via repeated [[allowsTransition]] checks
    * (mirrors [[cairn.kernel.ValidatedReplicaSetHistory]]); this is the
    * structural-only "which manifest is active at this epoch" query for
    * callers that already verified the transition chain.
    */
  def activeAt(history: List[NamespaceTrustManifest], epoch: Long): Either[String, NamespaceTrustManifest] =
    if history.isEmpty then Left("namespace-trust: empty history")
    else
      val ordered = history.sortBy(_.activationEpoch)
      ordered.filter(_.activationEpoch <= epoch).lastOption match
        case Some(m) => Right(m)
        case None => Left(s"namespace-trust: no manifest active at epoch $epoch (earliest=${ordered.head.activationEpoch})")
