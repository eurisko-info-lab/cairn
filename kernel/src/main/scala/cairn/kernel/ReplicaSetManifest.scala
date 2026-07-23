package cairn.kernel

/** Certified BFT replica set: sorted unique replica ids with public keys,
  * plus per-member seals over that body.
  *
  * [[replicaSetDigest]] binds **names and public keys** (not names alone), so
  * swapping a key under the same id changes the digest. Seals are persisted in
  * the artifact so the CAS object is self-authenticating against the member set.
  *
  * Amendments cite the predecessor sealed artifact digest via [[replaces]] and
  * become active at [[activationHeight]] on the ledger (see [[allowsTransition]]).
  *
  * Stored as [[ArtifactKind.Certificate]] tagged `replica-set-manifest`.
  * `n` must be a valid classic size ([[BftQuorum.validReplicaCount]]).
  */
final case class ReplicaSetManifest(
    replicas: List[(String, Vector[Byte])],
    /** Replica id → Ed25519 seal over [[bodyCanon]]. */
    seals: List[(String, Vector[Byte])] = Nil,
    /** Predecessor sealed-manifest digest (absent on genesis). */
    replaces: Option[Digest] = None,
    /** Ledger height at which this set becomes active (0 = genesis / immediate). */
    activationHeight: Long = 0L,
):
  def ids: List[String] = replicas.map(_._1)
  def authorities: Map[String, Vector[Byte]] = replicas.toMap
  def n: Int = replicas.length

  /** Unsigned body: membership + transition metadata — the digest input. */
  def bodyCanon: Canon = Canon.cmap(
    "replicas" -> Canon.CList(
      replicas.sortBy(_._1).map { (id, pk) =>
        Canon.cmap("id" -> Canon.CStr(id), "publicKey" -> Canon.CBytes(pk))
      }),
    "replaces" -> replaces.fold(Canon.CTag("none", Canon.CInt(0)))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))),
    "activationHeight" -> Canon.CInt(activationHeight))

  def replicaSetDigest: Digest = Digest.of(bodyCanon)

  def canon: Canon = Canon.CTag("replica-set-manifest", Canon.cmap(
    "body" -> bodyCanon,
    "seals" -> Canon.CList(seals.sortBy(_._1).map { (id, seal) =>
      Canon.cmap("id" -> Canon.CStr(id), "seal" -> Canon.CBytes(seal))
    })))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)
  def digest: Digest = artifact.digest

object ReplicaSetManifest:
  def fromCanon(c: Canon): Either[String, ReplicaSetManifest] =
    import Canon.*
    c match
      case CTag("replica-set-manifest", body) =>
        try
          // Current: { body: { replicas, replaces, activationHeight }, seals }
          // Legacy A: { replicas: CList, seals? }
          // Legacy B: bare CList of {id, publicKey}
          val (reps, replaces, height, seals) = body match
            case CList(xs) =>
              (parseReplicaRows(xs), None, 0L, Nil)
            case m =>
              m.asMap.get("body") match
                case Some(b) =>
                  val rows = b.field("replicas") match
                    case CList(xs) => xs
                    case other     => throw CodecError(s"replicas: $other")
                  val rep = b.asMap.get("replaces") match
                    case Some(CTag("some", CStr(hex))) => Some(Digest(hex))
                    case _                             => None
                  val h = b.asMap.get("activationHeight") match
                    case Some(CInt(n)) => n
                    case _             => 0L
                  val seals = m.asMap.get("seals") match
                    case Some(CList(xs)) => parseSealRows(xs)
                    case _               => Nil
                  (parseReplicaRows(rows), rep, h, seals)
                case None =>
                  val rows = m.field("replicas") match
                    case CList(xs) => xs
                    case other     => throw CodecError(s"replicas: $other")
                  val seals = m.asMap.get("seals") match
                    case Some(CList(xs)) => parseSealRows(xs)
                    case _               => Nil
                  (parseReplicaRows(rows), None, 0L, seals)
          val draft = ReplicaSetManifest(reps, seals, replaces, height)
          wellFormed(draft).map(_ => draft)
        catch case e: CodecError => Left(e.getMessage)
      case other => Left(s"not a replica-set-manifest: $other")

  private def parseReplicaRows(rows: List[Canon]): List[(String, Vector[Byte])] =
    import Canon.*
    rows.map { row =>
      row.field("id").asStr -> (row.field("publicKey") match
        case CBytes(bs) => bs
        case other      => throw CodecError(s"publicKey: $other"))
    }

  private def parseSealRows(rows: List[Canon]): List[(String, Vector[Byte])] =
    import Canon.*
    rows.map { row =>
      row.field("id").asStr -> (row.field("seal") match
        case CBytes(bs) => bs
        case other      => throw CodecError(s"seal: $other"))
    }

  def wellFormed(m: ReplicaSetManifest): Either[String, Unit] =
    val ids = m.ids
    if ids.isEmpty then Left("replica-set: empty")
    else if ids.length != ids.distinct.length then
      Left(s"replica-set: duplicate ids: ${ids.mkString(",")}")
    else if !BftQuorum.validReplicaCount(ids.length) then
      Left(s"replica-set: n=${ids.length} is not a valid 3f+1 size")
    else if m.replicas.exists((_, pk) => pk.isEmpty) then
      Left("replica-set: empty public key")
    else if m.seals.map(_._1).distinct.length != m.seals.length then
      Left("replica-set: duplicate seals")
    else if m.activationHeight < 0 then
      Left("replica-set: activationHeight must be >= 0")
    else Right(())

  /** Build an unsigned genesis manifest (caller must [[seal]] before trusting it). */
  def of(
      replicas: List[(String, Vector[Byte])],
      replaces: Option[Digest] = None,
      activationHeight: Long = 0L,
  ): Either[String, ReplicaSetManifest] =
    val m = ReplicaSetManifest(replicas.sortBy(_._1), Nil, replaces, activationHeight)
    wellFormed(m).map(_ => m)

  /** Attach a full set of member seals over [[bodyCanon]]. */
  def seal(
      unsigned: ReplicaSetManifest,
      signers: List[(String, Array[Byte] => Vector[Byte])],
  ): Either[String, ReplicaSetManifest] =
    val payload = Canon.encode(unsigned.bodyCanon)
    val seals = signers.map { (id, sign) => id -> sign(payload) }
    val missing = unsigned.ids.toSet -- seals.map(_._1).toSet
    if missing.nonEmpty then
      Left(s"replica-set: missing seals for ${missing.toList.sorted.mkString(",")}")
    else if seals.map(_._1).toSet != unsigned.ids.toSet then
      Left("replica-set: seal ids must equal replica ids")
    else
      val next = unsigned.copy(seals = seals.sortBy(_._1))
      wellFormed(next).map(_ => next)

  /** Verify every member seal against the manifest's own public keys. */
  def verifySeals(
      m: ReplicaSetManifest,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
  ): Either[String, Unit] =
    if m.seals.isEmpty then Left("replica-set: missing member seals (not endorsed)")
    else if m.seals.map(_._1).toSet != m.ids.toSet then
      Left("replica-set: seal coverage incomplete")
    else
      val payload = Canon.encode(m.bodyCanon)
      m.seals.foldLeft[Either[String, Unit]](Right(())) { case (acc, (id, seal)) =>
        acc.flatMap { _ =>
          m.authorities.get(id) match
            case None => Left(s"replica-set: seal for unknown id '$id'")
            case Some(pk) =>
              if verify(pk, payload, seal) then Right(())
              else Left(s"replica-set: bad seal from '$id'")
        }
      }

  /** Amendment policy (mirrors [[DomainAgreement.allowsTransition]]).
    *
    * `liveArtifactDigest` is the CAS/file digest of the prior sealed manifest.
    */
  def allowsTransition(
      proposed: ReplicaSetManifest,
      live: Option[ReplicaSetManifest],
      liveArtifactDigest: Option[Digest] = None,
  ): Either[String, Unit] =
    live match
      case None =>
        if proposed.replaces.isDefined then
          Left("replica-set: genesis must not set replaces")
        else Right(())
      case Some(prev) =>
        val expected = liveArtifactDigest.getOrElse(prev.digest)
        if !proposed.replaces.contains(expected) then
          Left(
            s"replica-set: amendment must replace ${expected.short} " +
              s"(got ${proposed.replaces.map(_.short).getOrElse("none")})")
        else if proposed.activationHeight <= prev.activationHeight then
          Left(
            s"replica-set: activationHeight ${proposed.activationHeight} must exceed " +
              s"predecessor ${prev.activationHeight}")
        else Right(())
