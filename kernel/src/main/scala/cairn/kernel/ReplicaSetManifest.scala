package cairn.kernel

/** Certified static BFT replica set: sorted unique replica ids with public keys,
  * plus per-member seals over that body.
  *
  * [[replicaSetDigest]] binds **names and public keys** (not names alone), so
  * swapping a key under the same id changes the digest. Seals are persisted in
  * the artifact so the CAS object is self-authenticating against the member set.
  *
  * Stored as [[ArtifactKind.Certificate]] tagged `replica-set-manifest`.
  * `n` must be a valid classic size ([[BftQuorum.validReplicaCount]]).
  */
final case class ReplicaSetManifest(
    replicas: List[(String, Vector[Byte])],
    /** Replica id → Ed25519 seal over [[bodyCanon]]. */
    seals: List[(String, Vector[Byte])] = Nil,
):
  def ids: List[String] = replicas.map(_._1)
  def authorities: Map[String, Vector[Byte]] = replicas.toMap
  def n: Int = replicas.length

  /** Unsigned body: sorted (id, publicKey) pairs — the digest input. */
  def bodyCanon: Canon = Canon.CList(
    replicas.sortBy(_._1).map { (id, pk) =>
      Canon.cmap("id" -> Canon.CStr(id), "publicKey" -> Canon.CBytes(pk))
    })

  def replicaSetDigest: Digest = Digest.of(bodyCanon)

  def canon: Canon = Canon.CTag("replica-set-manifest", Canon.cmap(
    "replicas" -> bodyCanon,
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
          val rows = body match
            // Legacy: bare list of {id, publicKey} without seals.
            case CList(xs) => xs
            case m => m.field("replicas") match
              case CList(xs) => xs
              case other     => throw CodecError(s"replicas: $other")
          val reps = rows.map { row =>
            row.field("id").asStr -> (row.field("publicKey") match
              case CBytes(bs) => bs
              case other      => throw CodecError(s"publicKey: $other"))
          }
          val seals = body match
            case CList(_) => Nil
            case m =>
              m.asMap.get("seals") match
                case Some(CList(xs)) =>
                  xs.map { row =>
                    row.field("id").asStr -> (row.field("seal") match
                      case CBytes(bs) => bs
                      case other      => throw CodecError(s"seal: $other"))
                  }
                case _ => Nil
          wellFormed(ReplicaSetManifest(reps, seals)).map(_ => ReplicaSetManifest(reps, seals))
        catch case e: CodecError => Left(e.getMessage)
      case other => Left(s"not a replica-set-manifest: $other")

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
    else Right(())

  /** Build an unsigned manifest (caller must [[seal]] before trusting it). */
  def of(replicas: List[(String, Vector[Byte])]): Either[String, ReplicaSetManifest] =
    val m = ReplicaSetManifest(replicas.sortBy(_._1), Nil)
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
    else wellFormed(unsigned.copy(seals = seals.sortBy(_._1))).map(_ => unsigned.copy(seals = seals.sortBy(_._1)))

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
