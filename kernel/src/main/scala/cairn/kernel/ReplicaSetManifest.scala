package cairn.kernel

/** Certified static BFT replica set: sorted unique replica ids with public keys.
  *
  * This — not unsigned peer-directory gossip entries — is the root of replica
  * authority. Each independent process stores only its own private key; peer
  * public keys come from this manifest (or its digest on a ledger/CAS).
  *
  * Stored as [[ArtifactKind.Certificate]] tagged `replica-set-manifest`.
  * `n` must be a valid classic size ([[BftQuorum.validReplicaCount]]).
  */
final case class ReplicaSetManifest(
    replicas: List[(String, Vector[Byte])],
):
  def ids: List[String] = replicas.map(_._1)
  def authorities: Map[String, Vector[Byte]] = replicas.toMap
  def n: Int = replicas.length
  def replicaSetDigest: Digest = Digest.of(Canon.cstrs(ids.sorted))

  def canon: Canon = Canon.CTag("replica-set-manifest", Canon.CList(
    replicas.sortBy(_._1).map { (id, pk) =>
      Canon.cmap("id" -> Canon.CStr(id), "publicKey" -> Canon.CBytes(pk))
    }))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)
  def digest: Digest = artifact.digest

object ReplicaSetManifest:
  def fromCanon(c: Canon): Either[String, ReplicaSetManifest] =
    import Canon.*
    c match
      case CTag("replica-set-manifest", CList(rows)) =>
        try
          val reps = rows.map { row =>
            row.field("id").asStr -> (row.field("publicKey") match
              case CBytes(bs) => bs
              case other      => throw CodecError(s"publicKey: $other"))
          }
          wellFormed(ReplicaSetManifest(reps)).map(_ => ReplicaSetManifest(reps))
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
    else Right(())

  def of(replicas: List[(String, Vector[Byte])]): Either[String, ReplicaSetManifest] =
    val m = ReplicaSetManifest(replicas.sortBy(_._1))
    wellFormed(m).map(_ => m)
