package cairn.systemhandler

import cairn.kernel.*
import java.nio.file.{Files, Path}

/** Multi-home replica-set membership ceremony.
  *
  * Operator flow (genesis):
  *   1. each host: [[keygen]] + [[exportPubkey]]
  *   2. coordinator: [[importPubkey]] for every peer, then [[assemble]]
  *   3. distribute draft.canon; each member: [[sealMember]]
  *   4. gather seal contributions; coordinator: [[commit]]
  *   5. [[exportBundle]] / [[installBundle]] onto every home
  *
  * Amendments add [[approve]] (predecessor quorum) before [[commit]].
  *
  * Layout under `$CAIRN_HOME/ceremony/`:
  *   pubkeys/<id>.canon, draft.canon, seals/<id>.canon, approvals/<id>.canon
  */
object BftCeremony:
  private val PubkeyTag = "replica-pubkey"
  private val SealTag = "replica-set-seal"
  private val ApprovalTag = "replica-set-approval"
  private val BundleTag = "replica-set-bundle"

  def ceremonyRoot(home: Path): Path = home.resolve("ceremony")
  def pubkeysDir(home: Path): Path = ceremonyRoot(home).resolve("pubkeys")
  def sealsDir(home: Path): Path = ceremonyRoot(home).resolve("seals")
  def approvalsDir(home: Path): Path = ceremonyRoot(home).resolve("approvals")
  def draftPath(home: Path): Path = ceremonyRoot(home).resolve("draft.canon")

  def pubkeyPath(home: Path, id: String): Path =
    pubkeysDir(home).resolve(s"$id.canon")
  def sealPath(home: Path, id: String): Path =
    sealsDir(home).resolve(s"$id.canon")
  def approvalPath(home: Path, id: String): Path =
    approvalsDir(home).resolve(s"$id.canon")

  def pubkeyCanon(id: String, publicKey: Vector[Byte]): Canon =
    Canon.CTag(PubkeyTag, Canon.cmap(
      "id" -> Canon.CStr(id),
      "publicKey" -> Canon.CBytes(publicKey)))

  def parsePubkey(c: Canon): Either[String, (String, Vector[Byte])] =
    c match
      case Canon.CTag(PubkeyTag, m) =>
        try
          val id = m.field("id").asStr
          m.field("publicKey") match
            case Canon.CBytes(pk) if pk.nonEmpty => Right(id -> pk)
            case other => Left(s"replica-pubkey: bad publicKey $other")
        catch case e: CodecError => Left(e.getMessage)
      case other => Left(s"not a replica-pubkey: $other")

  private def contributionCanon(
      tag: String,
      id: String,
      seal: Vector[Byte],
      bodyDigest: Digest,
  ): Canon =
    Canon.CTag(tag, Canon.cmap(
      "id" -> Canon.CStr(id),
      "seal" -> Canon.CBytes(seal),
      "bodyDigest" -> Canon.CStr(bodyDigest.hex)))

  private def parseContribution(tag: String, c: Canon): Either[String, (String, Vector[Byte], Digest)] =
    c match
      case Canon.CTag(`tag`, m) =>
        try
          val id = m.field("id").asStr
          val dig = Digest(m.field("bodyDigest").asStr)
          m.field("seal") match
            case Canon.CBytes(seal) => Right((id, seal, dig))
            case other => Left(s"$tag: bad seal $other")
        catch case e: CodecError => Left(e.getMessage)
      case other => Left(s"not a $tag: $other")

  def bundleCanon(tip: ReplicaSetManifest, history: List[ReplicaSetManifest]): Canon =
    Canon.CTag(BundleTag, Canon.cmap(
      "tip" -> tip.canon,
      "history" -> Canon.CList(history.map(_.canon))))

  def parseBundle(c: Canon): Either[String, (ReplicaSetManifest, List[ReplicaSetManifest])] =
    c match
      case Canon.CTag(BundleTag, m) =>
        for
          tip <- ReplicaSetManifest.fromCanon(m.field("tip"))
          hist <- m.field("history") match
            case Canon.CList(xs) =>
              xs.foldLeft[Either[String, List[ReplicaSetManifest]]](Right(Nil)) { (acc, row) =>
                acc.flatMap(hs => ReplicaSetManifest.fromCanon(row).map(hs :+ _))
              }
            case other => Left(s"replica-set-bundle: bad history $other")
          _ <- ValidatedReplicaSetHistory.verify(hist, Ed25519.verify)
          _ <- Either.cond(
            hist.exists(_.digest == tip.digest), (),
            "replica-set-bundle: tip not present in history")
        yield (tip, hist)
      case other => Left(s"not a replica-set-bundle: $other")

  /** Create exactly one local replica identity (create-only keystore). */
  def keygen(home: Path, name: String, secret: Option[Array[Byte]]): Either[String, Keypair] =
    Keystore.loadOrCreate(home, name, secret)

  /** Write this host's public key for out-of-band exchange. */
  def exportPubkey(
      home: Path,
      name: String,
      out: Path,
      secret: Option[Array[Byte]],
  ): Either[String, Path] =
    for
      kp <- Keystore.load(home, name, secret)
      _ <- DurableIo.writeAtomic(out, Canon.encode(pubkeyCanon(kp.name, kp.publicBytes)))
    yield out

  /** Import a peer pubkey into `ceremony/pubkeys/<id>.canon`. */
  def importPubkey(home: Path, from: Path): Either[String, (String, Path)] =
    if !Files.exists(from) then Left(s"missing pubkey file $from")
    else
      for
        c <- Canon.decode(Files.readAllBytes(from))
        parsed <- parsePubkey(c)
        (id, pk) = parsed
        dest = pubkeyPath(home, id)
        _ <- DurableIo.writeAtomic(dest, Canon.encode(pubkeyCanon(id, pk)))
      yield (id, dest)

  private def listCanonFiles(dir: Path): List[Path] =
    if !Files.isDirectory(dir) then Nil
    else
      val stream = Files.list(dir)
      try
        stream.filter(p => p.getFileName.toString.endsWith(".canon")).toArray.toList
          .asInstanceOf[List[Path]]
      finally stream.close()

  def loadImportedPubkeys(home: Path): Either[String, Map[String, Vector[Byte]]] =
    listCanonFiles(pubkeysDir(home)).foldLeft[Either[String, Map[String, Vector[Byte]]]](Right(Map.empty)) {
      (acc, p) =>
        acc.flatMap { m =>
          Canon.decode(Files.readAllBytes(p)).flatMap(parsePubkey).map { (id, pk) =>
            m + (id -> pk)
          }
        }
    }

  /** Build an unsigned draft from imported pubkeys (genesis or amendment). */
  def assemble(
      home: Path,
      ids: List[String],
      activationHeight: Long = 0L,
      replaces: Option[Digest] = None,
  ): Either[String, ReplicaSetManifest] =
    if ids.isEmpty then Left("ceremony: assemble requires at least one replica id")
    else
      for
        pks <- loadImportedPubkeys(home)
        missing = ids.filterNot(pks.contains)
        _ <- Either.cond(missing.isEmpty, (),
          s"ceremony: missing imported pubkeys for ${missing.mkString(",")}")
        replacesResolved <- replaces match
          case Some(d) => Right(Some(d))
          case None if activationHeight == 0L &&
              !Files.exists(BftFinality.defaultReplicaSetPath(home)) =>
            Right(None)
          case None if Files.exists(BftFinality.defaultReplicaSetPath(home)) =>
            // Amendment convenience: default replaces = current tip digest.
            BftFinality.loadReplicaSet(BftFinality.defaultReplicaSetPath(home)).map(m => Some(m.digest))
          case None => Right(None)
        draft <- ReplicaSetManifest.of(
          ids.map(id => id -> pks(id)),
          replaces = replacesResolved,
          activationHeight = activationHeight)
        unsigned = draft.copy(seals = Nil, predecessorApprovals = Nil)
        _ <- clearContributions(home)
        _ <- DurableIo.writeAtomic(draftPath(home), Canon.encode(unsigned.canon))
      yield unsigned

  private def clearContributions(home: Path): Either[String, Unit] =
    try
      List(sealsDir(home), approvalsDir(home)).foreach { dir =>
        listCanonFiles(dir).foreach(Files.deleteIfExists(_))
      }
      Right(())
    catch case e: Exception => Left(s"ceremony: failed to clear contributions: ${e.getMessage}")

  def loadDraft(home: Path): Either[String, ReplicaSetManifest] =
    val p = draftPath(home)
    if !Files.exists(p) then Left(s"ceremony: missing draft at $p (run assemble first)")
    else
      Canon.decode(Files.readAllBytes(p)).flatMap(ReplicaSetManifest.fromCanon).map { m =>
        m.copy(seals = Nil, predecessorApprovals = Nil)
      }

  /** Member seal over the draft body (new-set endorsement). */
  def sealMember(
      home: Path,
      name: String,
      secret: Option[Array[Byte]],
  ): Either[String, Path] =
    for
      draft <- loadDraft(home)
      _ <- Either.cond(draft.authorities.contains(name), (),
        s"ceremony: '$name' is not a member of the draft")
      kp <- Keystore.load(home, name, secret)
      _ <- Either.cond(kp.publicBytes == draft.authorities(name), (),
        s"ceremony: local public key for '$name' does not match draft")
      seal = kp.sign(Canon.encode(draft.bodyCanon))
      bodyDig = Digest.of(draft.bodyCanon)
      dest = sealPath(home, name)
      _ <- DurableIo.writeAtomic(
        dest, Canon.encode(contributionCanon(SealTag, name, seal, bodyDig)))
    yield dest

  /** Import a peer seal contribution into `ceremony/seals/<id>.canon`. */
  def importSeal(home: Path, from: Path): Either[String, (String, Path)] =
    importContribution(home, from, SealTag, sealsDir(home))

  /** Predecessor approval over the draft body (amendments only). */
  def approve(
      home: Path,
      name: String,
      secret: Option[Array[Byte]],
  ): Either[String, Path] =
    for
      draft <- loadDraft(home)
      _ <- Either.cond(draft.replaces.isDefined, (),
        "ceremony: approve is only for amendments (draft has no replaces)")
      tip <- BftFinality.loadReplicaSet(BftFinality.defaultReplicaSetPath(home))
      _ <- Either.cond(tip.authorities.contains(name), (),
        s"ceremony: '$name' is not in the predecessor set")
      kp <- Keystore.load(home, name, secret)
      _ <- Either.cond(kp.publicBytes == tip.authorities(name), (),
        s"ceremony: local public key for '$name' does not match predecessor set")
      seal = kp.sign(Canon.encode(draft.bodyCanon))
      bodyDig = Digest.of(draft.bodyCanon)
      dest = approvalPath(home, name)
      _ <- DurableIo.writeAtomic(
        dest, Canon.encode(contributionCanon(ApprovalTag, name, seal, bodyDig)))
    yield dest

  /** Import a predecessor approval into `ceremony/approvals/<id>.canon`. */
  def importApproval(home: Path, from: Path): Either[String, (String, Path)] =
    importContribution(home, from, ApprovalTag, approvalsDir(home))

  private def importContribution(
      home: Path,
      from: Path,
      tag: String,
      destDir: Path,
  ): Either[String, (String, Path)] =
    if !Files.exists(from) then Left(s"missing contribution file $from")
    else
      for
        draft <- loadDraft(home)
        expected = Digest.of(draft.bodyCanon)
        c <- Canon.decode(Files.readAllBytes(from))
        parsed <- parseContribution(tag, c)
        (id, seal, dig) = parsed
        _ <- Either.cond(dig == expected, (),
          s"ceremony: contribution body ${dig.short} != draft ${expected.short}")
        dest = destDir.resolve(s"$id.canon")
        _ <- DurableIo.writeAtomic(dest, Canon.encode(contributionCanon(tag, id, seal, dig)))
      yield (id, dest)

  def exportDraft(home: Path, out: Path): Either[String, Path] =
    for
      draft <- loadDraft(home)
      _ <- DurableIo.writeAtomic(out, Canon.encode(draft.canon))
    yield out

  def importDraft(home: Path, from: Path): Either[String, ReplicaSetManifest] =
    if !Files.exists(from) then Left(s"missing draft file $from")
    else
      for
        c <- Canon.decode(Files.readAllBytes(from))
        m <- ReplicaSetManifest.fromCanon(c)
        unsigned = m.copy(seals = Nil, predecessorApprovals = Nil)
        _ <- clearContributions(home)
        _ <- DurableIo.writeAtomic(draftPath(home), Canon.encode(unsigned.canon))
      yield unsigned

  private def loadContributions(
      dir: Path,
      tag: String,
      expectedBody: Digest,
  ): Either[String, List[(String, Vector[Byte])]] =
    listCanonFiles(dir).foldLeft[Either[String, List[(String, Vector[Byte])]]](Right(Nil)) { (acc, p) =>
      acc.flatMap { xs =>
        Canon.decode(Files.readAllBytes(p)).flatMap(parseContribution(tag, _)).flatMap {
          case (id, seal, dig) =>
            if dig != expectedBody then
              Left(s"ceremony: $tag from '$id' is for body ${dig.short}, draft is ${expectedBody.short}")
            else Right(xs :+ (id -> seal))
        }
      }
    }

  /** Collect seals (+ predecessor approvals), verify, and persist tip + history. */
  def commit(home: Path): Either[String, ReplicaSetManifest] =
    for
      draft <- loadDraft(home)
      bodyDig = Digest.of(draft.bodyCanon)
      seals <- loadContributions(sealsDir(home), SealTag, bodyDig)
      approvals <- loadContributions(approvalsDir(home), ApprovalTag, bodyDig)
      missing = draft.ids.toSet -- seals.map(_._1).toSet
      _ <- Either.cond(missing.isEmpty, (),
        s"ceremony: missing member seals for ${missing.toList.sorted.mkString(",")}")
      withSeals = draft.copy(seals = seals.sortBy(_._1))
      withApprovals <-
        if draft.replaces.isEmpty then
          Either.cond(approvals.isEmpty, withSeals,
            "ceremony: genesis draft must not carry predecessor approvals")
        else
          ReplicaSetManifest.withPredecessorApprovals(withSeals, approvals)
      _ <- ReplicaSetManifest.verifySeals(withApprovals, Ed25519.verify)
      _ <- BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(home), withApprovals)
    yield withApprovals

  /** Export tip + replay-verified history as a distributable bundle. */
  def exportBundle(home: Path, out: Path): Either[String, Path] =
    for
      tip <- BftFinality.loadReplicaSet(BftFinality.defaultReplicaSetPath(home))
      hist <- BftFinality.loadReplicaSetHistory(home)
      _ <- DurableIo.writeAtomic(out, Canon.encode(bundleCanon(tip, hist.manifests)))
    yield out

  /** Install a bundle (or a bare sealed manifest) into this home. */
  def installBundle(home: Path, from: Path): Either[String, ReplicaSetManifest] =
    if !Files.exists(from) then Left(s"ceremony: missing install source $from")
    else
      Canon.decode(Files.readAllBytes(from)).flatMap {
        case c @ Canon.CTag(BundleTag, _) =>
          parseBundle(c).flatMap { parsed =>
            val (tip, hist) = parsed
            for
              _ <- DurableIo.writeConsensus(
                BftFinality.defaultReplicaSetHistoryPath(home),
                Canon.encode(Canon.CList(hist.map(_.canon))))
              _ <- DurableIo.writeConsensus(
                BftFinality.defaultReplicaSetPath(home),
                Canon.encode(tip.canon))
              _ <- BftFinality.loadReplicaSetHistory(home) // re-verify on disk
            yield tip
          }
        case c =>
          // Bare sealed manifest — saveReplicaSet enforces transition rules.
          ReplicaSetManifest.fromCanon(c).flatMap { m =>
            BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(home), m).map(_ => m)
          }
      }

  def status(home: Path): Either[String, String] =
    val tip =
      if Files.exists(BftFinality.defaultReplicaSetPath(home)) then
        BftFinality.loadReplicaSet(BftFinality.defaultReplicaSetPath(home))
          .map(m => s"tip=${m.digest.short} n=${m.n} ids=${m.ids.mkString(",")}")
      else Right("(no replica-set.canon)")
    val draft =
      if Files.exists(draftPath(home)) then
        loadDraft(home).map { d =>
          val body = Digest.of(d.bodyCanon).short
          s"draft body=$body replaces=${d.replaces.map(_.short).getOrElse("none")} " +
            s"activation=${d.activationHeight} ids=${d.ids.mkString(",")}"
        }
      else Right("(no draft)")
    val pks = loadImportedPubkeys(home).map(m =>
      if m.isEmpty then "(no imported pubkeys)"
      else s"pubkeys=${m.keys.toList.sorted.mkString(",")}")
    for
      t <- tip
      d <- draft
      p <- pks
    yield s"$t\n$d\n$p"
