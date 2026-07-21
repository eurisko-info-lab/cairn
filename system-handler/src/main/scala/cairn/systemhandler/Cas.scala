package cairn.systemhandler

import cairn.kernel.*
import cairn.core.{Delta, LangMigration, Merge, Module, SemanticRepository}
import cairn.systeminterface.Cas
import java.nio.file.{Files, Path}

/** MIGRATION-PLAN.md Phase 1: the System Handler half of the old
  * `workbench.Cas.scala` — concrete, effectful `Cas` implementations plus
  * the filesystem-backed branch-ref store. The pure `Cas` contract lives in
  * `system-interface`; `BranchManifest` (pure data, its validity is a
  * Kernel concern) lives in `kernel`.
  */
final class MemCas extends Cas:
  private val store = scala.collection.mutable.Map[String, Array[Byte]]()
  def putBytes(bs: Array[Byte]): Digest =
    val d = Digest.ofBytes(bs); store(d.hex) = bs; d
  def getBytes(d: Digest): Either[String, Array[Byte]] =
    store.get(d.hex).toRight(s"blob ${d.short} not in CAS")
  def contains(d: Digest): Boolean = store.contains(d.hex)

/** Disk CAS: `<root>/objects/ab/cdef...`; digest re-verified on read (corruption check). */
final class DiskCas(root: Path) extends Cas:
  private def pathOf(d: Digest): Path = root.resolve("objects").resolve(d.hex.take(2)).resolve(d.hex.drop(2))
  def putBytes(bs: Array[Byte]): Digest =
    val d = Digest.ofBytes(bs)
    val p = pathOf(d)
    if !Files.exists(p) then
      Files.createDirectories(p.getParent)
      Files.write(p, bs)
    d
  def getBytes(d: Digest): Either[String, Array[Byte]] =
    val p = pathOf(d)
    if !Files.exists(p) then Left(s"blob ${d.short} not in CAS at $root")
    else
      val bs = Files.readAllBytes(p)
      val actual = Digest.ofBytes(bs)
      if actual == d then Right(bs)
      else Left(s"CAS corruption: blob ${d.short} hashes to ${actual.short}")
  def contains(d: Digest): Boolean = Files.exists(pathOf(d))

  // -- digest agility (M4): non-default algorithms live in sibling stores --
  private def pathOfAlgo(algo: String, hex: String): Path =
    root.resolve(s"objects-$algo").resolve(hex.take(2)).resolve(hex.drop(2))
  /** Store under an explicit algorithm; returns the self-describing key. */
  def putBytesAlgo(algo: String, bs: Array[Byte]): String =
    if algo == "sha256" then HashAlgo.render(algo, putBytes(bs).hex)
    else
      val hex = HashAlgo.hash(algo, bs)
      val p = pathOfAlgo(algo, hex)
      if !Files.exists(p) then { Files.createDirectories(p.getParent); Files.write(p, bs) }
      HashAlgo.render(algo, hex)
  /** Read by self-describing key (`algo:hex`), verifying under that algorithm. */
  def getBytesKey(key: String): Either[String, Array[Byte]] =
    HashAlgo.parse(key).flatMap { (algo, hex) =>
      if algo == "sha256" then Digest.parse(hex).flatMap(getBytes)
      else
        val p = pathOfAlgo(algo, hex)
        if !Files.exists(p) then Left(s"blob $key not in CAS at $root")
        else
          val bs = Files.readAllBytes(p)
          if HashAlgo.hash(algo, bs) == hex then Right(bs)
          else Left(s"CAS corruption on $key") }

/** Authorized CAS put/get over a store — same authorize →
  * [[AuthorizedEffect]] → perform spine as Filesystem / Workspace.
  */
object CasEffects:
  private val iface = EffectMeta.cas

  private def ctorName(req: Cas.Request): String = req match
    case Cas.Request.Put(_) => "put"
    case Cas.Request.Get(_) => "get"

  private def resourcePath(req: Cas.Request): String = req match
    case Cas.Request.Put(a)  => a.digest.hex
    case Cas.Request.Get(d)  => d.hex

  def intent(req: Cas.Request): (Effects.ActionKey, Authority.Resource) =
    (iface.keyFor(ctorName(req)).get, iface.resource.at(resourcePath(req)))

  def run(store: Cas, req: Cas.Request, ctx: EffectContext): Either[Cas.Error, Cas.Response] =
    val (action, resource) = intent(req)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) => perform(store, req, auth)

  def perform(store: Cas, req: Cas.Request, auth: AuthorizedEffect): Either[Cas.Error, Cas.Response] =
    val (action, resource) = intent(req)
    if !auth.covers(action, resource) then Left(Cas.Error.Io("authorized effect does not cover request"))
    else
      try req match
        case Cas.Request.Put(artifact) =>
          Right(Cas.Response.Key(store.put(artifact)))
        case Cas.Request.Get(digest) =>
          store.getByDigest(digest) match
            case Right(a) => Right(Cas.Response.Stored(a))
            case Left(_)  => Left(Cas.Error.Missing(digest))
      catch case e: Exception => Left(Cas.Error.Io(e.getMessage))

  /** Convenience: authorize + put an artifact; returns its typed key. */
  def put(store: Cas, artifact: Artifact, ctx: EffectContext): Either[Cas.Error, TypedKey] =
    run(store, Cas.Request.Put(artifact), ctx).flatMap {
      case Cas.Response.Key(k) => Right(k)
      case other               => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  /** Convenience: authorize + get an artifact by digest. */
  def get(store: Cas, digest: Digest, ctx: EffectContext): Either[Cas.Error, Artifact] =
    run(store, Cas.Request.Get(digest), ctx).flatMap {
      case Cas.Response.Stored(a) => Right(a)
      case other                  => Left(Cas.Error.Io(s"unexpected response: $other"))
    }

  /** Authorize put at the content digest, then store raw bytes (Sync path). */
  def putBytes(store: Cas, bs: Array[Byte], ctx: EffectContext): Either[Cas.Error, Digest] =
    val d = Digest.ofBytes(bs)
    ctx.authorize(iface.actionKey("put"), iface.resource.at(d.hex)) match
      case Left(err) => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) =>
        if !auth.covers(iface.actionKey("put"), iface.resource.at(d.hex)) then
          Left(Cas.Error.Io("authorized effect does not cover request"))
        else
          try Right(store.putBytes(bs))
          catch case e: Exception => Left(Cas.Error.Io(e.getMessage))

  /** Authorize get at `digest`, then read raw bytes (Sync path). */
  def getBytes(store: Cas, digest: Digest, ctx: EffectContext): Either[Cas.Error, Array[Byte]] =
    ctx.authorize(iface.actionKey("get"), iface.resource.at(digest.hex)) match
      case Left(err) => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) =>
        if !auth.covers(iface.actionKey("get"), iface.resource.at(digest.hex)) then
          Left(Cas.Error.Io("authorized effect does not cover request"))
        else store.getBytes(digest).left.map(_ => Cas.Error.Missing(digest))

/** Named branch refs over a CAS; ref file stores the manifest digest.
  *
  * Merge-aware (M17): [[merge]] / [[mergeBranches]] run
  * [[cairn.core.SemanticRepository.integrate]] then either advance the target
  * head or persist the conflict artifact. [[commitTip]] persists the
  * ValidatedChangeSet alongside the tip (tip sidecar + append-only history
  * log) so everyday merge need not pass change histories explicitly.
  *
  * Accept is local-only by default: advancing the branch ref does **not**
  * publish to the ledger. Call [[publishHead]] explicitly (or pass
  * `publish = Some(...)` to [[merge]] / [[mergeBranches]]) when a ledger
  * `SetBranchHead` is wanted.
  */
final class Branches(cas: Cas, refsDir: Path):
  private def refPath(branch: String): Path =
    require(branch.nonEmpty && branch.forall(c => c.isLetterOrDigit || c == '-' || c == '_'), s"bad branch name '$branch'")
    refsDir.resolve(branch)

  /** Sidecar ref: digest of the ValidatedChangeSet that produced the tip. */
  private def changeRefPath(branch: String): Path =
    refsDir.resolve(s"$branch.change")

  /** Append-only log of every ValidatedChangeSet digest for `branch`. */
  private def changeHistoryPath(branch: String): Path =
    refsDir.resolve(s"$branch.changes")

  private def isSidecar(name: String): Boolean =
    name.endsWith(".change") || name.endsWith(".changes")

  private def persistChange(branch: String, vcsKey: TypedKey): Unit =
    Files.createDirectories(refsDir)
    Files.writeString(changeRefPath(branch), vcsKey.valueHash.hex)
    val hist = changeHistoryPath(branch)
    val prev = if Files.exists(hist) then Files.readString(hist) else ""
    val line = vcsKey.valueHash.hex + "\n"
    Files.writeString(hist, prev + line)

  private def loadVcs(digest: Digest): Either[String, Delta.ValidatedChangeSet] =
    cas.getByDigest(digest).flatMap { a =>
      if a.kind != ArtifactKind.ChangeSet then Left(s"digest ${digest.short} is ${a.kind.name}, not a change-set")
      else Right(Delta.ValidatedChangeSet.fromCanon(a.body))
    }

  def load(branch: String): BranchManifest =
    val p = refPath(branch)
    if !Files.exists(p) then BranchManifest(branch, None, Nil)
    else
      val d = Digest(Files.readString(p).trim)
      cas.getByDigest(d).map(a => BranchManifest.fromCanon(a.body)).fold(e => throw RuntimeException(e), identity)

  /** Append: new head goes to history head; manifest itself stored in CAS. */
  def advance(branch: String, newHead: TypedKey): BranchManifest =
    val cur = load(branch)
    val next = BranchManifest(branch, Some(newHead), cur.head.toList ++ cur.history)
    val key = cas.put(next.artifact)
    Files.createDirectories(refsDir)
    Files.writeString(refPath(branch), key.valueHash.hex)
    next

  def list(): List[String] =
    if !Files.exists(refsDir) then Nil
    else
      Files.list(refsDir).map(_.getFileName.toString).toArray.toList.map(_.toString)
        .filterNot(isSidecar).sorted

  /** Load the module at a branch head (heads are [[ArtifactKind.Ir]] modules). */
  def headModule(branch: String): Either[String, Module] =
    load(branch).head.toRight(s"branch '$branch' has no head").flatMap { key =>
      cas.get(key).flatMap { a =>
        if a.kind != ArtifactKind.Ir then Left(s"branch '$branch' head is ${a.kind.name}, not a module")
        else Right(Module.fromCanon(a.body))
      }
    }

  /** Seed or advance a branch to a module tip; returns the new manifest. */
  def commitModule(branch: String, module: Module): BranchManifest =
    advance(branch, cas.put(module.artifact))

  /** Persist a semantic tip: store base + ValidatedChangeSet + tip module,
    * record provenance, write the change sidecar + history log, and advance.
    */
  def commitTip(branch: String, language: Digest, tip: SemanticRepository.Tip): BranchManifest =
    cas.put(tip.base.artifact)
    val vcs = Delta.ValidatedChangeSet(language, tip.baseDigest, tip.change, tip.tipDigest)
    val vcsKey = cas.put(vcs.artifact)
    val modKey = cas.put(tip.tip.artifact)
    Provenance.record(cas, tip.tipDigest, List(tip.baseDigest, vcsKey.valueHash), "semantic-commit")
    persistChange(branch, vcsKey)
    advance(branch, modKey)

  /** Load the tip ValidatedChangeSet recorded by [[commitTip]] / merge accept. */
  def loadChange(branch: String): Either[String, Delta.ValidatedChangeSet] =
    val p = changeRefPath(branch)
    if !Files.exists(p) then Left(s"branch '$branch' has no persisted change (commit via commitTip)")
    else loadVcs(Digest(Files.readString(p).trim))

  /** Full change history for `branch` (oldest → newest), from the `.changes` log.
    * Falls back to the tip sidecar alone when the log is absent (pre-history tips).
    */
  def loadChangeHistory(branch: String): Either[String, List[Delta.ValidatedChangeSet]] =
    val hist = changeHistoryPath(branch)
    if Files.exists(hist) then
      val digests = Files.readString(hist).linesIterator.map(_.trim).filter(_.nonEmpty).map(Digest(_)).toList
      digests.foldLeft[Either[String, List[Delta.ValidatedChangeSet]]](Right(Nil)) { (acc, d) =>
        acc.flatMap(xs => loadVcs(d).map(xs :+ _))
      }
    else loadChange(branch).map(List(_))

  /** Reconstruct a [[SemanticRepository.Tip]] from the tip sidecar + head module. */
  def loadTip(branch: String): Either[String, SemanticRepository.Tip] =
    for
      vcs <- loadChange(branch)
      tipMod <- headModule(branch)
      baseArt <- cas.getByDigest(vcs.base)
      base <-
        if baseArt.kind != ArtifactKind.Ir then Left(s"base ${vcs.base.short} is not a module")
        else Right(Module.fromCanon(baseArt.body))
      _ <- Either.cond(tipMod.digest == vcs.result, (),
        s"branch '$branch' head ${tipMod.digest.short} does not match change result ${vcs.result.short}")
    yield SemanticRepository.Tip(base, tipMod, vcs.change)

  /** Optional ledger publish after a local accept. */
  final case class Publish(
      node: Node,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
  )

  private def maybePublish(
      into: String,
      outcome: Either[Merge.Conflict, BranchManifest],
      publish: Option[Publish],
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    (outcome, publish) match
      case (Right(_), Some(p)) =>
        publishHead(into, p.node, p.authority, p.authorities).map(_ => outcome)
      case _ => Right(outcome)

  /** Semantic merge into `into`: integrate two change histories relative to
    * `base`, persist the ValidatedChangeSet + provenance, and advance `into`
    * on success. On conflict, the conflict artifact is stored in CAS and the
    * branch head is left unchanged. Ledger publish is opt-in via `publish`.
    *
    * @return `Right(manifest)` on accept, `Left(conflict)` on overlap.
    */
  def merge(
      language: ComposedLanguage,
      into: String,
      base: Module,
      changeOurs: Cst,
      changeTheirs: Cst,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
      publish: Option[Publish] = None,
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    SemanticRepository.integrate(language, base, changeOurs, changeTheirs, migration).flatMap {
      case SemanticRepository.Outcome.Conflicted(conflict) =>
        cas.put(conflict.artifact)
        Right(Left(conflict))
      case SemanticRepository.Outcome.Accepted(module, vcs, _, _) =>
        val vcsKey = cas.put(vcs.artifact)
        val modKey = cas.put(module.artifact)
        Provenance.record(cas, module.digest,
          List(base.digest, vcsKey.valueHash), "semantic-merge")
        persistChange(into, vcsKey)
        maybePublish(into, Right(advance(into, modKey)), publish)
    }

  /** Everyday merge: load tip changes persisted by [[commitTip]] on
    * `ours` / `theirs` (shared base from the VCS records). No explicit Cst.
    * Ledger publish remains opt-in via `publish` (default: local accept only).
    */
  def mergeBranches(
      language: ComposedLanguage,
      into: String,
      ours: String,
      theirs: String,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
      publish: Option[Publish] = None,
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    for
      tipA <- loadTip(ours)
      tipB <- loadTip(theirs)
      _ <- Either.cond(tipA.baseDigest == tipB.baseDigest, (),
        s"branch tips do not share a base: ${tipA.baseDigest.short} vs ${tipB.baseDigest.short}")
      out <- merge(language, into, tipA.base, tipA.change, tipB.change, migration, publish)
    yield out

  /** Opt-in ledger publication of an accepted branch head: `PublishArtifact`
    * then `SetBranchHead`. Not called automatically on merge accept.
    */
  def publishHead(
      branch: String,
      node: Node,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
  ): Either[String, Block] =
    load(branch).head.toRight(s"branch '$branch' has no head").flatMap { key =>
      node.append(authority, authorities, List(
        authority.signTx(Tx.PublishArtifact(key)),
        authority.signTx(Tx.SetBranchHead(branch, key))))
    }
