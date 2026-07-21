package cairn.systemhandler

import cairn.kernel.*
import cairn.core.{ChangeAlgebra, Delta, LangMigration, Merge, Module, SemanticRepository}
import cairn.systeminterface.Cas
import cairn.systeminterface.Filesystem as Fs
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

/** Authorized CAS put/get/contains over a store — same authorize →
  * [[AuthorizedEffect]] → perform spine as Filesystem / Workspace.
  * Admin ops (fsck/gc/stats) live on [[CasAdminEffects]] (path-rooted).
  */
object CasEffects:
  private val iface = EffectMeta.cas

  private def ctorName(req: Cas.Request): String = req match
    case Cas.Request.Put(_)       => "put"
    case Cas.Request.Get(_)       => "get"
    case Cas.Request.Contains(_)  => "contains"
    case Cas.Request.Fsck(_)      => "fsck"
    case Cas.Request.Gc(_, _)     => "gc"
    case Cas.Request.Stats(_)     => "stats"

  private def resourcePath(req: Cas.Request): String = req match
    case Cas.Request.Put(a)       => a.digest.hex
    case Cas.Request.Get(d)       => d.hex
    case Cas.Request.Contains(d)  => d.hex
    case Cas.Request.Fsck(root)   => root
    case Cas.Request.Gc(root, _)  => root
    case Cas.Request.Stats(root)  => root

  def intent(req: Cas.Request): (Effects.ActionKey, Authority.Resource) =
    (iface.keyFor(ctorName(req)).get, iface.resource.at(resourcePath(req)))

  /** Store-backed requests only (`put` / `get` / `contains`). Admin requests
    * must go through [[CasAdminEffects]]. */
  def run(store: Cas, req: Cas.Request, ctx: EffectContext): Either[Cas.Error, Cas.Response] =
    req match
      case _: Cas.Request.Fsck | _: Cas.Request.Gc | _: Cas.Request.Stats =>
        Left(Cas.Error.Io(s"admin request ${ctorName(req)} requires CasAdminEffects"))
      case _ =>
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
        case Cas.Request.Contains(digest) =>
          Right(Cas.Response.Present(store.contains(digest)))
        case Cas.Request.Fsck(_) | Cas.Request.Gc(_, _) | Cas.Request.Stats(_) =>
          Left(Cas.Error.Io(s"admin request ${ctorName(req)} requires CasAdminEffects"))
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

  /** Authorize get-class existence check at `digest`. */
  def contains(store: Cas, digest: Digest, ctx: EffectContext): Either[Cas.Error, Boolean] =
    run(store, Cas.Request.Contains(digest), ctx).flatMap {
      case Cas.Response.Present(p) => Right(p)
      case other                   => Left(Cas.Error.Io(s"unexpected response: $other"))
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
  * CAS put/get go through [[CasEffects]] with [[ctx]]; refs-directory
  * read/write/mkdirs/list go through [[Filesystem]] with the same [[ctx]]
  * (use [[EffectContext.forBranches]] at the composition root).
  *
  * Merge-aware (M17): [[merge]] / [[mergeBranches]] run
  * [[cairn.core.SemanticRepository.integrate]] then either advance the target
  * head or persist the conflict artifact. [[commitTip]] persists the
  * ValidatedChangeSet alongside the tip (tip sidecar + append-only history
  * log) so everyday merge need not pass change histories explicitly.
  *
  * [[mergeBranches]] composes each side's full stacked history (not tip-only)
  * when multiple commits share a causal chain from a common base.
  *
  * Accept is local-only by default: advancing the branch ref does **not**
  * publish to the ledger. Call [[publishHead]] explicitly (or pass
  * `publish = Some(...)` to [[merge]] / [[mergeBranches]]) when a ledger
  * `SetBranchHead` is wanted.
  */
final class Branches(cas: Cas, refsDir: Path, ctx: EffectContext):
  private def casErr(e: Cas.Error): String = e match
    case Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
    case Cas.Error.Io(m)      => m

  private def fsAbs(p: Path): Fs.Path = Fs.Path(p.toAbsolutePath.normalize.toString)

  private def fsErr(e: Fs.Error): String = e match
    case Fs.Error.NotFound(p) => s"not found: ${p.value}"
    case Fs.Error.Io(m)       => m

  private def fsRun(req: Fs.Request): Either[String, Fs.Response] =
    Filesystem.run(req, ctx).left.map(fsErr)

  private def refsMkdirs(): Unit =
    fsRun(Fs.Request.Mkdirs(fsAbs(refsDir))).fold(e => throw RuntimeException(e), _ => ())

  private def refsExists(p: Path): Boolean =
    fsRun(Fs.Request.Exists(fsAbs(p))) match
      case Right(Fs.Response.Bool(b)) => b
      case Left(e)                    => throw RuntimeException(e)
      case other                      => throw RuntimeException(s"unexpected fs response: $other")

  private def refsRead(p: Path): String =
    fsRun(Fs.Request.Read(fsAbs(p))) match
      case Right(Fs.Response.Text(s)) => s
      case Left(e)                    => throw RuntimeException(e)
      case other                      => throw RuntimeException(s"unexpected fs response: $other")

  private def refsWrite(p: Path, content: String): Unit =
    fsRun(Fs.Request.Write(fsAbs(p), content)) match
      case Right(Fs.Response.Ok) => ()
      case Left(e)               => throw RuntimeException(e)
      case other                 => throw RuntimeException(s"unexpected fs response: $other")

  private def putArt(a: Artifact): TypedKey =
    CasEffects.put(cas, a, ctx).fold(e => throw RuntimeException(casErr(e)), identity)

  private def getByDigest(d: Digest): Either[String, Artifact] =
    CasEffects.get(cas, d, ctx).left.map(casErr)

  private def getKey(key: TypedKey): Either[String, Artifact] =
    getByDigest(key.valueHash).flatMap { a =>
      TypedKey.check(key, a.key).map(_ => a)
    }

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
    refsMkdirs()
    refsWrite(changeRefPath(branch), vcsKey.valueHash.hex)
    val hist = changeHistoryPath(branch)
    val prev = if refsExists(hist) then refsRead(hist) else ""
    val line = vcsKey.valueHash.hex + "\n"
    refsWrite(hist, prev + line)

  private def loadVcs(digest: Digest): Either[String, Delta.ValidatedChangeSet] =
    getByDigest(digest).flatMap { a =>
      if a.kind != ArtifactKind.ChangeSet then Left(s"digest ${digest.short} is ${a.kind.name}, not a change-set")
      else Right(Delta.ValidatedChangeSet.fromCanon(a.body))
    }

  /** Compose stacked ValidatedChangeSets into one change relative to the
    * oldest base. Entries must chain: `hist(i).result == hist(i+1).base`.
    */
  private def composeHistory(
      language: ComposedLanguage,
      hist: List[Delta.ValidatedChangeSet],
  ): Either[String, (Digest, Cst)] =
    if hist.isEmpty then Left("empty change history")
    else
      val broken = hist.zip(hist.tail).collectFirst {
        case (a, b) if a.result != b.base =>
          s"change history is not a causal chain: ${a.result.short} ↛ ${b.base.short}"
      }
      broken match
        case Some(err) => Left(err)
        case None =>
          val composed = hist.map(_.change).reduceLeft(ChangeAlgebra.compose(language, _, _))
          Right((hist.head.base, composed))

  def load(branch: String): BranchManifest =
    val p = refPath(branch)
    if !refsExists(p) then BranchManifest(branch, None, Nil)
    else
      val d = Digest(refsRead(p).trim)
      getByDigest(d).map(a => BranchManifest.fromCanon(a.body)).fold(e => throw RuntimeException(e), identity)

  /** Append: new head goes to history head; manifest itself stored in CAS. */
  def advance(branch: String, newHead: TypedKey): BranchManifest =
    val cur = load(branch)
    val next = BranchManifest(branch, Some(newHead), cur.head.toList ++ cur.history)
    val key = putArt(next.artifact)
    refsMkdirs()
    refsWrite(refPath(branch), key.valueHash.hex)
    next

  def list(): List[String] =
    if !refsExists(refsDir) then Nil
    else
      fsRun(Fs.Request.List(fsAbs(refsDir))) match
        case Right(Fs.Response.Entries(names)) => names.filterNot(isSidecar).sorted
        case Left(e)                           => throw RuntimeException(e)
        case other                             => throw RuntimeException(s"unexpected fs response: $other")

  /** Load the module at a branch head (heads are [[ArtifactKind.Ir]] modules). */
  def headModule(branch: String): Either[String, Module] =
    load(branch).head.toRight(s"branch '$branch' has no head").flatMap { key =>
      getKey(key).flatMap { a =>
        if a.kind != ArtifactKind.Ir then Left(s"branch '$branch' head is ${a.kind.name}, not a module")
        else Right(Module.fromCanon(a.body))
      }
    }

  /** Seed or advance a branch to a module tip; returns the new manifest. */
  def commitModule(branch: String, module: Module): BranchManifest =
    advance(branch, putArt(module.artifact))

  /** Persist a semantic tip: store base + ValidatedChangeSet + tip module,
    * record provenance, write the change sidecar + history log, and advance.
    */
  def commitTip(branch: String, language: Digest, tip: SemanticRepository.Tip): BranchManifest =
    putArt(tip.base.artifact)
    val vcs = Delta.ValidatedChangeSet(language, tip.baseDigest, tip.change, tip.tipDigest)
    val vcsKey = putArt(vcs.artifact)
    val modKey = putArt(tip.tip.artifact)
    Provenance.record(cas, tip.tipDigest, List(tip.baseDigest, vcsKey.valueHash), "semantic-commit", ctx)
      .fold(e => throw RuntimeException(casErr(e)), identity)
    persistChange(branch, vcsKey)
    advance(branch, modKey)

  /** Load the tip ValidatedChangeSet recorded by [[commitTip]] / merge accept. */
  def loadChange(branch: String): Either[String, Delta.ValidatedChangeSet] =
    val p = changeRefPath(branch)
    if !refsExists(p) then Left(s"branch '$branch' has no persisted change (commit via commitTip)")
    else loadVcs(Digest(refsRead(p).trim))

  /** Full change history for `branch` (oldest → newest), from the `.changes` log.
    * Falls back to the tip sidecar alone when the log is absent (pre-history tips).
    */
  def loadChangeHistory(branch: String): Either[String, List[Delta.ValidatedChangeSet]] =
    val hist = changeHistoryPath(branch)
    if refsExists(hist) then
      val digests = refsRead(hist).linesIterator.map(_.trim).filter(_.nonEmpty).map(Digest(_)).toList
      digests.foldLeft[Either[String, List[Delta.ValidatedChangeSet]]](Right(Nil)) { (acc, d) =>
        acc.flatMap(xs => loadVcs(d).map(xs :+ _))
      }
    else loadChange(branch).map(List(_))

  /** Reconstruct a [[SemanticRepository.Tip]] from the tip sidecar + head module. */
  def loadTip(branch: String): Either[String, SemanticRepository.Tip] =
    for
      vcs <- loadChange(branch)
      tipMod <- headModule(branch)
      baseArt <- getByDigest(vcs.base)
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
        putArt(conflict.artifact)
        Right(Left(conflict))
      case SemanticRepository.Outcome.Accepted(module, vcs, _, _) =>
        val vcsKey = putArt(vcs.artifact)
        val modKey = putArt(module.artifact)
        Provenance.record(cas, module.digest,
          List(base.digest, vcsKey.valueHash), "semantic-merge", ctx)
          .fold(e => throw RuntimeException(casErr(e)), identity)
        persistChange(into, vcsKey)
        maybePublish(into, Right(advance(into, modKey)), publish)
    }

  /** Everyday merge: compose full stacked histories from [[commitTip]] on
    * `ours` / `theirs` (shared oldest base). No explicit Cst.
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
      histA <- loadChangeHistory(ours)
      histB <- loadChangeHistory(theirs)
      stackedA <- composeHistory(language, histA)
      stackedB <- composeHistory(language, histB)
      (baseDigA, chA) = stackedA
      (baseDigB, chB) = stackedB
      _ <- Either.cond(baseDigA == baseDigB, (),
        s"branch histories do not share a base: ${baseDigA.short} vs ${baseDigB.short}")
      baseArt <- getByDigest(baseDigA)
      base <-
        if baseArt.kind != ArtifactKind.Ir then Left(s"base ${baseDigA.short} is not a module")
        else Right(Module.fromCanon(baseArt.body))
      out <- merge(language, into, base, chA, chB, migration, publish)
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
