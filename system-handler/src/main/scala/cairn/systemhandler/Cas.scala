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

/** Named branch refs over a CAS; ref file stores the manifest digest.
  *
  * Merge-aware (M17): [[merge]] / [[mergeBranches]] run
  * [[cairn.core.SemanticRepository.integrate]] then either advance the target
  * head or persist the conflict artifact. [[commitTip]] persists the
  * ValidatedChangeSet alongside the tip so everyday merge need not pass
  * change histories explicitly.
  */
final class Branches(cas: Cas, refsDir: Path):
  private def refPath(branch: String): Path =
    require(branch.nonEmpty && branch.forall(c => c.isLetterOrDigit || c == '-' || c == '_'), s"bad branch name '$branch'")
    refsDir.resolve(branch)

  /** Sidecar ref: digest of the ValidatedChangeSet that produced the tip. */
  private def changeRefPath(branch: String): Path =
    refsDir.resolve(s"$branch.change")

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
        .filterNot(_.endsWith(".change")).sorted

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
    * record provenance, write the change sidecar, and advance the branch.
    */
  def commitTip(branch: String, language: Digest, tip: SemanticRepository.Tip): BranchManifest =
    cas.put(tip.base.artifact)
    val vcs = Delta.ValidatedChangeSet(language, tip.baseDigest, tip.change, tip.tipDigest)
    val vcsKey = cas.put(vcs.artifact)
    val modKey = cas.put(tip.tip.artifact)
    Provenance.record(cas, tip.tipDigest, List(tip.baseDigest, vcsKey.valueHash), "semantic-commit")
    Files.createDirectories(refsDir)
    Files.writeString(changeRefPath(branch), vcsKey.valueHash.hex)
    advance(branch, modKey)

  /** Load the ValidatedChangeSet recorded by [[commitTip]] for `branch`. */
  def loadChange(branch: String): Either[String, Delta.ValidatedChangeSet] =
    val p = changeRefPath(branch)
    if !Files.exists(p) then Left(s"branch '$branch' has no persisted change (commit via commitTip)")
    else
      val d = Digest(Files.readString(p).trim)
      cas.getByDigest(d).flatMap { a =>
        if a.kind != ArtifactKind.ChangeSet then Left(s"change ref for '$branch' is ${a.kind.name}, not a change-set")
        else Right(Delta.ValidatedChangeSet.fromCanon(a.body))
      }

  /** Semantic merge into `into`: integrate two change histories relative to
    * `base`, persist the ValidatedChangeSet + provenance, and advance `into`
    * on success. On conflict, the conflict artifact is stored in CAS and the
    * branch head is left unchanged.
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
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    SemanticRepository.integrate(language, base, changeOurs, changeTheirs, migration).map {
      case SemanticRepository.Outcome.Conflicted(conflict) =>
        cas.put(conflict.artifact)
        Left(conflict)
      case SemanticRepository.Outcome.Accepted(module, vcs, _, _) =>
        val vcsKey = cas.put(vcs.artifact)
        val modKey = cas.put(module.artifact)
        Provenance.record(cas, module.digest,
          List(base.digest, vcsKey.valueHash), "semantic-merge")
        Files.createDirectories(refsDir)
        Files.writeString(changeRefPath(into), vcsKey.valueHash.hex)
        Right(advance(into, modKey))
    }

  /** Everyday merge: load change histories persisted by [[commitTip]] on
    * `ours` / `theirs` (shared base from the VCS records). No explicit Cst.
    */
  def mergeBranches(
      language: ComposedLanguage,
      into: String,
      ours: String,
      theirs: String,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    for
      vcsA <- loadChange(ours)
      vcsB <- loadChange(theirs)
      _ <- Either.cond(vcsA.base == vcsB.base, (),
        s"branch tips do not share a base: ${vcsA.base.short} vs ${vcsB.base.short}")
      baseArt <- cas.getByDigest(vcsA.base)
      base <-
        if baseArt.kind != ArtifactKind.Ir then Left(s"base ${vcsA.base.short} is not a module")
        else Right(Module.fromCanon(baseArt.body))
      out <- merge(language, into, base, vcsA.change, vcsB.change, migration)
    yield out

  /** Optionally publish the accepted branch head via ledger: `PublishArtifact`
    * then `SetBranchHead` (heads must be published before they can be set).
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
