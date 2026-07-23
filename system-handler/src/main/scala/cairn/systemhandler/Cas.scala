package cairn.systemhandler

import cairn.kernel.*
import cairn.core.{ChangeAlgebra, Delta, LangMigration, Merge, Module, PatchGraph, SemanticRepository}
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
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.Cas)

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

  def intent(req: Cas.Request, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    val i = iface(registry)
    (i.keyFor(ctorName(req)).get, i.resource.at(resourcePath(req)))

  /** Store-backed requests only (`put` / `get` / `contains`). Admin requests
    * must go through [[CasAdminEffects]]. */
  def run(store: Cas, req: Cas.Request, ctx: EffectContext): Either[Cas.Error, Cas.Response] =
    req match
      case _: Cas.Request.Fsck | _: Cas.Request.Gc | _: Cas.Request.Stats =>
        Left(Cas.Error.Io(s"admin request ${ctorName(req)} requires CasAdminEffects"))
      case _ =>
        val (action, resource) = intent(req, ctx.registry)
        ctx.authorize(action, resource) match
          case Left(err)   => Left(Cas.Error.Io(s"denied: $err"))
          case Right(auth) => perform(store, req, auth, ctx.registry)

  def perform(store: Cas, req: Cas.Request, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): Either[Cas.Error, Cas.Response] =
    val (action, resource) = intent(req, registry)
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
    val i = iface(ctx.registry)
    ctx.authorize(i.actionKey("put"), i.resource.at(d.hex)) match
      case Left(err) => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) =>
        if !auth.covers(i.actionKey("put"), i.resource.at(d.hex)) then
          Left(Cas.Error.Io("authorized effect does not cover request"))
        else
          try Right(store.putBytes(bs))
          catch case e: Exception => Left(Cas.Error.Io(e.getMessage))

  /** Authorize get at `digest`, then read raw bytes (Sync path). */
  def getBytes(store: Cas, digest: Digest, ctx: EffectContext): Either[Cas.Error, Array[Byte]] =
    val i = iface(ctx.registry)
    ctx.authorize(i.actionKey("get"), i.resource.at(digest.hex)) match
      case Left(err) => Left(Cas.Error.Io(s"denied: $err"))
      case Right(auth) =>
        if !auth.covers(i.actionKey("get"), i.resource.at(digest.hex)) then
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
  * head or persist the conflict artifact. [[commitTip]] accepts only
  * [[SemanticRepository.ValidatedTip]]; change-sets are replay-checked on load.
  * Causal digests are also written into [[BranchManifest]] (CAS-backed);
  * refs sidecars remain for compatibility.
  *
  * [[mergeBranches]] finds a causal LCA by shared module-result digests
  * (not only identical linear change prefixes), then merges divergent suffixes.
  *
  * Accepts use a journaled transactional path: CAS blobs → accept journal →
  * refs (history + tip + branch) → optional ledger publish → journal clear.
  * [[recoverPendingAccepts]] rolls forward interrupted accepts.
  * [[reclaimOrphanBlobs]] recovers then mark/sweeps CAS with [[liveCasRoots]]
  * (branch refs, change history, pending journals) — the reclaim path for
  * unreferenced accept blobs left by a crash before the journal was written.
  *
  * Ledger `SetBranchHead` remains opt-in via [[publishHead]] or
  * `publish = Some(...)` on merge.
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

  private def refsDelete(p: Path): Unit =
    if refsExists(p) then
      fsRun(Fs.Request.Delete(fsAbs(p))) match
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

  private def acceptJournalPath(branch: String): Path =
    refsDir.resolve(s"$branch.accepting")

  /** Sidecar: conflict artifact digest when merge left the head unchanged. */
  private def conflictRefPath(branch: String): Path =
    refsDir.resolve(s"$branch.conflict")

  private def isSidecar(name: String): Boolean =
    name.endsWith(".change") || name.endsWith(".changes") ||
      name.endsWith(".accepting") || name.endsWith(".conflict")

  /** Journaled accept intent (CAS digests + intended ref / ledger steps). */
  private final case class AcceptJournal(
      branch: String,
      moduleDigest: Digest,
      vcsDigest: Digest,
      parents: List[Digest],
      causalHistoryRoot: Option[Digest],
      historyAppend: Boolean,
      phase: String, // "cas" | "refs" | "publish" | "done"
      /** Provisional digests (provenance, tip base, …) protected as GC roots. */
      extras: List[Digest] = Nil,
  ):
    def rootDigests: List[Digest] =
      moduleDigest :: vcsDigest :: parents ++ causalHistoryRoot.toList ++ extras

    def encode: String =
      val lines = List(
        s"branch=$branch",
        s"module=${moduleDigest.hex}",
        s"vcs=${vcsDigest.hex}",
        s"parents=${parents.map(_.hex).mkString(",")}",
        s"causal=${causalHistoryRoot.map(_.hex).getOrElse("")}",
        s"historyAppend=$historyAppend",
        s"phase=$phase",
        s"extras=${extras.map(_.hex).mkString(",")}")
      lines.mkString("\n")

  private object AcceptJournal:
    def parse(text: String): Either[String, AcceptJournal] =
      val m = text.linesIterator.map(_.trim).filter(_.nonEmpty).flatMap { line =>
        line.split("=", 2) match
          case Array(k, v) => Some(k -> v)
          case _           => None
      }.toMap
      for
        branch <- m.get("branch").toRight("accept journal: missing branch")
        mod <- m.get("module").toRight("accept journal: missing module")
        vcs <- m.get("vcs").toRight("accept journal: missing vcs")
        parents = m.getOrElse("parents", "").split(',').toList.map(_.trim).filter(_.nonEmpty).map(Digest(_))
        causal = m.get("causal").filter(_.nonEmpty).map(Digest(_))
        histAppend = m.get("historyAppend").forall(_ != "false")
        phase <- m.get("phase").toRight("accept journal: missing phase")
        extras = m.getOrElse("extras", "").split(',').toList.map(_.trim).filter(_.nonEmpty).map(Digest(_))
      yield AcceptJournal(branch, Digest(mod), Digest(vcs), parents, causal, histAppend, phase, extras)

  private def writeJournal(j: AcceptJournal): Unit =
    refsMkdirs()
    refsWrite(acceptJournalPath(j.branch), j.encode)

  private def clearJournal(branch: String): Unit =
    refsDelete(acceptJournalPath(branch))

  private def clearConflict(branch: String): Unit =
    refsDelete(conflictRefPath(branch))

  /** Optional ledger publish after a local accept. */
  final case class Publish(
      node: Node,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
  )

  private def applyRefs(j: AcceptJournal): BranchManifest =
    val modArt = getByDigest(j.moduleDigest).fold(e => throw RuntimeException(e), identity)
    val vcsArt = getByDigest(j.vcsDigest).fold(e => throw RuntimeException(e), identity)
    if j.historyAppend then persistChange(j.branch, vcsArt.key)
    else
      refsMkdirs()
      refsWrite(changeRefPath(j.branch), vcsArt.key.valueHash.hex)
    advance(
      j.branch,
      modArt.key,
      acceptedChange = Some(vcsArt.key.valueHash),
      parents = j.parents,
      causalHistoryRoot = j.causalHistoryRoot)

  /** All-or-nothing accept: CAS → journal → refs → optional ledger → clear.
    * On ledger failure after refs, journal stays at phase=publish for recovery.
    *
    * If `branch` carries a pending [[Merge.Conflict]] ref (`conflictRefPath`),
    * this accept is treated as resolving it: the conflict's digest is folded into
    * this accept's provenance inputs, so the resolution is not just an ordinary
    * commit that happens to supersede the conflict marker — [[Provenance.why]] on
    * the resulting head surfaces the resolved conflict as a direct lineage input,
    * permanently, even after [[clearConflict]] removes the branch's live ref.
    */
  private def transactionalAccept(
      branch: String,
      module: Module,
      vcs: Delta.ValidatedChangeSet,
      parents: List[Digest],
      causalHistoryRoot: Option[Digest],
      publish: Option[Publish],
      provenanceParents: List[Digest],
      provenanceTool: String,
      historyAppend: Boolean = true,
      extraPuts: List[Artifact] = Nil,
  ): Either[String, BranchManifest] =
    if module.digest != vcs.result then
      Left(s"accept rejected: module ${module.digest.short} ≠ validated change result ${vcs.result.short}")
    else
      val resolvedConflict =
        if refsExists(conflictRefPath(branch)) then Some(Digest(refsRead(conflictRefPath(branch)).trim))
        else None
      val extraKeys = extraPuts.map(putArt)
      val vcsKey = putArt(vcs.artifact)
      val modKey = putArt(module.artifact)
      val provDig =
        Provenance.record(
          cas, module.digest,
          provenanceParents ++ resolvedConflict.toList :+ vcsKey.valueHash,
          provenanceTool, ctx)
          .fold(e => throw RuntimeException(casErr(e)), identity)
      val extras = extraKeys.map(_.valueHash) :+ provDig
      var journal = AcceptJournal(
        branch, modKey.valueHash, vcsKey.valueHash, parents, causalHistoryRoot, historyAppend, "cas", extras)
      writeJournal(journal)
      val manifest = applyRefs(journal)
      journal = journal.copy(phase = "refs")
      writeJournal(journal)
      publish match
        case None =>
          clearJournal(branch)
          clearConflict(branch)
          Right(manifest)
        case Some(p) =>
          journal = journal.copy(phase = "publish")
          writeJournal(journal)
          publishHead(branch, p.node, p.authority, p.authorities) match
            case Left(err) => Left(err)
            case Right(_) =>
              clearJournal(branch)
              clearConflict(branch)
              Right(manifest)

  /** Roll forward interrupted accepts (refs and/or ledger publish).
    * Phase=`cas` with missing journal blobs abandons the journal (orphans are
    * then reclaimable via [[reclaimOrphanBlobs]]); other failures stay Left.
    */
  def recoverPendingAccepts(
      publish: Option[Publish] = None
  ): Either[String, List[String]] =
    if !refsExists(refsDir) then Right(Nil)
    else
      fsRun(Fs.Request.List(fsAbs(refsDir))) match
        case Left(e) => Left(e)
        case Right(Fs.Response.Entries(names)) =>
          val pending = names.filter(_.endsWith(".accepting")).sorted
          pending.foldLeft[Either[String, List[String]]](Right(Nil)) { (acc, name) =>
            acc.flatMap { done =>
              val branch = name.stripSuffix(".accepting")
              val text = refsRead(acceptJournalPath(branch))
              AcceptJournal.parse(text).flatMap { j =>
                j.phase match
                  case "cas" =>
                    tryApplyRefs(j) match
                      case Right(_) =>
                        clearJournal(branch)
                        Right(done :+ branch)
                      case Left(err) if err.contains("not in CAS") || err.contains("Missing") =>
                        // Incomplete put before crash — drop journal; GC reclaims orphans.
                        clearJournal(branch)
                        Right(done :+ branch)
                      case Left(err) => Left(err)
                  case "refs" =>
                    clearJournal(branch)
                    Right(done :+ branch)
                  case "publish" =>
                    publish match
                      case Some(p) =>
                        publishHead(branch, p.node, p.authority, p.authorities).map { _ =>
                          clearJournal(branch)
                          done :+ branch
                        }
                      case None =>
                        Left(s"pending publish for '$branch' needs Publish credentials")
                  case other => Left(s"unknown accept journal phase '$other' for $branch")
              }
            }
          }
        case other => Left(s"unexpected fs response: $other")

  private def tryApplyRefs(j: AcceptJournal): Either[String, BranchManifest] =
    getByDigest(j.moduleDigest).flatMap { _ =>
      getByDigest(j.vcsDigest).map { _ =>
        applyRefs(j)
      }
    }

  /** Digests that must survive CAS GC: branch heads, change sidecars /
    * histories, conflict sidecars, pending accept-journal digests, and
    * causal digests reachable from stored [[BranchManifest]]s.
    */
  def liveCasRoots(): Either[String, Set[Digest]] =
    if !refsExists(refsDir) then Right(Set.empty)
    else
      fsRun(Fs.Request.List(fsAbs(refsDir))) match
        case Left(e) => Left(e)
        case Right(Fs.Response.Entries(names)) =>
          val roots = scala.collection.mutable.Set[Digest]()
          def addHex(s: String): Unit =
            val t = s.trim
            if t.nonEmpty then
              Digest.parse(t).foreach(roots += _)
          for name <- names do
            val p = refsDir.resolve(name)
            if name.endsWith(".accepting") then
              AcceptJournal.parse(refsRead(p)).foreach(j => j.rootDigests.foreach(roots += _))
            else if name.endsWith(".changes") then
              refsRead(p).linesIterator.foreach(addHex)
            else if name.endsWith(".change") || name.endsWith(".conflict") then
              addHex(refsRead(p))
            else if !name.contains('.') then
              val hex = refsRead(p).trim
              addHex(hex)
              Digest.parse(hex).foreach { d =>
                getByDigest(d).foreach { a =>
                  if a.kind == ArtifactKind.BranchManifest then
                    val m = BranchManifest.fromCanon(a.body)
                    m.head.foreach(k => roots += k.valueHash)
                    m.acceptedChange.foreach(roots += _)
                    m.changeHistory.foreach(roots += _)
                    m.causalHistoryRoot.foreach(roots += _)
                    m.conflictState.foreach(roots += _)
                    m.parents.foreach(roots += _)
                    m.certificates.foreach(roots += _)
                    m.history.foreach(k => roots += k.valueHash)
                }
              }
          Right(roots.toSet)
        case other => Left(s"unexpected fs response: $other")

  final case class ReclaimReport(
      recovered: List[String],
      gc: CasAdmin.GcReport,
      roots: Int,
  )

  /** Recover pending accepts, then mark/sweep the disk CAS using
    * [[liveCasRoots]] plus provenance records that cite those roots.
    * Call after a crash (or periodically) to drop unreferenced accept blobs.
    * Requires a [[DiskCas]] root path.
    */
  def reclaimOrphanBlobs(
      casRoot: Path,
      publish: Option[Publish] = None,
  ): Either[String, ReclaimReport] =
    recoverPendingAccepts(publish).flatMap { recovered =>
      liveCasRoots().flatMap { roots =>
        val withProv = roots ++ provenanceRoots(casRoot, roots)
        CasAdminEffects.gc(casRoot, withProv, ctx).left.map(casErr).map { report =>
          ReclaimReport(recovered, report, withProv.size)
        }
      }
    }

  /** Keep provenance artifacts whose output/inputs intersect live roots. */
  private def provenanceRoots(casRoot: Path, roots: Set[Digest]): Set[Digest] =
    CasAdmin.objectFiles(casRoot).flatMap { p =>
      val dig = Digest(p.getParent.getFileName.toString + p.getFileName.toString)
      Artifact.decode(Files.readAllBytes(p)).toOption.flatMap(Provenance.fromArtifact).collect {
        case r if roots.contains(r.output) || r.inputs.exists(roots.contains) => dig
      }
    }.toSet

  private def persistChange(branch: String, vcsKey: TypedKey): Unit =
    refsMkdirs()
    refsWrite(changeRefPath(branch), vcsKey.valueHash.hex)
    val hist = changeHistoryPath(branch)
    val prev = if refsExists(hist) then refsRead(hist) else ""
    val line = vcsKey.valueHash.hex + "\n"
    val last = prev.linesIterator.map(_.trim).filter(_.nonEmpty).toList.lastOption
    if last != Some(vcsKey.valueHash.hex) then refsWrite(hist, prev + line)

  /** Load + replay-check a change-set artifact against `language`. */
  private def loadVcs(
      language: ComposedLanguage, digest: Digest
  ): Either[String, Delta.ValidatedChangeSet] =
    getByDigest(digest).flatMap { a =>
      if a.kind != ArtifactKind.ChangeSet then
        Left(s"digest ${digest.short} is ${a.kind.name}, not a change-set")
      else
        val claim = Delta.ValidatedChangeSet.decodeClaim(a.body)
        getByDigest(claim.base).flatMap { baseArt =>
          if baseArt.kind != ArtifactKind.Ir then
            Left(s"base ${claim.base.short} is not a module")
          else Delta.ValidatedChangeSet.check(language, Module.fromCanon(baseArt.body), claim)
        }
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

  /** Longest identical linear prefix (change-equal). Retained for diagnostics. */
  private def commonAncestorPrefix(
      a: List[Delta.ValidatedChangeSet],
      b: List[Delta.ValidatedChangeSet],
  ): Int =
    a.zip(b).takeWhile((x, y) => x.result == y.result && x.base == y.base && x.change == y.change).length

  /** Causal LCA by shared module-result digests: tip-proximal shared state,
    * even when the change objects that produced it differ (diamond / alternate
    * paths). Returns indices into `a`/`b` of the last shared result, or
    * `(-1,-1)` when only a shared starting base applies.
    */
  private def causalLca(
      a: List[Delta.ValidatedChangeSet],
      b: List[Delta.ValidatedChangeSet],
  ): (Int, Int) =
    val bByResult: Map[Digest, Int] =
      b.zipWithIndex.map((v, i) => v.result -> i).toMap
    a.zipWithIndex.reverse.collectFirst {
      case (v, i) if bByResult.contains(v.result) => (i, bByResult(v.result))
    }.getOrElse((-1, -1))

  /** Prefer [[PatchGraph]] DAG LCA when change-set digests form an explicit
    * parent graph; fall back to module-result [[causalLca]].
    */
  private def patchAwareLca(
      a: List[Delta.ValidatedChangeSet],
      b: List[Delta.ValidatedChangeSet],
  ): (Int, Int) =
    val fallback = causalLca(a, b)
    if a.isEmpty || b.isEmpty then fallback
    else
      val entriesA = a.map(v => (v.artifact.digest, v.base, v.result))
      val entriesB = b.map(v => (v.artifact.digest, v.base, v.result))
      val merged =
        PatchGraph.Graph.linear(entriesA).flatMap { g0 =>
          entriesB.zipWithIndex.foldLeft[Either[String, PatchGraph.Graph]](Right(g0)) {
            case (Left(e), _) => Left(e)
            case (Right(g), ((id, base, result), i)) =>
              if g.contains(id) then Right(g)
              else
                val parent = if i == 0 then None else Some(entriesB(i - 1)._1)
                val parents = parent.toList.filter(g.contains)
                g.add(PatchGraph.Node(id, parents, base, result))
          }
        }
      merged match
        case Left(_) => fallback
        case Right(g) =>
          g.lca(a.last.artifact.digest, b.last.artifact.digest) match
            case None => fallback
            case Some(lcaId) =>
              val iA = a.indexWhere(_.artifact.digest == lcaId)
              val iB = b.indexWhere(_.artifact.digest == lcaId)
              if iA >= 0 && iB >= 0 then (iA, iB) else fallback

  def load(branch: String): BranchManifest =
    val p = refPath(branch)
    if !refsExists(p) then BranchManifest(branch, None, Nil)
    else
      val d = Digest(refsRead(p).trim)
      getByDigest(d).map(a => BranchManifest.fromCanon(a.body)).fold(e => throw RuntimeException(e), identity)

  /** Append: new head goes to history head; manifest itself stored in CAS.
    * Optional causal digests are recorded on the manifest. When
    * `acceptedChange` is set, it is appended to [[BranchManifest.changeHistory]]
    * (sidecars remain write-through caches of the same digests).
    */
  def advance(
      branch: String,
      newHead: TypedKey,
      acceptedChange: Option[Digest] = None,
      parents: List[Digest] = Nil,
      causalHistoryRoot: Option[Digest] = None,
      conflictState: Option[Digest] = None,
  ): BranchManifest =
    val cur = load(branch)
    val nextHistory = acceptedChange match
      case Some(d) if cur.changeHistory.lastOption.contains(d) => cur.changeHistory
      case Some(d) => cur.changeHistory :+ d
      case None => cur.changeHistory
    val next = BranchManifest(
      branch,
      Some(newHead),
      cur.head.toList ++ cur.history,
      causalHistoryRoot = causalHistoryRoot.orElse(cur.causalHistoryRoot),
      parents = if parents.nonEmpty then parents else cur.head.toList.map(_.valueHash),
      acceptedChange = acceptedChange.orElse(cur.acceptedChange),
      conflictState = conflictState,
      changeHistory = nextHistory,
      certificates = cur.certificates,
      primaryAncestor = cur.primaryAncestor,
      references = cur.references)
    val key = putArt(next.artifact)
    refsMkdirs()
    refsWrite(refPath(branch), key.valueHash.hex)
    next

  /** Persist domain ancestry on a branch ref (head / history unchanged). */
  private def storeManifest(m: BranchManifest): BranchManifest =
    val key = putArt(m.artifact)
    refsMkdirs()
    refsWrite(refPath(m.branch), key.valueHash.hex)
    m

  /** Pull a domain branch off the ledger trunk (`primary = None`) or off an
    * existing primary ancestor. Soft [[references]] name additional ancestors
    * (e.g. SDS primary=LAW, references=CHEMISTRY). Optionally seeds a tip via
    * [[importModule]]; domain fields survive subsequent advances.
    */
  private def primaryOf(known: Set[String])(name: String): Option[String] =
    if known.contains(name) then load(name).primaryAncestor else None

  def forkFrom(
      child: String,
      primary: Option[String],
      module: Option[Module] = None,
      references: List[String] = Nil,
  ): Either[String, BranchManifest] =
    val known = list().toSet
    val refs = references.distinct.filterNot(r => primary.contains(r) || r == child)
    val draft = BranchManifest(
      child, None, Nil, primaryAncestor = primary, references = refs)
    DomainBranch.wellFormed(draft, known, primaryOf(known)).flatMap { _ =>
      if known.contains(child) then
        val cur = load(child)
        if cur.primaryAncestor == primary && cur.references == refs then
          module match
            case None    => Right(cur)
            case Some(m) => Right(importModule(child, m))
        else
          Left(
            s"domain: branch '$child' already exists " +
              s"(primary=${cur.primaryAncestor}, refs=${cur.references})")
      else
        storeManifest(draft)
        module match
          case None    => Right(load(child))
          case Some(m) => Right(importModule(child, m))
    }

  /** Add a soft cross-domain reference (not the primary ancestor). */
  def referTo(branch: String, other: String): Either[String, BranchManifest] =
    val known = list().toSet
    if !known.contains(branch) then Left(s"domain: branch '$branch' does not exist")
    else if !known.contains(other) then Left(s"domain: reference '$other' is not a known branch")
    else
      val cur = load(branch)
      if cur.primaryAncestor.contains(other) then
        Left(s"domain: '$other' is already the primary ancestor of '$branch'")
      else if cur.references.contains(other) then Right(cur)
      else
        val next = cur.copy(references = cur.references :+ other)
        DomainBranch.wellFormed(next, known, primaryOf(known)).map(_ => storeManifest(next))

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

  /** Bootstrap / import acceptance: plant a module tip **without** a
    * [[Delta.ValidatedChangeSet]]. Ordinary semantic advancement must use
    * [[commitTip]] (ValidatedTip / ΔL only). This is not a silent bypass of
    * ΔL discipline — call sites that seed demo/base modules should prefer
    * [[importModule]] by name.
    */
  def importModule(branch: String, module: Module): BranchManifest =
    advance(branch, putArt(module.artifact))

  /** @deprecated Use [[importModule]] for bootstrap/import seeds; [[commitTip]] for ΔL. */
  def commitModule(branch: String, module: Module): BranchManifest =
    importModule(branch, module)

  /** Persist a [[SemanticRepository.ValidatedTip]] via journaled transactional
    * accept (CAS → journal → refs → clear).
    */
  def commitTip(branch: String, tip: SemanticRepository.ValidatedTip): BranchManifest =
    val cur = load(branch)
    val histRoot = cur.causalHistoryRoot.orElse(Some(tip.vcs.base))
    transactionalAccept(
      branch,
      tip.tip,
      tip.vcs,
      parents = cur.head.toList.map(_.valueHash),
      causalHistoryRoot = histRoot,
      publish = None,
      provenanceParents = List(tip.baseDigest),
      provenanceTool = "semantic-commit",
      extraPuts = List(tip.base.artifact),
    ).fold(e => throw RuntimeException(e), identity)

  /** Load + replay-check the tip ValidatedChangeSet.
    * Prefers [[BranchManifest.acceptedChange]]; `.change` sidecar is a cache.
    */
  def loadChange(
      branch: String, language: ComposedLanguage
  ): Either[String, Delta.ValidatedChangeSet] =
    val fromManifest = load(branch).acceptedChange
    val fromSidecar =
      val p = changeRefPath(branch)
      if refsExists(p) then Some(Digest(refsRead(p).trim)) else None
    fromManifest.orElse(fromSidecar) match
      case Some(d) => loadVcs(language, d)
      case None =>
        Left(s"branch '$branch' has no persisted change (commit via commitTip)")

  /** Full change history (oldest → newest), each entry replay-checked.
    * Prefers [[BranchManifest.changeHistory]]; `.changes` sidecar is a cache.
    */
  def loadChangeHistory(
      branch: String, language: ComposedLanguage
  ): Either[String, List[Delta.ValidatedChangeSet]] =
    val manifestHist = load(branch).changeHistory
    val digests =
      if manifestHist.nonEmpty then manifestHist
      else
        val hist = changeHistoryPath(branch)
        if refsExists(hist) then
          refsRead(hist).linesIterator.map(_.trim).filter(_.nonEmpty).map(Digest(_)).toList
        else Nil
    if digests.nonEmpty then
      digests.foldLeft[Either[String, List[Delta.ValidatedChangeSet]]](Right(Nil)) { (acc, d) =>
        acc.flatMap(xs => loadVcs(language, d).map(xs :+ _))
      }
    else loadChange(branch, language).map(List(_))

  /** Reconstruct a [[SemanticRepository.ValidatedTip]] (replay-checked). */
  def loadTip(
      branch: String, language: ComposedLanguage
  ): Either[String, SemanticRepository.ValidatedTip] =
    for
      vcs <- loadChange(branch, language)
      tipMod <- headModule(branch)
      baseArt <- getByDigest(vcs.base)
      base <-
        if baseArt.kind != ArtifactKind.Ir then Left(s"base ${vcs.base.short} is not a module")
        else Right(Module.fromCanon(baseArt.body))
      _ <- Either.cond(tipMod.digest == vcs.result, (),
        s"branch '$branch' head ${tipMod.digest.short} does not match change result ${vcs.result.short}")
      checked <- SemanticRepository.ValidatedTip.check(
        language, SemanticRepository.Tip(base, tipMod, vcs.change))
    yield checked

  /** Semantic merge into `into`: integrate two change histories relative to
    * `base`, then journaled transactional accept. On conflict, the conflict
    * artifact is stored in CAS and the branch head is left unchanged.
    * Ledger publish is opt-in via `publish`.
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
      parentBranches: List[String] = Nil,
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    SemanticRepository.integrate(language, base, changeOurs, changeTheirs, migration).flatMap {
      case SemanticRepository.Outcome.Conflicted(conflict) =>
        val conflictKey = putArt(conflict.artifact)
        val cur = load(into)
        val marked = BranchManifest(
          into, cur.head, cur.history, cur.causalHistoryRoot, cur.parents,
          cur.acceptedChange, conflictState = Some(conflictKey.valueHash),
          changeHistory = cur.changeHistory,
          certificates = cur.certificates,
          primaryAncestor = cur.primaryAncestor,
          references = cur.references)
        putArt(marked.artifact) // conflictState recorded; head unchanged
        refsMkdirs()
        refsWrite(conflictRefPath(into), conflictKey.valueHash.hex)
        Right(Left(conflict))
      case SemanticRepository.Outcome.Accepted(module, vcs, _, _) =>
        val parentDigests =
          if parentBranches.nonEmpty then
            parentBranches.flatMap(b => load(b).head.map(_.valueHash))
          else load(into).head.toList.map(_.valueHash)
        transactionalAccept(
          into, module, vcs, parentDigests, Some(base.digest), publish,
          provenanceParents = List(base.digest),
          provenanceTool = "semantic-merge",
          extraPuts = List(base.artifact),
        ).map(Right(_))
    }

  /** Everyday merge: causal LCA via [[PatchGraph]] when histories form a DAG,
    * falling back to shared module-result digests. Loaded change-sets are
    * replay-checked. Ledger publish remains opt-in via `publish`.
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
      histA <- loadChangeHistory(ours, language)
      histB <- loadChangeHistory(theirs, language)
      (idxA, idxB) = patchAwareLca(histA, histB)
      // Prefer identical linear prefix when it reaches the same LCA tip.
      linear = commonAncestorPrefix(histA, histB)
      (iA, iB) =
        if linear > 0 && linear - 1 == idxA && linear - 1 == idxB then (linear - 1, linear - 1)
        else (idxA, idxB)
      suffixA = if iA < 0 then histA else histA.drop(iA + 1)
      suffixB = if iB < 0 then histB else histB.drop(iB + 1)
      baseDig <-
        if iA >= 0 then Right(histA(iA).result)
        else if histA.nonEmpty && histB.nonEmpty && histA.head.base == histB.head.base then
          Right(histA.head.base)
        else if histA.isEmpty || histB.isEmpty then Left("empty branch history")
        else Left(s"branch histories do not share a causal ancestor: ${histA.head.base.short} vs ${histB.head.base.short}")
      stackedA <-
        if suffixA.isEmpty then Right(None)
        else composeHistory(language, suffixA).map(Some(_))
      stackedB <-
        if suffixB.isEmpty then Right(None)
        else composeHistory(language, suffixB).map(Some(_))
      baseArt <- getByDigest(baseDig)
      base <-
        if baseArt.kind != ArtifactKind.Ir then Left(s"base ${baseDig.short} is not a module")
        else Right(Module.fromCanon(baseArt.body))
      parentDigests = List(ours, theirs).flatMap(b => load(b).head.map(_.valueHash))
      out <- (stackedA, stackedB) match
        case (None, None) =>
          headModule(ours).flatMap { m =>
            // Fast-forward: put tip module, advance without a new change-set.
            val modKey = putArt(m.artifact)
            Right(Right(advance(
              into, modKey,
              acceptedChange = load(ours).acceptedChange,
              parents = parentDigests,
              causalHistoryRoot = Some(baseDig))))
          }
        case (Some((_, chA)), Some((_, chB))) =>
          merge(language, into, base, chA, chB, migration, publish, List(ours, theirs))
        case (Some((_, chA)), None) =>
          SemanticRepository.commit(language, base, chA).flatMap { (mod, vcs) =>
            transactionalAccept(
              into, mod, vcs, parentDigests, Some(baseDig), publish,
              provenanceParents = List(base.digest),
              provenanceTool = "semantic-merge",
            ).map(Right(_))
          }
        case (None, Some((_, chB))) =>
          SemanticRepository.commit(language, base, chB).flatMap { (mod, vcs) =>
            transactionalAccept(
              into, mod, vcs, parentDigests, Some(baseDig), publish,
              provenanceParents = List(base.digest),
              provenanceTool = "semantic-merge",
            ).map(Right(_))
          }
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

  /** Put a certificate artifact and append its digest to the branch manifest
    * `certificates` list (linked CAS reference from branch state).
    *
    * Kernel [[CertificateAttach.check]] is mandatory: kind/issuer/tip structure,
    * and tip must match the current branch head when a head exists. Free-form
    * or tip-mismatched certificates cannot become privileged branch state.
    */
  def attachCertificate(branch: String, cert: Artifact): Either[String, (BranchManifest, Digest)] =
    val cur = load(branch)
    val expectedTip = cur.head.map(_.valueHash)
    CertificateAttach.check(cert, expectedTip).flatMap { _ =>
      val dig = putArt(cert).valueHash
      if cur.certificates.contains(dig) then Right((cur, dig))
      else
        val next = cur.copy(certificates = cur.certificates :+ dig)
        val key = putArt(next.artifact)
        refsMkdirs()
        refsWrite(refPath(branch), key.valueHash.hex)
        Right((next, dig))
    }
