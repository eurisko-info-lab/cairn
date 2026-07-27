package cairn.runtime

import cairn.kernel.*
import cairn.core.{Delta, Module}
import cairn.systeminterface.Cas
import cairn.systeminterface.Filesystem as Fs
import cairn.systemhandler.{CasEffects, CasAdmin, CasAdminEffects, Filesystem, EffectContext, Node, Keypair, Provenance}
import java.nio.file.{Files, Path}

/** Optional ledger publish after a local accept. Top-level (not nested in
  * [[BranchRefStore]]/[[Branches]]) so it isn't a path-dependent type tied to
  * one instance of either.
  */
final case class Publish(
    node: Node,
    authority: Keypair,
    authorities: Map[String, Vector[Byte]],
)

/** Result of [[BranchRefStore.reclaimOrphanBlobs]]. Top-level for the same
  * reason as [[Publish]] — not a path-dependent type tied to one instance.
  */
final case class ReclaimReport(
    recovered: List[String],
    gc: CasAdmin.GcReport,
    roots: Int,
)

/** Raw branch-ref/manifest mechanics over a CAS: ref file read/write, the
  * accept journal, GC roots, ledger publish of an already-decided head.
  * No ΔL replay, no [[cairn.core.AcceptancePolicy]], no domain gate — that's
  * [[Branches]]' job, which holds one of these and is the only sanctioned
  * way for code outside `cairn.runtime` to advance a branch head.
  *
  * Most members are `private[runtime]`: reachable from [[Branches]] (the
  * semantic façade built on top) and from same-package tests exercising the
  * raw mechanics directly (`cairn.runtime.BranchRefMechanicsSuite`), but not
  * from any other package — the acceptance boundary is sealed by keeping
  * this store out of the public surface everyone else uses, not by hiding
  * it behind a name.
  */
final class BranchRefStore(cas: Cas, refsDir: Path, ctx: EffectContext):
  private def casErr(e: Cas.Error): String = e match
    case Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
    case Cas.Error.Io(m)      => m

  private def fsAbs(p: Path): Fs.Path = Fs.Path(p.toAbsolutePath.normalize.toString)

  private def fsErr(e: Fs.Error): String = e match
    case Fs.Error.NotFound(p) => s"not found: ${p.value}"
    case Fs.Error.Io(m)       => m

  private def fsRun(req: Fs.Request): Either[String, Fs.Response] =
    Filesystem.run(req, ctx).left.map(fsErr)

  private[runtime] def refsMkdirs(): Unit =
    fsRun(Fs.Request.Mkdirs(fsAbs(refsDir))).fold(e => throw RuntimeException(e), _ => ())

  private[runtime] def refsExists(p: Path): Boolean =
    fsRun(Fs.Request.Exists(fsAbs(p))) match
      case Right(Fs.Response.Bool(b)) => b
      case Left(e)                    => throw RuntimeException(e)
      case other                      => throw RuntimeException(s"unexpected fs response: $other")

  private[runtime] def refsRead(p: Path): String =
    fsRun(Fs.Request.Read(fsAbs(p))) match
      case Right(Fs.Response.Text(s)) => s
      case Left(e)                    => throw RuntimeException(e)
      case other                      => throw RuntimeException(s"unexpected fs response: $other")

  private[runtime] def refsWrite(p: Path, content: String): Unit =
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

  private[runtime] def putArt(a: Artifact): TypedKey =
    CasEffects.put(cas, a, ctx).fold(e => throw RuntimeException(casErr(e)), identity)

  private[runtime] def getByDigest(d: Digest): Either[String, Artifact] =
    CasEffects.get(cas, d, ctx).left.map(casErr)

  private[runtime] def getKey(key: TypedKey): Either[String, Artifact] =
    getByDigest(key.valueHash).flatMap { a =>
      TypedKey.check(key, a.key).map(_ => a)
    }

  private[runtime] def refPath(branch: String): Path =
    require(branch.nonEmpty && branch.forall(c => c.isLetterOrDigit || c == '-' || c == '_'), s"bad branch name '$branch'")
    refsDir.resolve(branch)

  /** Sidecar ref: digest of the ValidatedChangeSet that produced the tip. */
  private[runtime] def changeRefPath(branch: String): Path =
    refsDir.resolve(s"$branch.change")

  /** Append-only log of every ValidatedChangeSet digest for `branch`. */
  private[runtime] def changeHistoryPath(branch: String): Path =
    refsDir.resolve(s"$branch.changes")

  private def acceptJournalPath(branch: String): Path =
    refsDir.resolve(s"$branch.accepting")

  /** Sidecar: conflict artifact digest when merge left the head unchanged. */
  private[runtime] def conflictRefPath(branch: String): Path =
    refsDir.resolve(s"$branch.conflict")

  private[runtime] def conflictContextRefPath(branch: String): Path =
    refsDir.resolve(s"$branch.conflict-context")

  private def isSidecar(name: String): Boolean =
    name.endsWith(".change") || name.endsWith(".changes") ||
      name.endsWith(".accepting") || name.endsWith(".conflict") || name.endsWith(".conflict-context")

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
      /** Domain-gate judgment that validated `moduleDigest`, if any (survives
        * crash-recovery so [[applyRefs]] can restore [[BranchManifest.gateEvidence]]).
        */
      gateJudgment: Option[String] = None,
      /** Digest of the [[cairn.core.AcceptanceEvidence]] artifact backing this
        * accept, if any (survives crash-recovery so [[applyRefs]] can restore
        * [[BranchManifest.acceptanceEvidence]]).
        */
      acceptanceEvidence: Option[Digest] = None,
      domainRuntime: Option[Digest] = None,
  ):
    def rootDigests: List[Digest] =
      moduleDigest :: vcsDigest :: parents ++ causalHistoryRoot.toList ++ extras ++ acceptanceEvidence.toList ++ domainRuntime.toList

    def encode: String =
      val lines = List(
        s"branch=$branch",
        s"module=${moduleDigest.hex}",
        s"vcs=${vcsDigest.hex}",
        s"parents=${parents.map(_.hex).mkString(",")}",
        s"causal=${causalHistoryRoot.map(_.hex).getOrElse("")}",
        s"historyAppend=$historyAppend",
        s"phase=$phase",
        s"extras=${extras.map(_.hex).mkString(",")}",
        s"gateJudgment=${gateJudgment.getOrElse("")}",
        s"acceptanceEvidence=${acceptanceEvidence.map(_.hex).getOrElse("")}",
        s"domainRuntime=${domainRuntime.map(_.hex).getOrElse("")}")
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
        gateJudgment = m.get("gateJudgment").filter(_.nonEmpty)
        acceptanceEvidence = m.get("acceptanceEvidence").filter(_.nonEmpty).map(Digest(_))
        domainRuntime = m.get("domainRuntime").filter(_.nonEmpty).map(Digest(_))
      yield AcceptJournal(
        branch, Digest(mod), Digest(vcs), parents, causal, histAppend, phase, extras,
        gateJudgment, acceptanceEvidence, domainRuntime)

  private def writeJournal(j: AcceptJournal): Unit =
    refsMkdirs()
    refsWrite(acceptJournalPath(j.branch), j.encode)

  private def clearJournal(branch: String): Unit =
    refsDelete(acceptJournalPath(branch))

  private[runtime] def clearConflict(branch: String): Unit =
    refsDelete(conflictRefPath(branch))
    refsDelete(conflictContextRefPath(branch))

  private[runtime] def clearConflictContext(branch: String): Unit =
    refsDelete(conflictContextRefPath(branch))

  private def applyRefs(j: AcceptJournal): BranchManifest =
    val modArt = getByDigest(j.moduleDigest).fold(e => throw RuntimeException(e), identity)
    val vcsArt = getByDigest(j.vcsDigest).fold(e => throw RuntimeException(e), identity)
    if j.historyAppend then persistChange(j.branch, vcsArt.key)
    else
      refsMkdirs()
      refsWrite(changeRefPath(j.branch), vcsArt.key.valueHash.hex)
    advanceRaw(
      j.branch,
      modArt.key,
      acceptedChange = Some(vcsArt.key.valueHash),
      parents = j.parents,
      causalHistoryRoot = j.causalHistoryRoot,
      gateEvidence = j.gateJudgment.map(g => List(g -> j.moduleDigest)).getOrElse(Nil),
      acceptanceEvidence = j.acceptanceEvidence,
      domainRuntime = j.domainRuntime)

  /** All-or-nothing accept: CAS → journal → refs → optional ledger → clear.
    * On ledger failure after refs, journal stays at phase=publish for recovery.
    *
    * If `branch` carries a pending [[cairn.core.Merge.Conflict]] ref
    * (`conflictRefPath`), only a validated ΔConflict artifact may resolve it.
    * The conflict's digest is folded into that accept's provenance inputs, so
    * [[Provenance.why]] on the resulting head surfaces the resolved conflict
    * as a direct lineage input even after [[clearConflict]] removes the live
    * ref. An ordinary commit fails closed instead of selecting a side.
    */
  private[runtime] def transactionalAccept(
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
      gateJudgment: Option[String] = None,
      acceptanceEvidence: Option[Digest] = None,
      domainRuntime: Option[Digest] = None,
      conflictResolution: Option[Artifact] = None,
  ): Either[String, BranchManifest] =
    if module.digest != vcs.result then
      Left(s"accept rejected: module ${module.digest.short} ≠ validated change result ${vcs.result.short}")
    else
      val resolvedConflict =
        if refsExists(conflictRefPath(branch)) then Some(Digest(refsRead(conflictRefPath(branch)).trim))
        else None
      if resolvedConflict.nonEmpty && conflictResolution.isEmpty then
        return Left("accept rejected: branch has an unresolved conflict; submit a validated ΔConflict resolution")
      val extraKeys = (extraPuts ++ conflictResolution.toList).map(putArt)
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
        branch, modKey.valueHash, vcsKey.valueHash, parents, causalHistoryRoot, historyAppend, "cas", extras,
        gateJudgment, acceptanceEvidence, domainRuntime)
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
    * histories, conflict sidecars, pending accept-journal digests, causal
    * digests reachable from stored [[BranchManifest]]s, and each manifest's
    * [[cairn.core.AcceptanceEvidence]] artifact digest.
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
                    m.acceptanceEvidence.foreach(roots += _)
                }
              }
          Right(roots.toSet)
        case other => Left(s"unexpected fs response: $other")

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

  private[runtime] def persistChange(branch: String, vcsKey: TypedKey): Unit =
    refsMkdirs()
    refsWrite(changeRefPath(branch), vcsKey.valueHash.hex)
    val hist = changeHistoryPath(branch)
    val prev = if refsExists(hist) then refsRead(hist) else ""
    val line = vcsKey.valueHash.hex + "\n"
    val last = prev.linesIterator.map(_.trim).filter(_.nonEmpty).toList.lastOption
    if last != Some(vcsKey.valueHash.hex) then refsWrite(hist, prev + line)

  def load(branch: String): BranchManifest =
    val p = refPath(branch)
    if !refsExists(p) then BranchManifest(branch, None, Nil)
    else
      val d = Digest(refsRead(p).trim)
      getByDigest(d).map(a => BranchManifest.fromCanon(a.body)).fold(e => throw RuntimeException(e), identity)

  private[runtime] def pendingConflict(branch: String): Option[Digest] =
    if refsExists(conflictRefPath(branch)) then Some(Digest(refsRead(conflictRefPath(branch)).trim))
    else None

  private[runtime] def pendingConflictContext(branch: String): Option[Digest] =
    if refsExists(conflictContextRefPath(branch)) then Some(Digest(refsRead(conflictContextRefPath(branch)).trim))
    else None

  /** Append: new head goes to history head; manifest itself stored in CAS.
    * Optional causal digests are recorded on the manifest. When
    * `acceptedChange` is set, it is appended to [[BranchManifest.changeHistory]]
    * (sidecars remain write-through caches of the same digests).
    *
    * Raw manifest/ref mechanics — no ΔL replay, no [[cairn.core.AcceptancePolicy]],
    * no domain gate. `package`-private on purpose: this is not part of the
    * sealed semantic-acceptance surface ([[Branches.commitTip]] /
    * [[Branches.merge]] / [[Branches.mergeBranches]]); [[Branches.importModule]]
    * is the only sanctioned caller for planting a module without ΔL, and only
    * on a pristine branch.
    */
  private[runtime] def advanceRaw(
      branch: String,
      newHead: TypedKey,
      acceptedChange: Option[Digest] = None,
      parents: List[Digest] = Nil,
      causalHistoryRoot: Option[Digest] = None,
      conflictState: Option[Digest] = None,
      /** Domain-gate judgment(s) that validated `newHead`'s module, if any.
        * Overwrites (not accumulates) — reflects this accept only, since an
        * un-gated advance genuinely does not carry forward a prior gate's claim.
        */
      gateEvidence: List[(String, Digest)] = Nil,
      /** Digest of the [[cairn.core.AcceptanceEvidence]] artifact backing this
        * accept, if any. Same overwrite-not-accumulate semantics as `gateEvidence`.
        */
      acceptanceEvidence: Option[Digest] = None,
      domainRuntime: Option[Digest] = None,
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
      gateEvidence = gateEvidence,
      acceptanceEvidence = acceptanceEvidence,
      domainRuntime = domainRuntime,
      primaryAncestor = cur.primaryAncestor,
      references = cur.references,
      domainAgreement = cur.domainAgreement)
    val key = putArt(next.artifact)
    refsMkdirs()
    refsWrite(refPath(branch), key.valueHash.hex)
    next

  /** Persist domain ancestry (or any other manifest field) on a branch ref
    * without touching head/history — used by [[Branches]]' domain-governance
    * layer (`forkFrom`/`referTo`/`plantGoverned`).
    */
  private[runtime] def storeManifest(m: BranchManifest): BranchManifest =
    val key = putArt(m.artifact)
    refsMkdirs()
    refsWrite(refPath(m.branch), key.valueHash.hex)
    m

  def list(): List[String] =
    if !refsExists(refsDir) then Nil
    else
      fsRun(Fs.Request.List(fsAbs(refsDir))) match
        case Right(Fs.Response.Entries(names)) => names.filterNot(isSidecar).sorted
        case Left(e)                           => throw RuntimeException(e)
        case other                             => throw RuntimeException(s"unexpected fs response: $other")

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
