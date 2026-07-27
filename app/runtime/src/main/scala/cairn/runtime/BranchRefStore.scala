package cairn.runtime

import cairn.kernel.*
import cairn.core.*
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

  private def repositoryGraphRefPath: Path = refsDir.resolve("repository.graph")
  private def repositoryCertificationsRefPath: Path = refsDir.resolve("repository.certifications")

  private def isSidecar(name: String): Boolean =
    name.endsWith(".change") || name.endsWith(".changes") ||
      name.endsWith(".accepting") || name.endsWith(".conflict") || name.endsWith(".conflict-context") ||
      name == "repository.graph" || name == "repository.certifications"

  def nativeRepository: Either[String, NativeRepository] =
    if !refsExists(repositoryGraphRefPath) then Right(NativeRepository.empty)
    else
      val digest = Digest(refsRead(repositoryGraphRefPath).trim)
      getByDigest(digest).flatMap(NativeRepository.fromArtifact)

  private def storeNativeRepository(repository: NativeRepository): Digest =
    val digest = putArt(repository.artifact).valueHash
    refsMkdirs()
    refsWrite(repositoryGraphRefPath, digest.hex)
    digest

  /** Change-centric transfer: branches contribute only their current head view. */
  def pullChanges(branch: String, receiverHas: Set[Digest]): Either[String, List[CausalChange]] =
    nativeRepository.flatMap(repo => repo.transfer(repo.heads.getOrElse(branch, Set.empty), receiverHas))

  def pushChanges(changes: List[CausalChange]): Either[String, PartialApplication] =
    nativeRepository.flatMap(_.offer(changes)).map { case (repo, result) =>
      storeNativeRepository(repo)
      result
    }

  /** Complete CAS payload for a change-centric pull, including the selected
    * runtime and every canonically discoverable dependency. */
  def pullChangeArtifacts(branch: String, receiverHas: Set[Digest]): Either[String, List[Artifact]] =
    pullChanges(branch, receiverHas).flatMap { changes =>
      def walk(todo: List[Digest], seen: Set[Digest], out: List[Artifact]): Either[String, List[Artifact]] = todo match
        case Nil => Right(out)
        case digest :: rest if seen(digest) => walk(rest, seen, out)
        case digest :: rest =>
          for
            artifact <- getByDigest(digest)
            dependencies <- ArtifactDependencies.direct(artifact)
            result <- walk(dependencies ++ rest, seen + digest, artifact :: out)
          yield result
      val roots = changes.flatMap(c => List(c.change, c.base, c.result, c.runtime) ++ c.acceptanceEvidence)
      for
        repository <- nativeRepository
        closure <- walk(roots, Set.empty, repository.artifact :: changes.map(_.artifact))
      yield closure.distinctBy(_.digest).sortBy(_.digest.hex)
    }

  /** Untrusted replication boundary. Artifact bytes are staged in memory;
    * causal nodes enter the graph only after runtime-bound replay, acceptance,
    * access/context recomputation, and certified-machine selection. */
  def pushChangeArtifacts(
      artifacts: List[Artifact], application: ResolvedApplication,
  ): Either[String, PartialApplication] =
    val staged = artifacts.map(a => a.digest -> a).toMap
    val causalArtifacts = artifacts.filter(_.body match
      case Canon.CTag("causal-change", _) => true
      case _ => false)
    val decodedCausal = causalArtifacts.map(artifact => artifact -> CausalChange.fromArtifact(artifact))
    val causal = decodedCausal.collect { case (_, Right(change)) => change }
    val malformed = decodedCausal.collect { case (artifact, Left(message)) => artifact.digest -> message }.toMap
    for
      _ <- Either.cond(causal.nonEmpty || malformed.nonEmpty, (), "change transfer contains no causal changes")
      current <- nativeRepository
      transferredGraphs <- artifacts.filter(_.kind == ArtifactKind.RepositoryGraph)
        .foldLeft[Either[String, List[NativeRepository]]](Right(Nil)) { (acc, artifact) =>
          for xs <- acc; graph <- NativeRepository.fromArtifact(artifact) yield xs :+ graph }
      _ <- Either.cond(transferredGraphs.size <= 1, (), "change transfer contains multiple repository roots")
      certificationTopology = transferredGraphs.headOption.getOrElse(current)
      classified = causal.map { change => change -> certifyIncoming(change, application, staged, certificationTopology) }
      valid = classified.collect { case (change, Right(_)) => change }
      incomplete = classified.collect { case (change, Left(CertificationFailure.Incomplete(_))) => change }
      rejected = malformed ++ classified.collect {
        case (change, Left(CertificationFailure.Invalid(message))) => change.id -> message }.toMap
      // Uncertified pending nodes are never fed back through structural
      // `offer`; only envelopes certified in this invocation may be promoted.
      offered <- current.copy(pending = Map.empty).offer(valid)
      (afterValid, applied) = offered
      withResolutions = valid.foldLeft(afterValid) { (repo, change) => change.resolves match
        case None => repo
        case Some(conflict) => repo.copy(conflicts = repo.conflicts.updatedWith(conflict)(_.map(
          _.copy(resolution = Some(change.id), unresolved = Set.empty)))) }
      retained = (current.pending -- valid.map(_.id)) ++ incomplete.map(c => c.id -> c)
      afterPending = withResolutions.copy(pending = retained)
      withViews = transferredGraphs.headOption match
        case Some(target) if target.changes.keySet.subsetOf(afterPending.changes.keySet) && target.pending.isEmpty =>
          afterPending.copy(conflicts = target.conflicts, heads = target.heads)
        case _ => afterPending
      missing = classified.collect { case (_, Left(CertificationFailure.Incomplete(ds))) => ds }.flatten.toSet ++ applied.missing
      evidenceArtifacts = classified.collect { case (_, Right((replay, context, certification))) =>
        List(replay.artifact, context.artifact, certification.artifact) }.flatten
      _ = (artifacts ++ evidenceArtifacts).distinctBy(_.digest).foreach(putArt)
      certificationDigests = evidenceArtifacts.filter(_.kind == ArtifactKind.CertifiedCausalChange).map(_.digest)
      _ = if certificationDigests.nonEmpty then
        refsMkdirs()
        val existing = if refsExists(repositoryCertificationsRefPath) then refsRead(repositoryCertificationsRefPath) else ""
        val merged = (existing.linesIterator.flatMap(Digest.parse(_).toOption).toList ++ certificationDigests)
          .distinct.sortBy(_.hex).map(_.hex).mkString("\n") + "\n"
        refsWrite(repositoryCertificationsRefPath, merged)
      _ = storeNativeRepository(withViews)
    yield PartialApplication(applied.applied, withViews.pending.keySet, missing, rejected)

  /** Legacy callers must explicitly supply the application-selected machine;
    * a process-local default is not a certification boundary. */
  def pushChangeArtifacts(artifacts: List[Artifact]): Either[String, PartialApplication] =
    Left("certified causal replication requires a resolved application")

  private enum CertificationFailure:
    case Incomplete(missing: Set[Digest])
    case Invalid(message: String)

  private def certifyIncoming(
      causal: CausalChange, application: ResolvedApplication, staged: Map[Digest, Artifact],
      repository: NativeRepository,
  ): Either[CertificationFailure, (CausalReplayEvidence, CausalContextEvidence, CertifiedCausalChange)] =
    def artifact(digest: Digest): Either[CertificationFailure, Artifact] =
      staged.get(digest).orElse(getByDigest(digest).toOption)
        .toRight(CertificationFailure.Incomplete(Set(digest)))
    def invalid[A](message: String): Either[CertificationFailure, A] = Left(CertificationFailure.Invalid(message))
    val runtimeE = application.runtimes.values.find(_.digest == causal.runtime)
      .toRight(CertificationFailure.Invalid(s"causal change ${causal.id.short} selects an unresolved runtime"))
    for
      runtime <- runtimeE
      implementation <- application.implementations.find(_.component == MachineComponent.ChangeProgramInterpreter)
        .toRight(CertificationFailure.Invalid("application has no certified change-program interpreter"))
      baseArtifact <- artifact(causal.base)
      _ <- Either.cond(baseArtifact.kind == ArtifactKind.Ir, (), CertificationFailure.Invalid("causal base is not a module artifact"))
      resultArtifact <- artifact(causal.result)
      _ <- Either.cond(resultArtifact.kind == ArtifactKind.Ir, (), CertificationFailure.Invalid("causal result is not a module artifact"))
      base <- scala.util.Try(Module.fromCanon(baseArtifact.body)).toEither.left
        .map(e => CertificationFailure.Invalid(s"invalid causal base module: ${e.getMessage}"))
      result <- scala.util.Try(Module.fromCanon(resultArtifact.body)).toEither.left
        .map(e => CertificationFailure.Invalid(s"invalid causal result module: ${e.getMessage}"))
      _ <- Either.cond(base.digest == causal.base && result.digest == causal.result, (),
        CertificationFailure.Invalid("causal base/result digest claim mismatch"))
      vcsArtifact <- artifact(causal.change)
      _ <- Either.cond(vcsArtifact.kind == ArtifactKind.ChangeSet, (), CertificationFailure.Invalid("causal change is not a ValidatedChangeSet artifact"))
      claim <- scala.util.Try(Delta.ValidatedChangeSet.decodeClaim(vcsArtifact.body)).toEither.left
        .map(e => CertificationFailure.Invalid(s"invalid ValidatedChangeSet: ${e.getMessage}"))
      vcs <- Delta.ValidatedChangeSet.check(runtime.language, runtime.changeModel, base, claim)
        .left.map(CertificationFailure.Invalid(_))
      _ <- Either.cond(vcs.result == causal.result && vcs.base == causal.base, (),
        CertificationFailure.Invalid("replayed change does not match causal base/result"))
      trace <- ChangeAlgebra.accessTrace(runtime.language, base, vcs.change, runtime.changeModel)
        .left.map(r => CertificationFailure.Invalid(r.render))
      accesses = trace.accesses.map(_.location).toSet
      declared = causal.context.map(_.location).toSet
      _ <- Either.cond(accesses == declared, (), CertificationFailure.Invalid("causal semantic context differs from replayed access trace"))
      _ <- Either.cond(causal.context.forall(_.providers.subsetOf(causal.dependencies)), (),
        CertificationFailure.Invalid("causal context providers are outside explicit dependencies"))
      _ <- causal.resolves.fold[Either[CertificationFailure, Unit]](Right(())) { conflict =>
        repository.conflicts.get(conflict) match
          case None => invalid(s"causal resolution names unknown conflict ${conflict.short}")
          case Some(value) => Either.cond(value.causes.subsetOf(causal.dependencies), (),
            CertificationFailure.Invalid("causal resolution is not dependent on every conflict cause")) }
      acceptanceDigest <- causal.acceptanceEvidence.toRight(
        CertificationFailure.Invalid("causal change does not name acceptance evidence"))
      acceptanceArtifact <- artifact(acceptanceDigest)
      _ <- Either.cond(acceptanceArtifact.kind == ArtifactKind.AcceptanceEvidence, (),
        CertificationFailure.Invalid("causal acceptance evidence has wrong artifact kind"))
      acceptance <- AcceptanceEvidence.fromCanon(acceptanceArtifact.body).left.map(CertificationFailure.Invalid(_))
      certificateArtifacts <- acceptance.certificates.foldLeft[Either[CertificationFailure, List[Artifact]]](Right(Nil)) {
        (acc, digest) => for xs <- acc; a <- artifact(digest) yield xs :+ a }
      facts = AcceptanceFacts(acceptance.domainAgreement,
        certificateArtifacts.map(a => CertificateFact(a.digest, a.kind, None)), acceptance.authorities.toSet,
        acceptance.migration, acceptance.publicationRequested)
      policy = AcceptancePolicy.gated(runtime.moduleGate(d => application.languages.values.find(_.language.digest == d).map(_.language)))
      _ <- AcceptanceEvidence.verifyComplete(runtime, base, Some(vcs), policy, facts, result, acceptance)
        .left.map(CertificationFailure.Invalid(_))
      traceCanon = Canon.CList(trace.accesses.map(a => Canon.cmap("mode" -> Canon.CStr(a.mode.toString),
        "location" -> a.location.canon)))
      replay = CausalReplayEvidence(application.machine.digest, implementation.artifact.digest,
        runtime.digest, causal.change, causal.base, causal.result, Digest.of(traceCanon))
      context = CausalContextEvidence(causal.id, accesses, causal.context)
      certified = CertifiedCausalChange(causal.id, runtime.digest, replay.artifact.digest,
        acceptanceDigest, context.artifact.digest)
    yield (replay, context, certified)

  /** Re-certify every resident graph node from CAS roots. Pending nodes are
    * intentionally excluded until their artifact/causal requirements arrive. */
  def verifyNativeRepository(application: ResolvedApplication): Either[String, Digest] = for
    repository <- nativeRepository
    digest <- repository.verifyFromRoots { causal =>
        certifyIncoming(causal, application, Map.empty, repository).left.map {
          case CertificationFailure.Incomplete(missing) =>
            s"repository change ${causal.id.short} is incomplete: ${missing.map(_.short).toList.sorted.mkString(",")}"
          case CertificationFailure.Invalid(message) => message
        }.map(_ => ()) }
  yield digest

  private[runtime] def recordNativeConflict(
      target: String, branches: List[String], conflict: cairn.core.Merge.Conflict,
  ): Either[String, Option[Digest]] = nativeRepository.flatMap { repo =>
    val causes = branches.flatMap(b => repo.heads.getOrElse(b, Set.empty)).toSet
    if causes.size < 2 then Right(None)
    else repo.recordConflict(cairn.core.RepositoryConflict(
      conflict.artifact.digest, causes, conflict.overlap)).flatMap(_.setHeads(target, causes)).map { next =>
      Some(storeNativeRepository(next))
    }
  }

  private[runtime] def copyNativeHeads(into: String, from: String): Either[String, Option[Digest]] =
    nativeRepository.flatMap { repo =>
      repo.heads.get(from) match
        case None => Right(None)
        case Some(heads) => repo.setHeads(into, heads).map(next => Some(storeNativeRepository(next)))
    }

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
      repositoryGraph: Option[Digest] = None,
  ):
    def rootDigests: List[Digest] =
      moduleDigest :: vcsDigest :: parents ++ causalHistoryRoot.toList ++ extras ++ acceptanceEvidence.toList ++
        domainRuntime.toList ++ repositoryGraph.toList

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
        s"domainRuntime=${domainRuntime.map(_.hex).getOrElse("")}",
        s"repositoryGraph=${repositoryGraph.map(_.hex).getOrElse("")}")
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
        repositoryGraph = m.get("repositoryGraph").filter(_.nonEmpty).map(Digest(_))
      yield AcceptJournal(
        branch, Digest(mod), Digest(vcs), parents, causal, histAppend, phase, extras,
        gateJudgment, acceptanceEvidence, domainRuntime, repositoryGraph)

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
    val manifest = advanceRaw(
      j.branch,
      modArt.key,
      acceptedChange = Some(vcsArt.key.valueHash),
      parents = j.parents,
      causalHistoryRoot = j.causalHistoryRoot,
      gateEvidence = j.gateJudgment.map(g => List(g -> j.moduleDigest)).getOrElse(Nil),
      acceptanceEvidence = j.acceptanceEvidence,
      domainRuntime = j.domainRuntime,
      repositoryGraph = j.repositoryGraph,
      appendChangeHistory = j.historyAppend)
    j.repositoryGraph.foreach(d => refsWrite(repositoryGraphRefPath, d.hex))
    manifest

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
      contextLocations: Set[SemanticLocation] = Set.empty,
      resolves: Option[Digest] = None,
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
      val repositoryUpdate = domainRuntime match
        case None => Right(None)
        case Some(runtime) => nativeRepository.flatMap { repo =>
          val dependencies = repo.heads.getOrElse(branch, Set.empty)
          val context = contextLocations.toList.sortBy(_.render).map(ContextDependency(_, dependencies))
          val change = CausalChange(vcsKey.valueHash, dependencies, context, vcs.base, vcs.result, runtime,
            resolves, acceptanceEvidence)
          val added = resolves match
            case Some(_) => repo.addResolution(change)
            case None    => repo.add(change)
          added.flatMap(_.setHeads(branch, Set(change.id))).map(next => Some(next -> putArt(next.artifact).valueHash))
        }
      val repositoryDigest = repositoryUpdate match
        case Left(error) => return Left(error)
        case Right(value) => value.map(_._2)
      val provDig =
        Provenance.record(
          cas, module.digest,
          provenanceParents ++ resolvedConflict.toList :+ vcsKey.valueHash,
          provenanceTool, ctx)
          .fold(e => throw RuntimeException(casErr(e)), identity)
      val extras = extraKeys.map(_.valueHash) :+ provDig
      var journal = AcceptJournal(
        branch, modKey.valueHash, vcsKey.valueHash, parents, causalHistoryRoot, historyAppend, "cas", extras,
        gateJudgment, acceptanceEvidence, domainRuntime, repositoryDigest)
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
            else if name.endsWith(".conflict-context") then addHex(refsRead(p))
            else if name == "repository.graph" then
              val hex = refsRead(p).trim
              addHex(hex)
              Digest.parse(hex).foreach(d => getByDigest(d).flatMap(NativeRepository.fromArtifact).foreach { repo =>
                repo.gcRoots.foreach(roots += _)
              })
            else if name == "repository.certifications" then
              refsRead(p).linesIterator.foreach { line =>
                val text = line.trim
                Digest.parse(text).foreach { digest =>
                  roots += digest
                  getByDigest(digest).flatMap(ArtifactDependencies.direct).foreach(_.foreach(roots += _))
                }
              }
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
                    m.domainRuntime.foreach(roots += _)
                    m.repositoryGraph.foreach(roots += _)
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
      repositoryGraph: Option[Digest] = None,
      appendChangeHistory: Boolean = true,
  ): BranchManifest =
    val cur = load(branch)
    val nextHistory = if !appendChangeHistory then cur.changeHistory else acceptedChange match
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
      repositoryGraph = repositoryGraph,
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
