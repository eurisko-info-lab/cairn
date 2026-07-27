package cairn.core

import cairn.kernel.*

/** A semantic location may depend on the context established by particular
  * causal changes. Positional witnesses remain inside SemanticPath and never
  * become repository identity. */
final case class ContextDependency(location: SemanticLocation, providers: Set[Digest]):
  def canon: Canon = Canon.cmap(
    "location" -> location.canon,
    "providers" -> Canon.cstrs(providers.toList.sortBy(_.hex).map(_.hex)))

object ContextDependency:
  def fromCanon(c: Canon): Either[String, ContextDependency] =
    SemanticLocation.fromCanon(c.field("location")).map(location =>
      ContextDependency(location, c.field("providers").asList.map(x => Digest(x.asStr)).toSet))

/** One validated change in the native causal graph. `change` is the real
  * ValidatedChangeSet digest; this envelope adds repository causality without
  * changing ΔL identity. */
final case class CausalChange(
    change: Digest,
    dependencies: Set[Digest],
    context: List[ContextDependency],
    base: Digest,
    result: Digest,
    runtime: Digest,
    resolves: Option[Digest] = None,
    acceptanceEvidence: Option[Digest] = None,
):
  def canon: Canon = Canon.cmap(
    "change" -> Canon.CStr(change.hex),
    "dependencies" -> Canon.cstrs(dependencies.toList.sortBy(_.hex).map(_.hex)),
    "context" -> Canon.CList(context.sortBy(_.location.render).map(_.canon)),
    "base" -> Canon.CStr(base.hex),
    "result" -> Canon.CStr(result.hex),
    "runtime" -> Canon.CStr(runtime.hex),
    "resolves" -> resolves.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))),
    "acceptanceEvidence" -> acceptanceEvidence.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))))
  def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, Canon.CTag("causal-change", canon))
  def id: Digest = artifact.digest

object CausalChange:
  def fromArtifact(a: Artifact): Either[String, CausalChange] =
    if a.kind != ArtifactKind.ChangeSet then Left("causal-change artifact has wrong kind")
    else a.body match
      case Canon.CTag("causal-change", c) =>
        try
          val context = c.field("context").asList.foldLeft[Either[String, List[ContextDependency]]](Right(Nil)) {
            (acc, raw) => for xs <- acc; x <- ContextDependency.fromCanon(raw) yield xs :+ x }
          context.map(ctx => CausalChange(
            Digest(c.field("change").asStr),
            c.field("dependencies").asList.map(x => Digest(x.asStr)).toSet,
            ctx, Digest(c.field("base").asStr), Digest(c.field("result").asStr),
            Digest(c.field("runtime").asStr), c.field("resolves") match
              case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d))
              case _ => None,
            c.asMap.get("acceptanceEvidence").flatMap {
              case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d)); case _ => None }))
        catch case e: Exception => Left(s"invalid causal change: ${e.getMessage}")
      case _ => Left("expected causal-change artifact")

final case class RepositoryConflict(
    conflict: Digest,
    causes: Set[Digest],
    unresolved: Set[SemanticLocation],
    resolution: Option[Digest] = None,
):
  def canon: Canon = Canon.cmap(
    "conflict" -> Canon.CStr(conflict.hex),
    "causes" -> Canon.cstrs(causes.toList.sortBy(_.hex).map(_.hex)),
    "unresolved" -> Canon.CList(unresolved.toList.sortBy(_.render).map(_.canon)),
    "resolution" -> resolution.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))))

object RepositoryConflict:
  def fromCanon(c: Canon): Either[String, RepositoryConflict] =
    c.field("unresolved").asList.foldLeft[Either[String, List[SemanticLocation]]](Right(Nil)) {
      (acc, raw) => for xs <- acc; x <- SemanticLocation.fromCanon(raw) yield xs :+ x
    }.map(locations => RepositoryConflict(
      Digest(c.field("conflict").asStr),
      c.field("causes").asList.map(x => Digest(x.asStr)).toSet,
      locations.toSet,
      c.field("resolution") match
        case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d))
        case _ => None))

final case class PartialApplication(
    applied: List[Digest],
    pending: Set[Digest],
    missing: Set[Digest],
    rejected: Map[Digest, String] = Map.empty,
)

/** The one native change graph. Branches are named views over head sets;
  * ordered histories are derived by graph traversal, never authoritative. */
final case class NativeRepository(
    changes: Map[Digest, CausalChange] = Map.empty,
    pending: Map[Digest, CausalChange] = Map.empty,
    conflicts: Map[Digest, RepositoryConflict] = Map.empty,
    heads: Map[String, Set[Digest]] = Map.empty,
):
  private def requirements(c: CausalChange): Set[Digest] =
    c.dependencies ++ c.context.flatMap(_.providers)

  private def validateReady(c: CausalChange, known: Set[Digest]): Either[String, Unit] =
    val missing = requirements(c) -- known
    if missing.nonEmpty then Left(s"change ${c.id.short} misses dependencies ${missing.toList.map(_.short).sorted.mkString(",")}")
    else if c.context.exists(cd => !cd.providers.subsetOf(c.dependencies)) then
      Left(s"change ${c.id.short} has context providers outside its explicit dependencies")
    else Right(())

  def add(change: CausalChange): Either[String, NativeRepository] =
    if changes.contains(change.id) || pending.contains(change.id) then Left(s"change ${change.id.short} already exists")
    else validateReady(change, changes.keySet).map(_ => copy(changes = changes + (change.id -> change)))

  /** Accept an arbitrary change-centric transfer. Ready changes enter the
    * graph; the rest remain durable pending state until dependencies arrive. */
  def offer(incoming: List[CausalChange]): Either[String, (NativeRepository, PartialApplication)] =
    val unique = incoming.map(c => c.id -> c).toMap
    if unique.size != incoming.size then Left("partial application contains duplicate change identities")
    else
      @annotation.tailrec
      def loop(todo: Map[Digest, CausalChange], graph: NativeRepository, applied: List[Digest]): (NativeRepository, List[Digest], Map[Digest, CausalChange]) =
        val ready = todo.values.filter(c => requirements(c).subsetOf(graph.changes.keySet)).toList.sortBy(_.id.hex)
        if ready.isEmpty then (graph, applied, todo)
        else
          val next = ready.foldLeft(graph) { (g, c) => g.copy(changes = g.changes + (c.id -> c), pending = g.pending - c.id) }
          loop(todo -- ready.map(_.id), next, applied ++ ready.map(_.id))
      val combined = pending ++ unique
      val start = copy(pending = Map.empty)
      val (readyGraph, applied, blocked) = loop(combined, start, Nil)
      val knownOrOffered = readyGraph.changes.keySet ++ blocked.keySet
      val missing = blocked.values.flatMap(requirements).toSet -- knownOrOffered
      Right(readyGraph.copy(pending = blocked) -> PartialApplication(applied, blocked.keySet, missing))

  /** Retain semantically plausible envelopes whose artifact closure is not
    * complete yet. They cannot become resident through this path. */
  def retainPending(incoming: List[CausalChange]): Either[String, NativeRepository] =
    val duplicates = incoming.map(_.id).filter(id => changes.contains(id) || pending.contains(id))
    if duplicates.nonEmpty then Left(s"pending transfer duplicates ${duplicates.map(_.short).mkString(",")}")
    else Right(copy(pending = pending ++ incoming.map(c => c.id -> c)))

  def recordConflict(value: RepositoryConflict): Either[String, NativeRepository] =
    if value.causes.size < 2 then Left("repository conflict must retain at least two causal changes")
    else if !value.causes.subsetOf(changes.keySet) then Left("repository conflict names unavailable causes")
    else if conflicts.contains(value.conflict) then Left(s"conflict ${value.conflict.short} already exists")
    else Right(copy(conflicts = conflicts + (value.conflict -> value)))

  def addResolution(change: CausalChange): Either[String, NativeRepository] =
    change.resolves.toRight("resolution change does not name a conflict").flatMap { conflictId =>
      conflicts.get(conflictId).toRight(s"unknown conflict ${conflictId.short}").flatMap { conflict =>
        if conflict.resolution.nonEmpty then Left(s"conflict ${conflictId.short} is already resolved")
        else if !conflict.causes.subsetOf(change.dependencies) then Left("resolution is not causally dependent on both conflicting changes")
        else add(change).map(g => g.copy(conflicts = g.conflicts.updated(conflictId,
          conflict.copy(resolution = Some(change.id), unresolved = Set.empty))))
      }
    }

  def setHeads(branch: String, value: Set[Digest]): Either[String, NativeRepository] =
    val unknown = value -- changes.keySet
    Either.cond(unknown.isEmpty, copy(heads = heads.updated(branch, value)),
      s"branch view '$branch' names unknown heads ${unknown.toList.map(_.short).sorted.mkString(",")}")

  def ancestors(ids: Set[Digest]): Set[Digest] =
    @annotation.tailrec
    def loop(todo: List[Digest], seen: Set[Digest]): Set[Digest] = todo match
      case Nil => seen
      case id :: rest if seen(id) => loop(rest, seen)
      case id :: rest => loop(changes.get(id).toList.flatMap(requirements).toList ++ rest, seen + id)
    loop(ids.toList, Set.empty)

  /** Change-centric pull/push payload in deterministic dependency order. */
  def transfer(wantedHeads: Set[Digest], receiverHas: Set[Digest]): Either[String, List[CausalChange]] =
    val unknown = wantedHeads -- changes.keySet
    if unknown.nonEmpty then Left(s"unknown requested heads ${unknown.toList.map(_.short).sorted.mkString(",")}")
    else
      val wanted = ancestors(wantedHeads) -- receiverHas
      @annotation.tailrec
      def order(todo: Set[Digest], emitted: Set[Digest], out: List[CausalChange]): List[CausalChange] =
        if todo.isEmpty then out
        else
          val ready = todo.toList.filter(id => requirements(changes(id)).subsetOf(emitted ++ receiverHas)).sortBy(_.hex)
          if ready.isEmpty then out // impossible for a validated graph
          else order(todo -- ready, emitted ++ ready, out ++ ready.map(changes))
      Right(order(wanted, Set.empty, Nil))

  /** Full graph verification hook. Core checks causal/conflict topology from
    * roots; the runtime callback independently certifies each semantic node
    * against its installed artifact closure. */
  def verifyFromRoots(certify: CausalChange => Either[String, Unit]): Either[String, Digest] = for
    _ <- changes.values.toList.sortBy(_.id.hex).foldLeft[Either[String, Unit]](Right(())) {
      (acc, change) => acc.flatMap(_ => validateReady(change, changes.keySet)).flatMap(_ => certify(change)) }
    _ <- conflicts.values.foldLeft[Either[String, Unit]](Right(())) { (acc, conflict) => acc.flatMap { _ =>
      for
        _ <- Either.cond(conflict.causes.size >= 2 && conflict.causes.subsetOf(changes.keySet), (),
          s"conflict ${conflict.conflict.short} has unavailable causes")
        _ <- conflict.resolution.fold[Either[String, Unit]](Right(())) { resolution =>
          changes.get(resolution).toRight(s"conflict ${conflict.conflict.short} has unavailable resolution")
            .flatMap(c => Either.cond(c.resolves.contains(conflict.conflict) && conflict.causes.subsetOf(c.dependencies), (),
              s"conflict ${conflict.conflict.short} has invalid resolution linkage")) }
      yield () } }
    _ <- heads.foldLeft[Either[String, Unit]](Right(())) { case (acc, (branch, selected)) =>
      acc.flatMap(_ => Either.cond(selected.subsetOf(changes.keySet), (), s"branch '$branch' has unavailable heads")) }
  yield digest

  /** CAS roots required by the causal graph, independent of branch-history lists. */
  def gcRoots: Set[Digest] =
    changes.values.flatMap(c => List(c.change, c.base, c.result, c.runtime) ++ c.resolves.toList ++ c.acceptanceEvidence).toSet ++
      pending.values.flatMap(c => List(c.change, c.base, c.result, c.runtime) ++ c.acceptanceEvidence).toSet ++
      conflicts.values.flatMap(c => c.causes ++ Set(c.conflict) ++ c.resolution).toSet

  def canon: Canon = Canon.CTag("native-repository", Canon.cmap(
    "changes" -> Canon.CList(changes.values.toList.sortBy(_.id.hex).map(_.canon)),
    "pending" -> Canon.CList(pending.values.toList.sortBy(_.id.hex).map(_.canon)),
    "conflicts" -> Canon.CList(conflicts.values.toList.sortBy(_.conflict.hex).map(_.canon)),
    "heads" -> Canon.cmap(heads.toList.sortBy(_._1).map((name, ids) =>
      name -> Canon.cstrs(ids.toList.sortBy(_.hex).map(_.hex)))*)))
  def artifact: Artifact = Artifact(ArtifactKind.RepositoryGraph, canon)
  def digest: Digest = artifact.digest

object NativeRepository:
  val empty: NativeRepository = NativeRepository()

  def fromArtifact(a: Artifact): Either[String, NativeRepository] =
    if a.kind != ArtifactKind.RepositoryGraph then Left("expected repository-graph artifact")
    else a.body match
      case Canon.CTag("native-repository", c) =>
        def changes(field: String): Either[String, List[CausalChange]] =
          c.field(field).asList.foldLeft[Either[String, List[CausalChange]]](Right(Nil)) { (acc, raw) =>
            for xs <- acc; change <- CausalChange.fromArtifact(Artifact(ArtifactKind.ChangeSet, Canon.CTag("causal-change", raw)))
            yield xs :+ change }
        for
          accepted <- changes("changes")
          pending <- changes("pending")
          conflicts <- c.field("conflicts").asList.foldLeft[Either[String, List[RepositoryConflict]]](Right(Nil)) {
            (acc, raw) => for xs <- acc; conflict <- RepositoryConflict.fromCanon(raw) yield xs :+ conflict }
          repo = NativeRepository(accepted.map(x => x.id -> x).toMap, pending.map(x => x.id -> x).toMap,
            conflicts.map(x => x.conflict -> x).toMap,
            c.field("heads").asMap.map((name, ids) => name -> ids.asList.map(x => Digest(x.asStr)).toSet))
          _ <- Either.cond(repo.changes.keySet.intersect(repo.pending.keySet).isEmpty, (), "repository duplicates accepted and pending changes")
          _ <- repo.changes.values.foldLeft[Either[String, Unit]](Right(()))((acc, x) =>
            acc.flatMap(_ => repo.validateReady(x, repo.changes.keySet)))
          _ <- repo.heads.foldLeft[Either[String, Unit]](Right(()))((acc, row) =>
            acc.flatMap(_ => Either.cond(row._2.subsetOf(repo.changes.keySet), (), s"branch '${row._1}' has unknown heads")))
          _ <- repo.conflicts.values.foldLeft[Either[String, Unit]](Right(()))((acc, conflict) =>
            acc.flatMap(_ => Either.cond(conflict.causes.subsetOf(repo.changes.keySet), (),
              s"conflict ${conflict.conflict.short} has unknown causes")))
        yield repo
      case _ => Left("expected native-repository body")
