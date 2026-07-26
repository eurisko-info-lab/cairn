package cairn.core

import cairn.kernel.*

/** M16: algebra of validated changes — composition, inverses (relative to a
  * base module), and footprint commutation. M17: three-way semantic merge
  * built on commutation. Never a textual diff (§8).
  */
object ChangeAlgebra:
  import Delta.tag

  private def changeItems(l: ComposedLanguage, change: Cst): List[Cst] = change match
    case Cst.Node(t, List(Cst.Node("list", items))) if t == tag(l, "changeset") => items
    case Cst.Node(t, items) if t == tag(l, "changeset") => items
    case single => List(single)

  def changeset(l: ComposedLanguage, items: List[Cst]): Cst =
    Cst.Node(tag(l, "changeset"), List(Cst.Node("list", items)))

  /** Sequential composition: apply(compose(a, b)) == apply(b) ∘ apply(a). */
  def compose(l: ComposedLanguage, a: Cst, b: Cst): Cst =
    changeset(l, changeItems(l, a) ++ changeItems(l, b))

  /** Definition names an atomic change touches (writes) and reads. Each
    * item's [[ChangeOpDef]] is looked up by tag once, then evaluated via
    * [[ChangeModelInterp.footprintOf]] — no operation-name switch here any
    * more; adding an operation to `model` needs no change to this method.
    */
  def footprint(l: ComposedLanguage, change: Cst, model: ChangeModel = ChangeModel.default): Set[String] =
    changeItems(l, change).flatMap {
      case Cst.Node(t, children) =>
        model.operations.find(o => t == tag(l, o.name)) match
          case Some(op) => ChangeModelInterp.footprintOf(op, children)
          case None     => Set.empty
      case _ => Set.empty
    }.toSet

  /** Rejects a change containing any item whose operation tag is not part of
    * `model` — called BEFORE [[footprint]]/[[Delta.applyTyped]] wherever a
    * caller supplies an explicit model, so a change built against the wrong
    * model fails here rather than silently footprinting to `Set.empty`
    * (which [[footprint]] does for an unrecognized tag, since it must stay
    * total for its many default-model callers) and appearing to commute
    * with everything.
    */
  def checkRecognized(l: ComposedLanguage, change: Cst, model: ChangeModel): Either[String, Unit] =
    changeItems(l, change).foldLeft[Either[String, Unit]](Right(())) { (acc, item) =>
      acc.flatMap { _ =>
        item match
          case Cst.Node(t, _) if model.operations.exists(o => t == tag(l, o.name)) => Right(())
          case other => Left(s"unrecognized operation under this model: ${other.render}")
      }
    }

  /** Syntactic APPROXIMATION of commutation — disjoint footprints (no shared
    * definition name) — used where no base [[Module]] is available to
    * actually try both orders (graph construction in [[PatchGraph]], the
    * pairwise predicate exposed by [[SemanticRepository]]). Disjoint names
    * make non-commutation unlikely but not impossible: a whole-module domain
    * gate can still reject, or in principle order-dependently accept, a
    * footprint-disjoint pair. Wherever a base module IS available and the
    * actual merged result matters, [[Merge.threeWay]] WITNESSES commutation
    * by literally applying both orders and requiring them to agree, rather
    * than trusting this predicate alone.
    */
  def commutes(l: ComposedLanguage, a: Cst, b: Cst, model: ChangeModel = ChangeModel.default): Boolean =
    footprint(l, a, model).intersect(footprint(l, b, model)).isEmpty

  /** Inverse of a change-set RELATIVE to the module it was applied to:
    * apply(invert(c)) ∘ apply(c) == id. Requires the base module because
    * remove/replace/edit inverses need the overwritten content. Each item's
    * inverse is built via [[ChangeModelInterp.inverseOf]] against the
    * PRE-state module (before that item is applied) — no operation-name
    * switch here any more; adding an operation to `model` needs no change
    * to this method, only an [[InverseExpr]] on its [[ChangeOpDef]].
    */
  def invert(l: ComposedLanguage, base: Module, change: Cst, model: ChangeModel = ChangeModel.default): Either[String, Cst] =
    val items = changeItems(l, change)
    // walk forward, collecting each op's inverse against the current module
    val zero: Either[String, (Module, List[Cst])] = Right((base, Nil))
    items.foldLeft(zero) { (acc, item) =>
      acc.flatMap { (m, invs) =>
        val inverse: Either[String, Cst] = item match
          case Cst.Node(t, children) =>
            model.operations.find(o => t == tag(l, o.name)) match
              case Some(op) => ChangeModelInterp.inverseOf(l, model, op, children, m)
              case None     => Left(s"cannot invert: ${item.render}")
          case other => Left(s"cannot invert: ${other.render}")
        for
          inv <- inverse
          applied <- Delta.apply(l, m, changeset(l, List(item)), model).map(_._1)
        yield (applied, inv :: invs) // inverses accumulate in REVERSE order
      }
    }.map((_, invs) => changeset(l, invs))

/** M17: three-way semantic merge over change histories. Disjoint-footprint
  * branches merge automatically; overlaps surface as a structured conflict
  * artifact naming both change-sets — never silent, never textual.
  */
object Merge:
  /** Digest-ordered application order used when witnessing commutation. */
  enum Order:
    case Canonical, Reversed
    def canon: Canon = Canon.CStr(this match
      case Canonical => "canonical"
      case Reversed  => "reversed")
    def render: String = this match
      case Canonical => "canonical order"
      case Reversed  => "reversed order"

  /** Typed evidence for a merge conflict that name-overlap alone did not
    * predict. Part of [[Conflict.canon]] so distinct causes cannot share an
    * artifact digest.
    */
  enum ConflictWitness:
    case ApplyFailed(order: Order, rejection: Delta.Rejection)
    case ResultsDiffer(first: Digest, second: Digest)
    case DomainValidationFailed(judgment: String, detail: Canon)

    def canon: Canon = this match
      case ApplyFailed(order, rejection) =>
        Canon.CTag("apply-failed", Canon.cmap(
          "order" -> order.canon,
          "rejection" -> rejection.canon))
      case ResultsDiffer(first, second) =>
        Canon.CTag("results-differ", Canon.cmap(
          "first" -> Canon.CStr(first.hex),
          "second" -> Canon.CStr(second.hex)))
      case DomainValidationFailed(judgment, detail) =>
        Canon.CTag("domain-validation-failed", Canon.cmap(
          "judgment" -> Canon.CStr(judgment),
          "detail" -> detail))

    def render: String = this match
      case ApplyFailed(order, rejection) =>
        s"${order.render} failed: ${rejection.render}"
      case ResultsDiffer(first, second) =>
        s"order-dependent result: ${first.short} vs ${second.short}"
      case DomainValidationFailed(judgment, detail) =>
        val d = detail match
          case Canon.CStr(s) => s
          case other         => other.toString
        s"module gate '$judgment' rejected: $d"

  /** `witness`, when present, explains a conflict that footprint disjointness
    * alone didn't predict — apply failure, order-dependent results, or a
    * [[ModuleGate]] rejection — as opposed to an outright name overlap
    * (`overlap.nonEmpty`, `witness = None`). Optional and last so every
    * existing 3-arg `Conflict(overlap, changeA, changeB)` call site is
    * unaffected.
    */
  final case class Conflict(
      overlap: Set[String],
      changeA: Digest,
      changeB: Digest,
      witness: Option[ConflictWitness] = None,
  ):
    def canon: Canon =
      val base = List(
        "overlap" -> Canon.cstrs(overlap.toList.sorted),
        "changeA" -> Canon.CStr(changeA.hex),
        "changeB" -> Canon.CStr(changeB.hex))
      val withWitness = witness match
        case None    => base
        case Some(w) => base :+ ("witness" -> w.canon)
      Canon.cmap(withWitness*)
    def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, Canon.CTag("merge-conflict", canon))
    def render: String =
      if overlap.nonEmpty then s"merge conflict on {${overlap.toList.sorted.mkString(", ")}} between ${changeA.short} and ${changeB.short}"
      else s"merge conflict between ${changeA.short} and ${changeB.short}: ${witness.map(_.render).getOrElse("order-dependent result")}"

  /** Disjoint-footprint branches merge automatically — but footprint
    * disjointness (no shared definition NAME) is a syntactic approximation
    * of commutation, not a proof of it: a whole-module domain gate (SDS's
    * percentage-sum check, LanguageChecker's structural gate, a judgment
    * side condition) can still reject the combination, or — in principle —
    * accept it but produce a different result depending on order, even
    * though the two change-sets never touch the same name. This WITNESSES
    * commutation instead of assuming it: both orders are actually applied
    * against `base`, and the merge only succeeds if both apply cleanly AND
    * agree on the result. An optional [[ModuleGate]] then re-checks the
    * merged module under domain judgments. Either failure surfaces as a
    * `Conflict` (never an exception — a disjoint-footprint pair failing this
    * check is a real, anticipatable outcome, not a broken invariant).
    */
  def threeWay(
      l: ComposedLanguage,
      base: Module,
      changeA: Cst,
      changeB: Cst,
      gate: ModuleGate = ModuleGate.passthrough,
      model: ChangeModel = ChangeModel.default,
  ): Either[Conflict, (Module, Delta.ValidatedChangeSet)] =
    val digA = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(changeA)).digest
    val digB = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(changeB)).digest
    (ChangeAlgebra.checkRecognized(l, changeA, model), ChangeAlgebra.checkRecognized(l, changeB, model)) match
      case (Left(e), _) => Left(Conflict(Set.empty, digA, digB, Some(ConflictWitness.ApplyFailed(Order.Canonical, Delta.Rejection.Malformed(e)))))
      case (_, Left(e)) => Left(Conflict(Set.empty, digA, digB, Some(ConflictWitness.ApplyFailed(Order.Reversed, Delta.Rejection.Malformed(e)))))
      case (Right(()), Right(())) =>
        val fa = ChangeAlgebra.footprint(l, changeA, model)
        val fb = ChangeAlgebra.footprint(l, changeB, model)
        val overlap = fa.intersect(fb)
        if overlap.nonEmpty then Left(Conflict(overlap, digA, digB))
        else
          // canonical order: apply the change-set with the smaller digest first
          val (first, second) = if digA.hex <= digB.hex then (changeA, changeB) else (changeB, changeA)
          val canonical = ChangeAlgebra.compose(l, first, second)
          val reversed = ChangeAlgebra.compose(l, second, first)
          def conflict(w: ConflictWitness) = Conflict(overlap, digA, digB, Some(w))
          (Delta.applyTyped(l, base, canonical, model), Delta.applyTyped(l, base, reversed, model)) match
            case (Left(rej), _) =>
              Left(conflict(ConflictWitness.ApplyFailed(Order.Canonical, rej)))
            case (_, Left(rej)) =>
              Left(conflict(ConflictWitness.ApplyFailed(Order.Reversed, rej)))
            case (Right(canonResult), Right(reversedResult)) =>
              if canonResult._1.digest != reversedResult._1.digest then
                Left(conflict(ConflictWitness.ResultsDiffer(
                  canonResult._1.digest, reversedResult._1.digest)))
              else
                gate(canonResult._1) match
                  case Left(w) => Left(conflict(w))
                  case Right(()) => Right(canonResult)

/** M18: language migrations — revision morphisms between language versions
  * that transport modules and change-sets, kernel-validated against the
  * target language's constructor table.
  */
final case class LangMigration(
    fromLang: Digest,
    toLang: Digest,
    ctorRenames: Map[String, String],
    /** ctor (post-rename) -> (new arity, default child appended to reach it).
      * Tail-padding only — cannot reposition or rename a field. Ignored for
      * any ctor also listed in [[fieldRemap]] (that ctor's shape is fully
      * described by the remap instead).
      */
    arityChanges: Map[String, (Int, Cst)],
    /** ctor (post-rename) -> for each NEW position, how to derive that
      * child: `Left(fieldId)` carries over whichever child the SOURCE
      * ctor's [[CtorDef.fieldIds]] identifies as `fieldId` — found by field
      * identity via [[Migrate.term]]'s `source` language, not by position,
      * so a field that moved or a ctor whose arity changed by more than
      * "grew at the end" still transports correctly. `Right(default)` is a
      * genuinely new field with no old counterpart. A ctor listed here must
      * not also appear in [[arityChanges]] (ambiguous authoring — reject
      * rather than silently pick one).
      */
    fieldRemap: Map[String, List[Either[String, Cst]]] = Map.empty,
):
  def canon: Canon = Canon.cmap(
    "from" -> Canon.CStr(fromLang.hex),
    "to" -> Canon.CStr(toLang.hex),
    "ctorRenames" -> Canon.cmap(ctorRenames.toList.map((k, v) => k -> Canon.CStr(v))*),
    "arityChanges" -> Canon.cmap(arityChanges.toList.map((k, v) =>
      k -> Canon.cmap("arity" -> Canon.CInt(v._1), "default" -> Cst.toCanon(v._2)))*),
    "fieldRemap" -> Canon.cmap(fieldRemap.toList.map((k, slots) =>
      k -> Canon.CList(slots.map {
        case Left(fieldId) => Canon.CTag("carry", Canon.CStr(fieldId))
        case Right(default) => Canon.CTag("default", Cst.toCanon(default))
      }))*))
  def artifact: Artifact = Artifact(ArtifactKind.Migration, Canon.CTag("lang-migration", canon))

object Migrate:
  private val structuralTags = Set("list", "some", "none", "group", "$error")

  /** Transport a term across a migration; validated against the target.
    * `source` is only consulted for [[LangMigration.fieldRemap]] lookups
    * (finding an old ctor's fieldId->position mapping) — the plain
    * rename/arity-pad path never needed the source language, since it
    * walks whatever shape the term already has.
    */
  def term(mig: LangMigration, source: ComposedLanguage, target: ComposedLanguage, t: Cst): Either[String, Cst] =
    def go(t: Cst): Either[String, Cst] = t match
      case Cst.Leaf(_) => Right(t)
      case Cst.Node(c, cs) =>
        val c2 = mig.ctorRenames.getOrElse(c, c)
        cs.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, ch) =>
          for xs <- acc; x <- go(ch) yield xs :+ x
        }.flatMap { kids =>
          mig.fieldRemap.get(c2) match
            case Some(remap) =>
              if mig.arityChanges.contains(c2) then
                Left(s"migration: '$c2' listed in both fieldRemap and arityChanges (ambiguous)")
              else
                source.constructors.get(c) match
                  case None => Left(s"migration: source ctor '$c' not found for fieldRemap of '$c2'")
                  case Some(oldCd) =>
                    remap.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, slot) =>
                      acc.flatMap { xs =>
                        slot match
                          case Right(default) => Right(xs :+ default)
                          case Left(fieldId) =>
                            oldCd.fieldIds.indexOf(Some(fieldId)) match
                              case -1 =>
                                Left(s"migration: '$c' has no field '$fieldId' to carry into '$c2'")
                              case oldPos if oldPos < kids.length =>
                                Right(xs :+ kids(oldPos))
                              case oldPos =>
                                Left(s"migration: '$c' field '$fieldId' position $oldPos " +
                                  s"out of range (${kids.length} children)")
                      }
                    }
            case None =>
              Right(mig.arityChanges.get(c2) match
                case Some((arity, default)) if kids.length < arity =>
                  kids ++ List.fill(arity - kids.length)(default)
                case _ => kids)
        }.flatMap { finalKids =>
          if structuralTags.contains(c2) || target.constructors.contains(c2) then
            Right(Cst.Node(c2, finalKids))
          else Left(s"migrated term uses '$c2', which is not a constructor of '${target.name}'")
        }
    go(t)

  def module(mig: LangMigration, source: ComposedLanguage, target: ComposedLanguage, m: Module): Either[String, Module] =
    m.defs.foldLeft[Either[String, List[(String, Cst)]]](Right(Nil)) { case (acc, (n, t)) =>
      for xs <- acc; t2 <- term(mig, source, target, t).left.map(e => s"def '$n': $e") yield xs :+ (n, t2)
    }.map(Module(_).sorted)

  /** Transport a ΔL change-set across a migration: re-qualify the change tags
    * from the source language name to the target's, migrating embedded terms.
    */
  def changeset(mig: LangMigration, source: ComposedLanguage, target: ComposedLanguage, change: Cst): Either[String, Cst] =
    def retag(t: String): String =
      val suffix = s":${source.name}"
      if t.endsWith(suffix) then t.dropRight(suffix.length) + s":${target.name}" else t
    def go(t: Cst): Either[String, Cst] = t match
      case Cst.Node(c, cs) if c.endsWith(s":${source.name}") =>
        cs.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, ch) =>
          for xs <- acc; x <- goChild(ch) yield xs :+ x
        }.map(Cst.Node(retag(c), _))
      case other => goChild(other)
    def goChild(t: Cst): Either[String, Cst] = t match
      case Cst.Node(c, cs) if c.endsWith(s":${source.name}") => go(t)
      case Cst.Node(c, cs) if structuralTags.contains(c) =>
        cs.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, ch) =>
          for xs <- acc; x <- goChild(ch) yield xs :+ x
        }.map(Cst.Node(c, _))
      case Cst.Leaf(_) => Right(t)
      case termNode    => term(mig, source, target, termNode) // an embedded object-language term
    go(change)

  /** Outcome of transporting a single [[SemanticPath.Step.Field]] hop across
    * a migration, by field identity rather than position.
    */
  enum FieldTransport:
    /** The field survived, at `newPosition` (unchanged from before when the
      * ctor has no [[LangMigration.fieldRemap]] entry, corrected when it does).
      */
    case Transported(newPosition: Int)
    /** No slot in the migrated ctor carries this fieldId anymore — removed,
      * not moved.
      */
    case Deleted(fieldId: String)
    /** More than one slot in [[LangMigration.fieldRemap]] claims this
      * fieldId (the same old field was fanned out into multiple new
      * positions) — there is no single unambiguous new target.
      */
    case Ambiguous(fieldId: String, candidates: List[Int])

  /** Outcome of transporting a whole [[SemanticPath]] across a migration. */
  enum PathTransport:
    case Transported(path: SemanticPath)
    case Deleted(fieldId: String)
    case Ambiguous(fieldId: String, candidates: List[Int])

  /** Where a single fieldId ends up under `mig`'s post-rename ctor `ctor2`:
    * looked up in [[LangMigration.fieldRemap]] when `ctor2` has an entry
    * (reordered/inserted shape), otherwise in `target`'s own (unchanged-
    * position) [[CtorDef.fieldIds]] for ctor2 — a ctor with no remap entry
    * only ever grows via [[LangMigration.arityChanges]]' tail-padding, so
    * existing fields keep their position.
    */
  private def transportField(mig: LangMigration, target: ComposedLanguage, ctor2: String, fieldId: String): FieldTransport =
    mig.fieldRemap.get(ctor2) match
      case Some(remap) =>
        val candidates = remap.zipWithIndex.collect { case (Left(id), i) if id == fieldId => i }
        candidates match
          case Nil          => FieldTransport.Deleted(fieldId)
          case List(single) => FieldTransport.Transported(single)
          case many         => FieldTransport.Ambiguous(fieldId, many)
      case None =>
        target.constructors.get(ctor2).flatMap(_.fieldIds.indexOf(Some(fieldId)) match
          case -1  => None
          case idx => Some(idx)
        ) match
          case Some(idx) => FieldTransport.Transported(idx)
          case None      => FieldTransport.Deleted(fieldId)

  /** Transport a [[SemanticPath]] across a migration by field identity, not
    * position. Every [[SemanticPath.Step.Field]] hop is rebuilt against
    * `target` (ctor renamed via [[LangMigration.ctorRenames]], position
    * resolved via [[transportField]], label re-derived fresh from `target`'s
    * own grammar metadata — the old label may not even apply under a
    * different surface grammar); [[SemanticPath.Step.Index]] hops (list/
    * some/none wrappers) pass through unchanged, since migrations don't
    * (yet) model wrapper-shape changes.
    *
    * A `Step.Field` with no `fieldId` (a positional-only constructor arg)
    * hitting a ctor with a `fieldRemap` entry cannot be transported at all —
    * there is no identity to look up, and the ctor's shape may have
    * genuinely reordered — this is a hard `Left`, not one of the three
    * structured outcomes below (those describe what happens to a field
    * that DOES have an identity).
    *
    * On success, the rebuilt claim is re-verified via [[SemanticPath.verify]]
    * against `migratedRoot` (the term [[term]] already produced) — never
    * trusting the rebuilt steps without replaying them for real.
    */
  def path(
      mig: LangMigration,
      target: ComposedLanguage,
      migratedRoot: Cst,
      p: SemanticPath,
  ): Either[String, PathTransport] =
    if p.language != mig.fromLang then
      Left(s"Migrate.path: path's language ${p.language.short} ≠ migration's fromLang ${mig.fromLang.short}")
    else
      def go(steps: List[SemanticPath.Step]): Either[String, Either[FieldTransport, List[SemanticPath.Step]]] =
        steps match
          case Nil => Right(Right(Nil))
          case (idx: SemanticPath.Step.Index) :: rest =>
            go(rest).map(_.map(idx :: _))
          case (keyed: SemanticPath.Step.KeyedElement) :: rest =>
            // Names a sort + field + value, not a ctor position — untouched
            // by ctorRenames/fieldRemap. Passes through unchanged; the final
            // verify() call still catches a genuinely broken key (the sort's
            // ctor removed, or the key field itself deleted from it).
            go(rest).map(_.map(keyed :: _))
          case SemanticPath.Step.Field(ctor, _, _, None) :: _ if mig.fieldRemap.contains(mig.ctorRenames.getOrElse(ctor, ctor)) =>
            Left(s"Migrate.path: '$ctor' has no fieldId to transport, but its migrated shape is a fieldRemap " +
              "(position alone cannot survive a reorder)")
          case SemanticPath.Step.Field(ctor, _, position, None) :: rest =>
            // No remap for this ctor: position is preserved (arityChanges-only growth).
            go(rest).map(_.map(SemanticPath.Step.Field(mig.ctorRenames.getOrElse(ctor, ctor), None, position, None) :: _))
          case SemanticPath.Step.Field(ctor, _, _, Some(fieldId)) :: rest =>
            val ctor2 = mig.ctorRenames.getOrElse(ctor, ctor)
            transportField(mig, target, ctor2, fieldId) match
              case FieldTransport.Transported(newPos) =>
                val label = target.constructors.get(ctor2).flatMap(_.argLabels.lift(newPos).flatten)
                go(rest).map(_.map(SemanticPath.Step.Field(ctor2, label, newPos, Some(fieldId)) :: _))
              case d: FieldTransport.Deleted     => Right(Left(d))
              case a: FieldTransport.Ambiguous   => Right(Left(a))
      go(p.steps).flatMap {
        case Left(FieldTransport.Deleted(fieldId))              => Right(PathTransport.Deleted(fieldId))
        case Left(FieldTransport.Ambiguous(fieldId, candidates)) => Right(PathTransport.Ambiguous(fieldId, candidates))
        case Left(FieldTransport.Transported(_)) =>
          Left("Migrate.path: internal error, transportField result leaked into the early-exit path")
        case Right(newSteps) =>
          val claim = SemanticPath.Claim(mig.toLang, p.rootSort, newSteps)
          SemanticPath.verify(target, migratedRoot, claim).map(PathTransport.Transported(_))
      }
