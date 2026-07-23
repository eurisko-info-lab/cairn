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

  /** Definition names an atomic change touches (writes) and reads. */
  def footprint(l: ComposedLanguage, change: Cst): Set[String] =
    changeItems(l, change).flatMap {
      case Cst.Node(t, Cst.Leaf(n) :: _) if t == tag(l, "add") || t == tag(l, "replace") ||
          t == tag(l, "remove") || t == tag(l, "edit") => List(n)
      case Cst.Node(t, List(Cst.Leaf(from), Cst.Leaf(to), fp)) if t == tag(l, "rename") =>
        from :: to :: (fp match
          case Cst.Node("some", List(Cst.Node("list", xs))) => xs.collect { case Cst.Leaf(x) => x }
          case _ => Nil)
      case _ => Nil
    }.toSet

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
  def commutes(l: ComposedLanguage, a: Cst, b: Cst): Boolean =
    footprint(l, a).intersect(footprint(l, b)).isEmpty

  /** Inverse of a change-set RELATIVE to the module it was applied to:
    * apply(invert(c)) ∘ apply(c) == id. Requires the base module because
    * remove/replace/edit inverses need the overwritten content.
    */
  def invert(l: ComposedLanguage, base: Module, change: Cst): Either[String, Cst] =
    val items = changeItems(l, change)
    // walk forward, collecting each op's inverse against the current module
    val zero: Either[String, (Module, List[Cst])] = Right((base, Nil))
    items.foldLeft(zero) { (acc, item) =>
      acc.flatMap { (m, invs) =>
        val inverse: Either[String, Cst] = item match
          case Cst.Node(t, List(Cst.Leaf(n), _)) if t == tag(l, "add") =>
            Right(Cst.node(tag(l, "remove"), Cst.Leaf(n)))
          case Cst.Node(t, List(Cst.Leaf(n), _)) if t == tag(l, "replace") =>
            m.get(n).toRight(s"invert replace: '$n' not in module")
              .map(old => Cst.node(tag(l, "replace"), Cst.Leaf(n), old))
          case Cst.Node(t, List(Cst.Leaf(n))) if t == tag(l, "remove") =>
            m.get(n).toRight(s"invert remove: '$n' not in module")
              .map(old => Cst.node(tag(l, "add"), Cst.Leaf(n), old))
          case Cst.Node(t, List(Cst.Leaf(from), Cst.Leaf(to), fp)) if t == tag(l, "rename") =>
            // inverse rename; the footprint transports to the new names
            val transported = fp match
              case Cst.Node("some", List(Cst.Node("list", xs))) =>
                Cst.node("some", Cst.Node("list", xs))
              case other => other
            Right(Cst.Node(tag(l, "rename"), List(Cst.Leaf(to), Cst.Leaf(from), transported)))
          case Cst.Node(t, List(Cst.Leaf(n), pathCst, _)) if t == tag(l, "edit") =>
            for
              old <- m.get(n).toRight(s"invert edit: '$n' not in module")
              sub <- Delta.subtreeAt(old, Delta.pathOf(pathCst))
            yield Cst.Node(tag(l, "edit"), List(Cst.Leaf(n), pathCst, sub))
          case other => Left(s"cannot invert: ${other.render}")
        for
          inv <- inverse
          applied <- Delta.apply(l, m, changeset(l, List(item))).map(_._1)
        yield (applied, inv :: invs) // inverses accumulate in REVERSE order
      }
    }.map((_, invs) => changeset(l, invs))

/** M17: three-way semantic merge over change histories. Disjoint-footprint
  * branches merge automatically; overlaps surface as a structured conflict
  * artifact naming both change-sets — never silent, never textual.
  */
object Merge:
  /** `witness`, when present, explains a conflict that footprint disjointness
    * alone didn't predict — either order failed to apply, or the two orders
    * applied to different results — as opposed to an outright name overlap
    * (`overlap.nonEmpty`, `witness = None`). Optional and last so every
    * existing 3-arg `Conflict(overlap, changeA, changeB)` call site is
    * unaffected.
    */
  final case class Conflict(overlap: Set[String], changeA: Digest, changeB: Digest, witness: Option[String] = None):
    def canon: Canon = Canon.cmap(
      "overlap" -> Canon.cstrs(overlap.toList.sorted),
      "changeA" -> Canon.CStr(changeA.hex),
      "changeB" -> Canon.CStr(changeB.hex))
    def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, Canon.CTag("merge-conflict", canon))
    def render: String =
      if overlap.nonEmpty then s"merge conflict on {${overlap.toList.sorted.mkString(", ")}} between ${changeA.short} and ${changeB.short}"
      else s"merge conflict between ${changeA.short} and ${changeB.short}: ${witness.getOrElse("order-dependent result")}"

  /** Disjoint-footprint branches merge automatically — but footprint
    * disjointness (no shared definition NAME) is a syntactic approximation
    * of commutation, not a proof of it: a whole-module domain gate (SDS's
    * percentage-sum check, LanguageChecker's structural gate, a judgment
    * side condition) can still reject the combination, or — in principle —
    * accept it but produce a different result depending on order, even
    * though the two change-sets never touch the same name. This WITNESSES
    * commutation instead of assuming it: both orders are actually applied
    * against `base`, and the merge only succeeds if both apply cleanly AND
    * agree on the result. Either failure surfaces as a `Conflict` (never an
    * exception — a disjoint-footprint pair failing this check is a real,
    * anticipatable outcome, not a broken invariant).
    */
  def threeWay(l: ComposedLanguage, base: Module, changeA: Cst, changeB: Cst): Either[Conflict, (Module, Delta.ValidatedChangeSet)] =
    val fa = ChangeAlgebra.footprint(l, changeA)
    val fb = ChangeAlgebra.footprint(l, changeB)
    val overlap = fa.intersect(fb)
    val digA = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(changeA)).digest
    val digB = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(changeB)).digest
    if overlap.nonEmpty then Left(Conflict(overlap, digA, digB))
    else
      // canonical order: apply the change-set with the smaller digest first
      val (first, second) = if digA.hex <= digB.hex then (changeA, changeB) else (changeB, changeA)
      val canonical = ChangeAlgebra.compose(l, first, second)
      val reversed = ChangeAlgebra.compose(l, second, first)
      def conflict(detail: String) = Conflict(overlap, digA, digB, Some(detail))
      for
        canonResult <- Delta.apply(l, base, canonical)
          .left.map(e => conflict(s"disjoint footprints but merge failed to apply: $e"))
        reversedResult <- Delta.apply(l, base, reversed)
          .left.map(e => conflict(s"disjoint footprints but reverse order failed to apply: $e"))
        result <- Either.cond(canonResult._1.digest == reversedResult._1.digest, canonResult,
          conflict(s"disjoint footprints but order-dependent result: " +
            s"${canonResult._1.digest.short} vs ${reversedResult._1.digest.short}"))
      yield result

/** M18: language migrations — revision morphisms between language versions
  * that transport modules and change-sets, kernel-validated against the
  * target language's constructor table.
  */
final case class LangMigration(
    fromLang: Digest,
    toLang: Digest,
    ctorRenames: Map[String, String],
    /** ctor (post-rename) -> (new arity, default child appended to reach it) */
    arityChanges: Map[String, (Int, Cst)],
):
  def canon: Canon = Canon.cmap(
    "from" -> Canon.CStr(fromLang.hex),
    "to" -> Canon.CStr(toLang.hex),
    "ctorRenames" -> Canon.cmap(ctorRenames.toList.map((k, v) => k -> Canon.CStr(v))*),
    "arityChanges" -> Canon.cmap(arityChanges.toList.map((k, v) =>
      k -> Canon.cmap("arity" -> Canon.CInt(v._1), "default" -> Cst.toCanon(v._2)))*))
  def artifact: Artifact = Artifact(ArtifactKind.Migration, Canon.CTag("lang-migration", canon))

object Migrate:
  private val structuralTags = Set("list", "some", "none", "group", "$error")

  /** Transport a term across a migration; validated against the target. */
  def term(mig: LangMigration, target: ComposedLanguage, t: Cst): Either[String, Cst] =
    def go(t: Cst): Either[String, Cst] = t match
      case Cst.Leaf(_) => Right(t)
      case Cst.Node(c, cs) =>
        val c2 = mig.ctorRenames.getOrElse(c, c)
        cs.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, ch) =>
          for xs <- acc; x <- go(ch) yield xs :+ x
        }.flatMap { kids =>
          val padded = mig.arityChanges.get(c2) match
            case Some((arity, default)) if kids.length < arity =>
              kids ++ List.fill(arity - kids.length)(default)
            case _ => kids
          if structuralTags.contains(c2) || target.constructors.contains(c2) then
            Right(Cst.Node(c2, padded))
          else Left(s"migrated term uses '$c2', which is not a constructor of '${target.name}'")
        }
    go(t)

  def module(mig: LangMigration, target: ComposedLanguage, m: Module): Either[String, Module] =
    m.defs.foldLeft[Either[String, List[(String, Cst)]]](Right(Nil)) { case (acc, (n, t)) =>
      for xs <- acc; t2 <- term(mig, target, t).left.map(e => s"def '$n': $e") yield xs :+ (n, t2)
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
      case termNode    => term(mig, target, termNode) // an embedded object-language term
    go(change)
