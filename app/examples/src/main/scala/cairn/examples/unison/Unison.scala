package cairn.examples.unison

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{CasEffects, EffectContext, MemCas}

/** Unison-inspired pack (M48, §5b, §2c): a name-independent definition store
  * over ALPHA-INVARIANT digests (M2). Definitions are identified by content;
  * names are aliases in a tiny `names` language whose ΔL is the patch
  * language (alias moves). "No builds": rename everything — the underlying
  * definition digests never change, so nothing downstream is invalidated.
  *
  * Stored terms are real `UnisonCore` terms (§2c: this pack is a general-
  * purpose hosted language, peer to STLC/MiniTT — not a domain ADT, not a
  * Unison fork) — the binding discipline is read from `core.language`
  * itself rather than hardcoded, so `Alpha.digest`/`normalize` see ALL of
  * its binders (`lam` plus `matchList`/`matchOption`'s pattern binders), not
  * just `lam`.
  *
  * `Store` bodies live in `Cas` via [[CasEffects]] (the SAME generic
  * content-store PKI/Law's `Module`-backed registries and Search's `putTerm`
  * already use, §2c: no language reimplements its own storage) — `digests` is
  * just a local index of which of THIS store's alpha-normalized terms have
  * been added, not a second copy of the term bytes.
  *
  * Hash-linked [[UnisonCore.call]] edges form a thin call graph: renames never
  * touch caller digests; type-preserving edit propagation is **detection**
  * (recheck after callee signature change), not auto-patch.
  */
final class Unison(core: UnisonCore):
  private val spec = core.language.binderSpec
  private val varCtor = core.language.varCtor.getOrElse("var")

  final case class Store(
      cas: Cas,
      digests: Set[Digest],
      types: Map[Digest, Cst],
      ctx: EffectContext,
  ):
    def add(term: Cst): (Digest, Store) =
      val normalized = Alpha.normalize(spec, varCtor)(term)
      val d = CasEffects.put(cas, Artifact(ArtifactKind.Term, Cst.toCanon(normalized)), ctx)
        .fold(e => throw RuntimeException(e.toString), _.valueHash)
      (d, Store(cas, digests + d, types, ctx))
    def addTyped(term: Cst, ty: Cst): (Digest, Store) =
      val (d, s2) = add(term)
      (d, s2.copy(types = s2.types + (d -> ty)))
    def get(d: Digest): Option[Cst] =
      if !digests.contains(d) then None
      else CasEffects.get(cas, d, ctx).toOption.map(a => Cst.fromCanon(a.body))
    def typeOf(d: Digest): Option[Cst] = types.get(d)
    def size: Int = digests.size

  object Store:
    def empty: Store = Store(MemCas(), Set.empty, Map.empty, EffectContext.forCas())

  /** The names language: name -> ref <digest>. Its ΔL is the patch language. */
  val namesFragment: Fragment = Fragment(
    name = "names",
    provides = List("names"),
    requires = Nil,
    sorts = List(SortDef("Ref", SortMode.Tree)),
    constructors = List(CtorDef("ref", "Ref", List("Hash"))),
    grammar = GrammarPart(
      keywords = List("ref"),
      categories = List(CategorySpec("refTerm", List(
        ConstructorSpec("ref", List(Elem.Tok("ref"), Elem.StrLeaf))))),
      printRules = List(
        PrintRule("ref", List(PrintSeg.Lit("ref"), PrintSeg.Space, PrintSeg.StrField(0)))),
      top = Some("refTerm")))

  lazy val namesLanguage: ComposedLanguage =
    Compose.compose("names", List(namesFragment)).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")), identity)

  def refTerm(d: Digest): Cst = Cst.node("ref", Cst.Leaf(d.hex))

  /** A codebase: content-addressed definitions + a names module. */
  final case class Codebase(store: Store, names: Module):
    def define(name: String, term: Cst): Codebase =
      val (d, s2) = store.add(term)
      Codebase(s2, Module(names.defs :+ (name -> refTerm(d))).sorted)
    def defineTyped(name: String, term: Cst, ty: Cst): Codebase =
      val (d, s2) = store.addTyped(term, ty)
      Codebase(s2, Module(names.defs :+ (name -> refTerm(d))).sorted)
    def resolve(name: String): Option[Cst] =
      names.get(name).flatMap {
        case Cst.Node("ref", List(Cst.Leaf(hex))) => store.get(Digest(hex))
        case _ => None }
    def digestOf(name: String): Option[Digest] =
      names.get(name).collect { case Cst.Node("ref", List(Cst.Leaf(hex))) => Digest(hex) }
    /** Hash-linked callees referenced by `call($h)` in the named term. */
    def dependencies(name: String): Set[Digest] =
      resolve(name).map(core.callTargets).getOrElse(Set.empty)

    /** Reverse of [[dependencies]]: names whose call graph references `target`
      * directly ("what calls this," not just "what does this call"). Derived
      * by scanning [[names]] rather than a separately maintained index — there
      * is nothing to invalidate, since renames never change any digest this
      * depends on.
      */
    def dependents(target: Digest): Set[String] =
      names.defs.map(_._1).filter(n => dependencies(n).contains(target)).toSet

    /** [[dependents]] keyed by name instead of digest. */
    def dependentsOf(name: String): Set[String] =
      digestOf(name).map(dependents).getOrElse(Set.empty)

    /** Everything that calls `target`, directly or transitively through
      * another dependent. */
    def transitiveDependents(target: Digest): Set[String] =
      def go(frontier: Set[Digest], acc: Set[String]): Set[String] =
        val found = frontier.flatMap(dependents) -- acc
        if found.isEmpty then acc
        else go(found.flatMap(digestOf), acc ++ found)
      go(Set(target), Set.empty)

    /** Edit-propagation cascade: after `changed`'s recorded type is edited in
      * place (same digest, a corrected [[Store.typeOf]] entry — "callee
      * signature changed"), recheck every transitive dependent's OWN top-level
      * `hasType` goal against the codebase's CURRENT typeOf, via
      * [[transitiveDependents]] instead of leaving that scan to the caller.
      *
      * Each name's verdict is a genuinely independent, shallow $call-type
      * check against its *direct* dependency's recorded type — not a
      * recursive re-verification of its whole transitive closure. So a name
      * two hops from `changed` reports its own real, current answer: it only
      * flips to `Left` once ITS direct dependency's recorded type itself
      * changes (e.g. because a human already patched it in response to this
      * cascade). That is the honest shape of hash-linked detection: bodies
      * are never auto-rewritten, so fixing a break is a deliberate edit, and
      * this cascade's job is finding everyone who might need one — not
      * pretending the whole graph is invalid until they do.
      */
    def recheckAfterEdit(changed: Digest): Map[String, Either[String, Unit]] =
      transitiveDependents(changed).flatMap { name =>
        for
          d <- digestOf(name)
          term <- resolve(name)
          ty <- store.typeOf(d)
        yield name -> core.check(core.ctxNil, term, ty, store.typeOf).map(_ => ())
      }.toMap

  object Codebase:
    def empty: Codebase = Codebase(Store.empty, Module(Nil))

  /** A patch is a ΔL change-set over the names module — alias moves as data. */
  def applyPatch(cb: Codebase, patchSrc: String): Either[String, (Codebase, Delta.ValidatedChangeSet)] =
    for
      dl <- Delta.deltaOf(namesLanguage).left.map(_.map(_.render).mkString("; "))
      change <- Parser.parse(dl.grammar, patchSrc)
      out <- Delta.apply(namesLanguage, cb.names, change)
    yield (Codebase(cb.store, out._1), out._2)
