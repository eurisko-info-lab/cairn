package cairn.examples.unison

import cairn.kernel.*
import cairn.workbench.*

/** Unison-inspired pack (M48, §5b, §2c): a name-independent definition store
  * over ALPHA-INVARIANT digests (M2). Definitions are identified by content;
  * names are aliases in a tiny `names` language whose ΔL is the patch
  * language (alias moves). "No builds": rename everything — the underlying
  * definition digests never change, so nothing downstream is invalidated.
  *
  * Stored terms are real `UnisonCore` terms (§2c: this pack is a general-
  * purpose hosted language, peer to STLC/MiniTT — not a domain ADT, not a
  * Unison fork) — the binding discipline is read from `UnisonCore.language`
  * itself rather than hardcoded, so `Alpha.digest`/`normalize` see ALL of
  * its binders (`lam` plus `matchList`/`matchOption`'s pattern binders), not
  * just `lam`.
  *
  * `Store` bodies live in `workbench.Cas` (the SAME generic content-store
  * PKI/Law's `Module`-backed registries and Search's `putTerm` already use,
  * §2c: no language reimplements its own storage) — `digests` is just a
  * local index of which of THIS store's alpha-normalized terms have been
  * added, not a second copy of the term bytes.
  */
object Unison:
  private val spec = UnisonCore.language.binderSpec
  private val varCtor = UnisonCore.language.varCtor.getOrElse("var")

  final case class Store(cas: Cas, digests: Set[Digest]):
    def add(term: Cst): (Digest, Store) =
      val normalized = Alpha.normalize(spec, varCtor)(term)
      val d = cas.put(Artifact(ArtifactKind.Term, Cst.toCanon(normalized))).valueHash
      (d, Store(cas, digests + d))
    def get(d: Digest): Option[Cst] =
      if !digests.contains(d) then None
      else cas.getByDigest(d).toOption.map(a => Cst.fromCanon(a.body))
    def size: Int = digests.size

  object Store:
    def empty: Store = Store(MemCas(), Set.empty)

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
    def resolve(name: String): Option[Cst] =
      names.get(name).flatMap {
        case Cst.Node("ref", List(Cst.Leaf(hex))) => store.get(Digest(hex))
        case _ => None }
    def digestOf(name: String): Option[Digest] =
      names.get(name).collect { case Cst.Node("ref", List(Cst.Leaf(hex))) => Digest(hex) }

  object Codebase:
    def empty: Codebase = Codebase(Store.empty, Module(Nil))

  /** A patch is a ΔL change-set over the names module — alias moves as data. */
  def applyPatch(cb: Codebase, patchSrc: String): Either[String, (Codebase, Delta.ValidatedChangeSet)] =
    for
      dl <- Delta.deltaOf(namesLanguage).left.map(_.map(_.render).mkString("; "))
      change <- Parser.parse(dl.grammar, patchSrc)
      out <- Delta.apply(namesLanguage, cb.names, change)
    yield (Codebase(cb.store, out._1), out._2)
