package cairn.user.leancore

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** LeanCore — the formal-methods IR ladder's rung past MiniTT (§5b, §2c
  * amendment "executable reference vs. optimized backend", §8b): everything
  * MiniTT has (Π types, a closed 2-level universe hierarchy, one hardcoded
  * `Nat` inductive with its recursor, definitional equality via `$defeq`),
  * plus identity types (`Eq`/`refl`/`subst`) and a minimal environment of
  * checked declarations. Still NOT full CIC, NOT Lean-kernel-compatible in
  * the sense of accepting real Lean syntax or matching its full inference
  * rules — a further, honestly-scoped slice in the same lineage, checked by
  * the SAME generic kernel `Checker`/`Search` as every other pack.
  *
  * `subst` (transport) rather than full path-induction `J`: `J`'s motive
  * depends on the equality proof itself (`(x y : A) (p : Eq(A,x,y)) ->
  * Type`), not just the endpoint (`A -> Type`) — a substantially harder
  * typing rule to search over. `subst`'s single computation rule
  * (`subst($P, refl($a), $px) => $px`) and single typing rule mirror
  * `natRec`'s own `$U where $defeq($U, app($P,$b))` shape exactly — the
  * same pattern already proven out, not a new one.
  *
  * Environment: an ordered, Scala-level list of checked declarations
  * (`name`, type, value) — later ones fold into the SAME `ctxCons` chain
  * `hasType`'s `$ctx-lookup` already walks (MiniTT's own "context is
  * plumbing, never parsed/printed" convention), so no new grammar or
  * engine machinery is needed. Entries are opaque once checked, closer to
  * Lean's `axiom`/`theorem` than a `def` that later terms can unfold —
  * `theorem checking` here means "the value checks against the stated
  * type," not "the body is available for delta-reduction."
  */
final class LeanCore(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("leancore")

  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("leancore", fragments)

  lazy val language: ComposedLanguage = packs.requireClosed("leancore")

  private def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)

  // ---- term builders ----
  def sort0: Cst = n("sort0")
  def sort1: Cst = n("sort1")
  def pi(x: String, a: Cst, b: Cst): Cst = n("pi", Cst.Leaf(x), a, b)
  def lam(x: String, a: Cst, b: Cst): Cst = n("lam", Cst.Leaf(x), a, b)
  def app(f: Cst, a: Cst): Cst = n("app", f, a)
  def v(x: String): Cst = n("var", Cst.Leaf(x))
  def natTy: Cst = n("natTy")
  def zero: Cst = n("zero")
  def succ(t: Cst): Cst = n("succ", t)
  def natRec(mo: Cst, z: Cst, s: Cst, t: Cst): Cst = n("natRec", mo, z, s, t)
  def eqTy(a: Cst, x: Cst, y: Cst): Cst = n("eqTy", a, x, y)
  def refl(a: Cst): Cst = n("refl", a)
  def subst(motive: Cst, p: Cst, px: Cst): Cst = n("subst", motive, p, px)

  // ---- contexts + judgment goal (Scala-level only — never parsed/printed,
  // same convention as MiniTT/STLC: ctxCons/ctxNil are judgment plumbing,
  // not object-language syntax) ----
  def ctxNil: Cst = n("ctxNil")
  def ctxCons(x: String, t: Cst, r: Cst): Cst = n("ctxCons", Cst.Leaf(x), t, r)
  def hasType(ctx: Cst, term: Cst, ty: Cst): Cst = n("hasType", ctx, term, ty)

  private def spec: BinderSpec = language.binderSpec
  private def varCtor: String = language.varCtor.getOrElse("var")

  /** `$subst($body, $x, $value)` can appear (already-instantiated, ground)
    * inside a side-condition argument — `Checker`'s own `instantiate` walks
    * every child regardless of tag, so metavariables inside it are already
    * resolved by the time this extension runs; only the substitution ITSELF
    * (a real term-rewrite, not something the generic checker performs) is
    * this function's job, via the kernel's existing `Binding.subst`.
    */
  private def resolveSubst(t: Cst): Cst = t match
    case Cst.Node("$subst", List(body, Cst.Leaf(x), value)) =>
      Binding.subst(spec, varCtor)(resolveSubst(body), x, resolveSubst(value))
    case Cst.Node(tag, cs) => Cst.Node(tag, cs.map(resolveSubst))
    case other => other

  /** Definitional equality: normalize (β/ι, `core.TreeEngine`, generic)
    * then compare up to alpha (`kernel.Alpha.normalize`, generic, M2).
    */
  def defeq(a: Cst, b: Cst): Either[String, Boolean] =
    for
      na <- TreeEngine.normalize(language, resolveSubst(a))
      nb <- TreeEngine.normalize(language, resolveSubst(b))
    yield Alpha.normalize(spec, varCtor)(na) == Alpha.normalize(spec, varCtor)(nb)

  def extensions: Map[String, List[Cst] => Either[String, Boolean]] = Map(
    "$defeq" -> {
      case List(a, b) => defeq(a, b)
      case other       => Left(s"$$defeq: bad args ${other.map(_.render).mkString(", ")}")
    })

  def checkerCfg: CheckerCfg =
    CheckerCfg(language.judgments.values.toList, extensions = extensions, binderSpec = spec, varCtor = varCtor)

  /** Untrusted `Search.infer` proposes a derivation for `hasType(ctx, term,
    * ty)`, the SAME independent `Checker.check` as everywhere else in this
    * codebase certifies it. `ty` must be supplied concretely (checking
    * mode) — `$defeq` requires its arguments already ground, same
    * documented limitation as MiniTT.
    */
  def check(ctx: Cst, term: Cst, ty: Cst): Either[String, Derivation] =
    val cfg = checkerCfg
    Search.infer(cfg, hasType(ctx, term, ty)).flatMap { d =>
      Checker.check(cfg, d).left.map(_.render).map(_ => d) }

  def normalize(term: Cst): Either[String, Cst] = TreeEngine.normalize(language, term)

  // ---- environment: an ordered list of checked declarations, later ones
  // visible to earlier-declared names via the same ctxCons chain hasType
  // already walks (§2c amendment: "environment declarations", "theorem
  // checking") ----
  final case class Decl(name: String, ty: Cst, value: Cst, transparent: Boolean = false)

  final case class Environment(decls: List[Decl]):
    /** The context every declaration (and any term checked "in" this
      * environment) sees: later declarations shadow earlier same-named
      * ones, matching `$ctx-lookup`'s own first-hit-wins walk.
      */
    def toCtx: Cst = decls.foldRight(ctxNil)((d, acc) => ctxCons(d.name, d.ty, acc))

    def get(name: String): Option[Decl] = decls.findLast(_.name == name)

    /** Host-side delta-unfold: substitute every transparent decl's name for
      * its value (declaration order). Opaque (theorem-style) decls are
      * untouched — same discipline as [[resolveSubst]], not a kernel delta rule.
      */
    def unfold(t: Cst): Cst =
      decls.foldLeft(t) { case (acc, d) =>
        if d.transparent then Binding.subst(spec, varCtor)(acc, d.name, d.value) else acc
      }

    /** Check `value : ty` against the CURRENT environment's context, then
      * append it. Default opaque (closer to Lean `theorem`); pass
      * `transparent = true` for def-style delta-unfold via [[unfold]].
      */
    def extend(
        name: String, ty: Cst, value: Cst, transparent: Boolean = false
    ): Either[String, Environment] =
      check(toCtx, value, ty).map(_ => Environment(decls :+ Decl(name, ty, value, transparent)))

  object Environment:
    val empty: Environment = Environment(Nil)

  def checkUnfolding(env: Environment, term: Cst, ty: Cst): Either[String, Derivation] =
    check(env.toCtx, env.unfold(term), ty)
