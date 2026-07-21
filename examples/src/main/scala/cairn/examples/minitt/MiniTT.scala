package cairn.examples.minitt

import cairn.kernel.*
import cairn.workbench.*
import cairn.compute.TreeEngine
import cairn.core.Search

/** MiniTT — a minimal, closed dependent type core (§5b, §2c; the "Formal-
  * methods IR ladder" rung, §8b), checked by the SAME generic kernel
  * `Checker`/`Search` as STLC/PKI's judgments. NOT full CIC (§8 anti-goal):
  * a 2-level, non-cumulative universe hierarchy (`Type : Type1`, `Type1`
  * itself untyped), Π types, and exactly one hardcoded inductive (`Nat`
  * with its recursor) — no universe polymorphism, no user-declarable
  * inductives, no tactics/elaboration. NOT Lean-surface-compatible: Lean
  * stays a Rosetta *projection* target only (§4.10); this is a genuinely
  * different, Cairn-native calculus merely inspired by the same lineage.
  *
  * The one thing STLC's typing judgment never needed: type CONVERSION.
  * `t-app`'s result type is `B[x:=a]` — true substitution, not a value
  * `Checker.matchPat`'s pure syntactic pattern matching can produce on its
  * own — so it's checked via a `$defeq` side condition (same extension
  * mechanism as PKI's `$sig-ok`/`$anchor`), which normalizes both sides
  * (`compute.TreeEngine`, unmodified/generic) and compares up to alpha
  * (`kernel.Alpha.normalize`, unmodified/generic — both already existed).
  *
  * `t-lam` vs `t-lam-conv`: a lambda's own domain annotation and its Pi
  * type's domain slot are the same metavariable in `t-lam`, so checking a
  * lambda against an expected type requires them to unify SYNTACTICALLY —
  * fine when the expected domain is unbound (synthesis) or written the
  * same way, but too strict when it's merely defeq (e.g. an unreduced
  * `app(motive, k)` standing for `Nat`, as `t-rec`'s step-function premise
  * produces). `Search.infer`'s per-rule unification has no way to fall
  * back to `$defeq` mid-unify, so `t-lam-conv` is a second rule, tried only
  * once `t-lam`'s stricter match fails, that decouples the two slots and
  * bridges them with an explicit `$defeq` — same "declare the alternative
  * as another rule" idiom the judgment already uses elsewhere.
  */
object MiniTT:
  lazy val fragments: List[Fragment] = PackLoader.requireOwn("minitt")

  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("minitt", fragments)

  lazy val language: ComposedLanguage = PackLoader.requireClosed("minitt")

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

  // ---- contexts + judgment goal (Scala-level only — never parsed/printed,
  // same convention as STLC/Claims: ctxCons/ctxNil are judgment plumbing,
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

  /** Definitional equality: normalize (β/ι, `compute.TreeEngine`, generic)
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
    * codebase certifies it — "propose, then certify," same two-step as
    * `PkiMax.validate`/`Search.checkWellFormed`. `ty` must be supplied
    * concretely (checking mode) — `$defeq` requires its arguments already
    * ground, so synthesis of an unbound type slot through it isn't
    * supported (documented limitation, not a bug: see MiniTTSuite).
    */
  def check(ctx: Cst, term: Cst, ty: Cst): Either[String, Derivation] =
    val cfg = checkerCfg
    Search.infer(cfg, hasType(ctx, term, ty)).flatMap { d =>
      Checker.check(cfg, d).left.map(_.render).map(_ => d) }

  def normalize(term: Cst): Either[String, Cst] = TreeEngine.normalize(language, term)
