package cairn.user.unison

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** UnisonCore (§5b, §2c): a real term language for the Unison-inspired
  * content-addressed store (`Unison.Store`/`Unison.Codebase`), replacing the
  * borrowed STLC terms M48 originally exercised it with. A small closed set
  * of built-in structural types — `List`/`cons`/`nil`, `Option`/`some`/
  * `none` — plus general application/lambda (STLC's own shape) and ONE
  * minimal ability, `Abort`, with a single unit-typed operation (`abort`)
  * and a non-resumptive `handle ... with ...` (REffectV2's "unit-typed ops"
  * simplicity, not general algebraic effects). NOT a general user-declarable
  * ADT or ability mechanism — same honest, closed-set limitation as MiniTT's
  * hardcoded `Nat`. NOT Unison-surface-compatible (§5b: Unison informs the
  * platform more than it's reproduced by it — this is a Cairn-native
  * calculus merely in Unison's lineage, not a fork).
  *
  * Simply typed (arrow/List/Option/Unit only) — types are never reduced, so
  * unlike MiniTT there is no defeq/t-conv: `Checker`/`Search` need no
  * extensions at all here.
  *
  * `handle`'s reduction is one rule per closed value shape (`handle-unit`,
  * `handle-nil`, `handle-cons`, ...) rather than one generic passthrough
  * rule: `core.TreeEngine.step` tries root rules before recursing into
  * children, so a generic `handle($v,$h) => $v` would fire on an
  * un-reduced body immediately, discarding it before it ever gets the
  * chance to reduce to `abort()`. Enumerating the closed set of value
  * shapes (as MiniTT enumerates `Nat`'s constructors) keeps `handle`
  * honest under normal-order, root-first rewriting.
  */
final class UnisonCore(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("unisoncore")

  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("unisoncore", fragments)

  lazy val language: ComposedLanguage = packs.requireClosed("unisoncore")

  private def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)

  // ---- type builders ----
  def tyUnit: Cst = n("tyUnit")
  def tyList(t: Cst): Cst = n("tyList", t)
  def tyOption(t: Cst): Cst = n("tyOption", t)
  def arrow(a: Cst, b: Cst): Cst = n("arrow", a, b)

  // ---- term builders ----
  def v(x: String): Cst = n("var", Cst.Leaf(x))
  def app(f: Cst, a: Cst): Cst = n("app", f, a)
  def lam(x: String, t: Cst, b: Cst): Cst = n("lam", Cst.Leaf(x), t, b)
  def unit: Cst = n("unit")
  def nil: Cst = n("nil")
  def cons(h: Cst, t: Cst): Cst = n("cons", h, t)
  def none: Cst = n("optNone")
  def some(v: Cst): Cst = n("optSome", v)
  def matchList(scrut: Cst, nilB: Cst, h: String, t: String, consB: Cst): Cst =
    n("matchList", scrut, nilB, Cst.Leaf(h), Cst.Leaf(t), consB)
  def matchOption(scrut: Cst, noneB: Cst, x: String, someB: Cst): Cst =
    n("matchOption", scrut, noneB, Cst.Leaf(x), someB)
  def abort: Cst = n("abort")
  def handle(body: Cst, h: Cst): Cst = n("handle", body, h)

  // ---- contexts + judgment goal (Scala-level plumbing only, same
  // convention as MiniTT/STLC: never parsed/printed) ----
  def ctxNil: Cst = n("ctxNil")
  def ctxCons(x: String, t: Cst, r: Cst): Cst = n("ctxCons", Cst.Leaf(x), t, r)
  def hasType(ctx: Cst, term: Cst, ty: Cst): Cst = n("hasType", ctx, term, ty)

  def checkerCfg: CheckerCfg =
    CheckerCfg(language.judgments.values.toList, binderSpec = language.binderSpec,
      varCtor = language.varCtor.getOrElse("var"))

  /** Untrusted `Search.infer` proposes, the independent `Checker.check`
    * certifies — same "propose, then certify" shape as everywhere else.
    */
  def check(ctx: Cst, term: Cst, ty: Cst): Either[String, Derivation] =
    val cfg = checkerCfg
    Search.infer(cfg, hasType(ctx, term, ty)).flatMap { d =>
      Checker.check(cfg, d).left.map(_.render).map(_ => d) }

  def normalize(term: Cst): Either[String, Cst] = TreeEngine.normalize(language, term)
