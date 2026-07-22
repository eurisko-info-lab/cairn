package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.systemhandler.EffectContext
/** UnisonCore (§5b, §2c): a real term language — closed List/Option ADTs,
  * pattern matching, a minimal non-resumptive `Abort` ability — checked by
  * the same generic kernel Checker/Search as STLC/PKI/MiniTT, and stored by
  * the SAME `Unison.Store`/`Codebase`/`applyPatch` machinery M48 already
  * exercised with borrowed STLC terms. See UnisonCore.scala's doc comment
  * for exactly what's in scope (closed ADTs, one ability, simply typed) and
  * what isn't (no user-declarable ADTs/abilities, no Unison-surface syntax).
  */
class UnisonCoreSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContext.forPackLoader())
  private val UnisonCore = cairn.examples.unison.UnisonCore(packs)
  private val Unison = cairn.examples.unison.Unison(UnisonCore)

  test("unisoncore pack loads from languages/*.cairn at runtime"):
    val raw = packs.loadRaw()
    assert(raw.contains("unisoncore"), raw.keySet.toString)

  test("unisoncore composes standalone — no unmet requires"):
    val unmet = packs.unmetRequires("unisoncore", packs.loadRaw())
    assertEquals(unmet, Set.empty[String])
    UnisonCore.ownCompose match
      case Right(lang) =>
        assert(lang.fragments.exists(_.provides.contains("unisoncore")))
        assert(lang.judgments.contains("hasType"))
        assert(lang.rewriteRules.exists(_.name == "beta"))
      case Left(errs) => fail(errs.map(_.render).mkString)

  test("unisoncore.cairn text round-trips the meta surface"):
    val fs = packs.requireOwn("unisoncore")
    val text = Meta.printLanguage("unisoncore", fs).fold(e => fail(e), identity)
    val back = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
    assertEquals(back._1, "unisoncore")
    assertEquals(back._2.map(_.digest), fs.map(_.digest))

  test("terms round-trip under the unisoncore grammar"):
    import UnisonCore.*
    val g = language.grammar
    for t <- List(
      unit, nil, none, some(unit), cons(unit, nil),
      app(lam("x", tyUnit, v("x")), unit),
      matchList(nil, unit, "h", "t", unit),
      matchOption(none, unit, "x", unit),
      abort, handle(abort, unit)
    ) do RoundTrip.check(g, t).fold(e => fail(s"${t.render}: $e"), identity)

  // ---- typing + reduction: lambda calculus core (STLC's own shape) ----

  test("identity function on Unit has type Unit -> Unit"):
    val idUnit = UnisonCore.lam("x", UnisonCore.tyUnit, UnisonCore.v("x"))
    val ty = UnisonCore.arrow(UnisonCore.tyUnit, UnisonCore.tyUnit)
    assert(UnisonCore.check(UnisonCore.ctxNil, idUnit, ty).isRight)

  test("applying identity to unit type-checks to Unit and reduces to unit"):
    val idUnit = UnisonCore.lam("x", UnisonCore.tyUnit, UnisonCore.v("x"))
    val applied = UnisonCore.app(idUnit, UnisonCore.unit)
    assert(UnisonCore.check(UnisonCore.ctxNil, applied, UnisonCore.tyUnit).isRight)
    val reduced = UnisonCore.normalize(applied).fold(e => fail(e), identity)
    assertEquals(reduced, UnisonCore.unit)

  // ---- typing + reduction: List/Option ----

  test("Cons(unit, Nil) : List(Unit)"):
    val l = UnisonCore.cons(UnisonCore.unit, UnisonCore.nil)
    assert(UnisonCore.check(UnisonCore.ctxNil, l, UnisonCore.tyList(UnisonCore.tyUnit)).isRight)

  test("Nil is checked against the goal's element type (checking mode, no annotation needed)"):
    assert(UnisonCore.check(UnisonCore.ctxNil, UnisonCore.nil, UnisonCore.tyList(UnisonCore.tyUnit)).isRight)

  test("Some(unit) : Option(Unit); None : Option(Unit)"):
    assert(UnisonCore.check(UnisonCore.ctxNil, UnisonCore.some(UnisonCore.unit), UnisonCore.tyOption(UnisonCore.tyUnit)).isRight)
    assert(UnisonCore.check(UnisonCore.ctxNil, UnisonCore.none, UnisonCore.tyOption(UnisonCore.tyUnit)).isRight)

  test("cons with mismatched element type is rejected"):
    val bad = UnisonCore.cons(UnisonCore.unit, UnisonCore.some(UnisonCore.unit))
    assert(UnisonCore.check(UnisonCore.ctxNil, bad, UnisonCore.tyList(UnisonCore.tyUnit)).isLeft)

  test("matchList: head-or-default over Cons(unit, Nil) type-checks and reduces to Some(unit)"):
    // match scrut { Nil -> None ; Cons(h, t) -> Some(h) }
    val scrut = UnisonCore.cons(UnisonCore.unit, UnisonCore.nil)
    val m = UnisonCore.matchList(scrut, UnisonCore.none, "h", "t", UnisonCore.some(UnisonCore.v("h")))
    assert(UnisonCore.check(UnisonCore.ctxNil, m, UnisonCore.tyOption(UnisonCore.tyUnit)).isRight)
    val reduced = UnisonCore.normalize(m).fold(e => fail(e), identity)
    assertEquals(reduced, UnisonCore.some(UnisonCore.unit))

  test("matchList over Nil reduces to the nil branch (None)"):
    val m = UnisonCore.matchList(UnisonCore.nil, UnisonCore.none, "h", "t", UnisonCore.some(UnisonCore.v("h")))
    assert(UnisonCore.check(UnisonCore.ctxNil, m, UnisonCore.tyOption(UnisonCore.tyUnit)).isRight)
    val reduced = UnisonCore.normalize(m).fold(e => fail(e), identity)
    assertEquals(reduced, UnisonCore.none)

  test("matchOption unwraps Some, defaults on None"):
    val m1 = UnisonCore.matchOption(UnisonCore.some(UnisonCore.unit), UnisonCore.nil, "x", UnisonCore.cons(UnisonCore.v("x"), UnisonCore.nil))
    assert(UnisonCore.check(UnisonCore.ctxNil, m1, UnisonCore.tyList(UnisonCore.tyUnit)).isRight)
    assertEquals(UnisonCore.normalize(m1).fold(e => fail(e), identity), UnisonCore.cons(UnisonCore.unit, UnisonCore.nil))

    val m2 = UnisonCore.matchOption(UnisonCore.none, UnisonCore.nil, "x", UnisonCore.cons(UnisonCore.v("x"), UnisonCore.nil))
    assertEquals(UnisonCore.normalize(m2).fold(e => fail(e), identity), UnisonCore.nil)

  test("matchList with mismatched branch result types is rejected"):
    // nil branch : Unit, cons branch : List(Unit) — no single $R satisfies both
    val bad = UnisonCore.matchList(UnisonCore.nil, UnisonCore.unit, "h", "t", UnisonCore.nil)
    assert(UnisonCore.check(UnisonCore.ctxNil, bad, UnisonCore.tyUnit).isLeft)

  // ---- typing + reduction: the Abort ability ----

  test("abort checks at any goal type (checking-mode bottom, documented limitation)"):
    assert(UnisonCore.check(UnisonCore.ctxNil, UnisonCore.abort, UnisonCore.tyUnit).isRight)
    assert(UnisonCore.check(UnisonCore.ctxNil, UnisonCore.abort, UnisonCore.tyList(UnisonCore.tyUnit)).isRight)

  test("handle(abort, fallback) type-checks and reduces to the fallback"):
    val h = UnisonCore.handle(UnisonCore.abort, UnisonCore.unit)
    assert(UnisonCore.check(UnisonCore.ctxNil, h, UnisonCore.tyUnit).isRight)
    assertEquals(UnisonCore.normalize(h).fold(e => fail(e), identity), UnisonCore.unit)

  test("handle(non-aborting body, fallback) reduces to the body's value, not the fallback"):
    val idOpt = UnisonCore.lam("x", UnisonCore.tyOption(UnisonCore.tyUnit), UnisonCore.v("x"))
    val body = UnisonCore.app(idOpt, UnisonCore.some(UnisonCore.unit))
    val h = UnisonCore.handle(body, UnisonCore.none)
    assert(UnisonCore.check(UnisonCore.ctxNil, h, UnisonCore.tyOption(UnisonCore.tyUnit)).isRight)
    assertEquals(UnisonCore.normalize(h).fold(e => fail(e), identity), UnisonCore.some(UnisonCore.unit))

  test("handle requires the body and the fallback to share a type"):
    val bad = UnisonCore.handle(UnisonCore.abort, UnisonCore.nil)
    assert(UnisonCore.check(UnisonCore.ctxNil, bad, UnisonCore.tyUnit).isLeft)

  // ---- kernel actually gates, not just search (established idiom) ----

  test("Checker.check rejects a forged derivation directly, independent of search"):
    val forgedConclusion = UnisonCore.hasType(UnisonCore.ctxNil, UnisonCore.unit, UnisonCore.tyList(UnisonCore.tyUnit))
    val forged = Derivation("t-unit", forgedConclusion, Nil)
    assert(Checker.check(UnisonCore.checkerCfg, forged).isLeft)

  // ---- M48-style store/codebase round-trip, now with real UnisonCore terms ----

  test("a real UnisonCore term (not borrowed STLC) round-trips through Store/Codebase"):
    // Store normalizes to an alpha-canonical form (M2) — resolve returns
    // that canonical term, not the literal one passed to define.
    val idUnit = UnisonCore.lam("x", UnisonCore.tyUnit, UnisonCore.v("x"))
    val canonical = Alpha.normalize(UnisonCore.language.binderSpec, "var")(idUnit)
    val cb = Unison.Codebase.empty.define("identity", idUnit)
    assertEquals(cb.resolve("identity"), Some(canonical))

  test("alpha-equivalent UnisonCore lambdas dedup to one stored definition"):
    val idX = UnisonCore.lam("x", UnisonCore.tyUnit, UnisonCore.v("x"))
    val idY = UnisonCore.lam("y", UnisonCore.tyUnit, UnisonCore.v("y"))
    val cb = Unison.Codebase.empty.define("identity", idX).define("id2", idY)
    assertEquals(cb.store.size, 1)
    assertEquals(cb.digestOf("identity"), cb.digestOf("id2"))

  test("a term using matchList's pattern binders also dedups correctly under alpha-equivalence"):
    val head1 = UnisonCore.matchList(UnisonCore.v("xs"), UnisonCore.none, "h", "t", UnisonCore.some(UnisonCore.v("h")))
    val head2 = UnisonCore.matchList(UnisonCore.v("xs"), UnisonCore.none, "a", "b", UnisonCore.some(UnisonCore.v("a")))
    val cb = Unison.Codebase.empty.define("headOf", head1).define("headOf2", head2)
    assertEquals(cb.store.size, 1) // h/t and a/b are alpha-equivalent binder names
    assertEquals(cb.digestOf("headOf"), cb.digestOf("headOf2"))

  test("patch renames a stored UnisonCore definition's alias; the term itself is untouched"):
    val idUnit = UnisonCore.lam("x", UnisonCore.tyUnit, UnisonCore.v("x"))
    val canonical = Alpha.normalize(UnisonCore.language.binderSpec, "var")(idUnit)
    val cb = Unison.Codebase.empty.define("identity", idUnit)
    val (cb2, _) = Unison.applyPatch(cb, "{ rename identity to id footprint []; }").fold(e => fail(e), identity)
    assertEquals(cb2.resolve("id"), Some(canonical))
    assertEquals(cb2.store, cb.store)

  test("hash-linked call: rename callee alias; caller digest + deps unchanged"):
    val idTy = UnisonCore.arrow(UnisonCore.tyUnit, UnisonCore.tyUnit)
    val idUnit = UnisonCore.lam("x", UnisonCore.tyUnit, UnisonCore.v("x"))
    val cb0 = Unison.Codebase.empty.defineTyped("identity", idUnit, idTy)
    val idDigest = cb0.digestOf("identity").get
    // $call-type needs a ground type — check the call alone (not under app metavars).
    val chk = UnisonCore.check(UnisonCore.ctxNil, UnisonCore.call(idDigest), idTy, cb0.store.typeOf)
    assert(chk.isRight, chk.toString)
    assert(UnisonCore.check(UnisonCore.ctxNil, UnisonCore.call(idDigest), UnisonCore.tyUnit, cb0.store.typeOf).isLeft)
    val caller = UnisonCore.app(UnisonCore.call(idDigest), UnisonCore.unit)
    val unfolded = UnisonCore.unfoldCalls(cb0.store.get)(caller)
    assert(UnisonCore.check(UnisonCore.ctxNil, unfolded, UnisonCore.tyUnit).isRight)
    val cb1 = cb0.defineTyped("useIdentity", caller, UnisonCore.tyUnit)
    val callerDigestBefore = cb1.digestOf("useIdentity").get
    assertEquals(cb1.dependencies("useIdentity"), Set(idDigest))
    val (cb2, _) = Unison.applyPatch(cb1, "{ rename identity to id footprint []; }").fold(e => fail(e), identity)
    assertEquals(cb2.digestOf("useIdentity"), Some(callerDigestBefore))
    assertEquals(cb2.dependencies("useIdentity"), Set(idDigest))
    assertEquals(
      UnisonCore.normalize(UnisonCore.unfoldCalls(cb2.store.get)(caller)).fold(e => fail(e), identity),
      UnisonCore.unit)
