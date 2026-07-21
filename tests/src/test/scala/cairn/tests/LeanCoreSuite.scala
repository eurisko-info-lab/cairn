package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.systemhandler.EffectContext
/** LeanCore (§5b, §2c amendment, §8b): the formal-methods IR ladder's rung
  * past MiniTT — identity types (`Eq`/`refl`/`subst`) and a minimal
  * environment of checked declarations, on top of everything MiniTT already
  * has. See LeanCore.scala's doc comment for exactly what's in scope
  * (subst/transport, not full path-induction `J`; opaque declarations, not
  * delta-unfolding `def`s) and what isn't (user-declarable inductives,
  * universe polymorphism, real Lean syntax).
  */
class LeanCoreSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContext.bootstrapped())
  private val LeanCore = cairn.examples.leancore.LeanCore(packs)

  test("leancore pack loads from languages/*.cairn at runtime"):
    val raw = packs.loadRaw()
    assert(raw.contains("leancore"), raw.keySet.toString)

  test("leancore composes standalone — no unmet requires"):
    val unmet = packs.unmetRequires("leancore", packs.loadRaw())
    assertEquals(unmet, Set.empty[String])
    LeanCore.ownCompose match
      case Right(lang) =>
        assert(lang.fragments.exists(_.provides.contains("leancore")))
        assert(lang.judgments.contains("hasType"))
        assert(lang.rewriteRules.exists(_.name == "subst-refl"))
      case Left(errs) => fail(errs.map(_.render).mkString)

  test("leancore.cairn text round-trips the meta surface"):
    val fs = packs.requireOwn("leancore")
    val text = Meta.printLanguage("leancore", fs).fold(e => fail(e), identity)
    val back = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
    assertEquals(back._1, "leancore")
    assertEquals(back._2.map(_.digest), fs.map(_.digest))

  test("terms round-trip under the leancore grammar"):
    import LeanCore.*
    val g = language.grammar
    for t <- List(
      sort0, sort1, natTy, zero, succ(zero), app(v("f"), v("a")),
      pi("x", natTy, natTy), lam("x", natTy, v("x")),
      natRec(v("mo"), v("z"), v("s"), zero),
      eqTy(natTy, zero, zero), refl(zero),
      subst(v("P"), refl(zero), zero)
    ) do RoundTrip.check(g, t).fold(e => fail(s"${t.render}: $e"), identity)

  // ---- MiniTT's own ground still holds (same rules, unmodified) ----

  test("Type : Type1; Nat : Type; identity on Nat applied to zero reduces to zero"):
    assert(LeanCore.check(LeanCore.ctxNil, LeanCore.sort0, LeanCore.sort1).isRight)
    assert(LeanCore.check(LeanCore.ctxNil, LeanCore.natTy, LeanCore.sort0).isRight)
    val idNat = LeanCore.lam("x", LeanCore.natTy, LeanCore.v("x"))
    val applied = LeanCore.app(idNat, LeanCore.zero)
    assert(LeanCore.check(LeanCore.ctxNil, applied, LeanCore.natTy).isRight)
    assertEquals(LeanCore.normalize(applied).fold(e => fail(e), identity), LeanCore.zero)

  // ---- identity types: refl, Eq formation ----

  test("refl(zero) : Eq(Nat, zero, zero)"):
    val goal = LeanCore.eqTy(LeanCore.natTy, LeanCore.zero, LeanCore.zero)
    assert(LeanCore.check(LeanCore.ctxNil, LeanCore.refl(LeanCore.zero), goal).isRight)

  test("Eq(Nat, zero, zero) : Type"):
    val eq = LeanCore.eqTy(LeanCore.natTy, LeanCore.zero, LeanCore.zero)
    assert(LeanCore.check(LeanCore.ctxNil, eq, LeanCore.sort0).isRight)

  test("refl(zero) does NOT check against Eq(Nat, zero, succ(zero)) (endpoints differ)"):
    val goal = LeanCore.eqTy(LeanCore.natTy, LeanCore.zero, LeanCore.succ(LeanCore.zero))
    assert(LeanCore.check(LeanCore.ctxNil, LeanCore.refl(LeanCore.zero), goal).isLeft)

  // ---- subst (transport) ----

  test("subst along refl is the identity and reduces accordingly"):
    val motive = LeanCore.lam("x", LeanCore.natTy, LeanCore.natTy)
    val term = LeanCore.subst(motive, LeanCore.refl(LeanCore.zero), LeanCore.zero)
    assert(LeanCore.check(LeanCore.ctxNil, term, LeanCore.natTy).isRight)
    assertEquals(LeanCore.normalize(term).fold(e => fail(e), identity), LeanCore.zero)

  test("subst transports succ(zero) : P(zero) to P(zero) along refl(zero), unreduced motive applications bridge via defeq"):
    // motive that isn't syntactically pre-reduced: P = \x. Nat (ignores x)
    val motive = LeanCore.lam("_", LeanCore.natTy, LeanCore.natTy)
    val px = LeanCore.succ(LeanCore.zero)
    val term = LeanCore.subst(motive, LeanCore.refl(LeanCore.zero), px)
    assert(LeanCore.check(LeanCore.ctxNil, term, LeanCore.natTy).isRight)
    assertEquals(LeanCore.normalize(term).fold(e => fail(e), identity), px)

  test("symmetry: from p : Eq(Nat, a, a) derive Eq(Nat, a, a) via subst with a motive over the equality type itself"):
    val a = LeanCore.zero
    val p = LeanCore.refl(a)
    val symMotive = LeanCore.lam("y", LeanCore.natTy, LeanCore.eqTy(LeanCore.natTy, LeanCore.v("y"), a))
    val symTerm = LeanCore.subst(symMotive, p, LeanCore.refl(a))
    val goal = LeanCore.eqTy(LeanCore.natTy, a, a)
    assert(LeanCore.check(LeanCore.ctxNil, symTerm, goal).isRight)

  test("subst is rejected when px's type doesn't match the motive applied to the proof's left endpoint"):
    val motive = LeanCore.lam("x", LeanCore.natTy, LeanCore.natTy)
    // px : Eq(...) instead of Nat — wrong shape entirely
    val badPx = LeanCore.refl(LeanCore.zero)
    val term = LeanCore.subst(motive, LeanCore.refl(LeanCore.zero), badPx)
    assert(LeanCore.check(LeanCore.ctxNil, term, LeanCore.natTy).isLeft)

  // ---- environment: declarations, name lookup, theorem checking ----

  test("environment: a well-typed declaration extends the environment and is referenceable by name"):
    val env = LeanCore.Environment.empty.extend("two", LeanCore.natTy, LeanCore.succ(LeanCore.succ(LeanCore.zero)))
      .fold(e => fail(e), identity)
    assertEquals(env.get("two").map(_.value), Some(LeanCore.succ(LeanCore.succ(LeanCore.zero))))
    assert(LeanCore.check(env.toCtx, LeanCore.v("two"), LeanCore.natTy).isRight)

  test("environment: a later declaration may reference an earlier one by name"):
    val env = for
      e1 <- LeanCore.Environment.empty.extend("one", LeanCore.natTy, LeanCore.succ(LeanCore.zero))
      e2 <- e1.extend("two", LeanCore.natTy, LeanCore.succ(LeanCore.v("one")))
    yield e2
    val e2 = env.fold(e => fail(e), identity)
    assert(LeanCore.check(e2.toCtx, LeanCore.v("two"), LeanCore.natTy).isRight)

  test("environment: a mistyped declaration is rejected and does not extend the environment"):
    val bad = LeanCore.Environment.empty.extend("oops", LeanCore.natTy, LeanCore.sort0)
    assert(bad.isLeft)

  test("environment: a proof-carrying theorem declaration (Eq witness) checks against its stated type"):
    val env = LeanCore.Environment.empty.extend(
      "zero-refl", LeanCore.eqTy(LeanCore.natTy, LeanCore.zero, LeanCore.zero), LeanCore.refl(LeanCore.zero))
      .fold(e => fail(e), identity)
    assert(LeanCore.check(env.toCtx, LeanCore.v("zero-refl"), LeanCore.eqTy(LeanCore.natTy, LeanCore.zero, LeanCore.zero)).isRight)

  // ---- kernel actually gates, not just search (established idiom) ----

  test("Checker.check rejects a forged derivation directly, independent of search"):
    val forgedConclusion = LeanCore.hasType(LeanCore.ctxNil, LeanCore.zero, LeanCore.sort0) // zero : Type, false
    val forged = Derivation("t-zero", forgedConclusion, Nil)
    assert(Checker.check(LeanCore.checkerCfg, forged).isLeft)

  test("Checker.check rejects a forged Eq derivation with mismatched endpoints"):
    val forgedConclusion = LeanCore.hasType(LeanCore.ctxNil, LeanCore.refl(LeanCore.zero),
      LeanCore.eqTy(LeanCore.natTy, LeanCore.zero, LeanCore.succ(LeanCore.zero)))
    val forged = Derivation("t-refl", forgedConclusion, List(
      Derivation("t-zero", LeanCore.hasType(LeanCore.ctxNil, LeanCore.zero, LeanCore.natTy), Nil)))
    assert(Checker.check(LeanCore.checkerCfg, forged).isLeft)
