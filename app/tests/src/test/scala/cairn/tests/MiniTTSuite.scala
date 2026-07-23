package cairn.tests
import cairn.runtime.EffectContexts

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
/** MiniTT (§5b, §2c): minimal dependent type core, checked by the same
  * generic kernel Checker/Search as STLC/PKI. See MiniTT.scala's doc
  * comment for exactly what's in scope (Π types, one hardcoded Nat
  * inductive, a closed 2-level universe hierarchy) and what isn't (no
  * polymorphism, no user inductives, no tactics/elaboration, not Lean).
  */
class MiniTTSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContexts.forPackLoader())
  private val MiniTT = cairn.examples.minitt.MiniTT(packs)

  test("minitt pack loads from languages/*.cairn at runtime"):
    val raw = packs.loadRaw()
    assert(raw.contains("minitt"), raw.keySet.toString)

  test("minitt composes standalone — no unmet requires"):
    val unmet = packs.unmetRequires("minitt", packs.loadRaw())
    assertEquals(unmet, Set.empty[String])
    MiniTT.ownCompose match
      case Right(lang) =>
        assert(lang.fragments.exists(_.provides.contains("minitt")))
        assert(lang.judgments.contains("hasType"))
        assert(lang.rewriteRules.exists(_.name == "beta"))
      case Left(errs) => fail(errs.map(_.render).mkString)

  test("minitt.cairn text round-trips the meta surface"):
    val fs = packs.requireOwn("minitt")
    val text = Meta.printLanguage("minitt", fs).fold(e => fail(e), identity)
    val back = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
    assertEquals(back._1, "minitt")
    assertEquals(back._2.map(_.digest), fs.map(_.digest))

  test("terms round-trip under the minitt grammar"):
    import MiniTT.*
    val g = language.grammar
    for t <- List(
      sort0, sort1, natTy, zero, succ(zero), app(v("f"), v("a")),
      pi("x", natTy, natTy), lam("x", natTy, v("x")),
      natRec(v("mo"), v("z"), v("s"), zero)
    ) do RoundTrip.check(g, t).fold(e => fail(s"${t.render}: $e"), identity)

  // ---- typing: universes ----

  test("Type : Type1"):
    assert(MiniTT.check(MiniTT.ctxNil, MiniTT.sort0, MiniTT.sort1).isRight)

  test("Type1 is untyped (closed 2-level hierarchy, honest limitation)"):
    assert(MiniTT.check(MiniTT.ctxNil, MiniTT.sort1, MiniTT.sort0).isLeft)

  test("Nat : Type"):
    assert(MiniTT.check(MiniTT.ctxNil, MiniTT.natTy, MiniTT.sort0).isRight)

  // ---- typing: Pi / lambda / app ----

  test("Pi x : Nat . Nat : Type1 (t-pi always targets the top universe, no max computation)"):
    val ty = MiniTT.pi("x", MiniTT.natTy, MiniTT.natTy)
    assert(MiniTT.check(MiniTT.ctxNil, ty, MiniTT.sort1).isRight)

  test("identity function on Nat has type Pi x : Nat . Nat"):
    val idNat = MiniTT.lam("x", MiniTT.natTy, MiniTT.v("x"))
    val ty = MiniTT.pi("x", MiniTT.natTy, MiniTT.natTy)
    assert(MiniTT.check(MiniTT.ctxNil, idNat, ty).isRight)

  test("applying identity to zero type-checks to Nat and reduces to zero"):
    val idNat = MiniTT.lam("x", MiniTT.natTy, MiniTT.v("x"))
    val applied = MiniTT.app(idNat, MiniTT.zero)
    assert(MiniTT.check(MiniTT.ctxNil, applied, MiniTT.natTy).isRight)
    val reduced = MiniTT.normalize(applied).fold(e => fail(e), identity)
    assertEquals(reduced, MiniTT.zero)

  test("ill-typed application is rejected (arg type mismatch)"):
    val idNat = MiniTT.lam("x", MiniTT.natTy, MiniTT.v("x"))
    // apply the Nat-identity to a Type-sorted argument instead of a Nat
    val badApp = MiniTT.app(idNat, MiniTT.natTy)
    assert(MiniTT.check(MiniTT.ctxNil, badApp, MiniTT.natTy).isLeft)

  // ---- typing + reduction: the Nat recursor ----

  /** addition via natRec: plus(m, n) = natRec(motive=\_:Nat.Nat, m, succCase, n) */
  private def plus(m: Cst, nTerm: Cst): Cst =
    val motive = MiniTT.lam("_", MiniTT.natTy, MiniTT.natTy)
    val succCase = MiniTT.lam("k", MiniTT.natTy, MiniTT.lam("ih", MiniTT.natTy, MiniTT.succ(MiniTT.v("ih"))))
    MiniTT.natRec(motive, m, succCase, nTerm)

  private def nat(i: Int): Cst = (0 until i).foldLeft(MiniTT.zero)((acc, _) => MiniTT.succ(acc))

  test("natRec recursor: 2 + 2 type-checks to Nat and reduces to 4 (succ^4 zero)"):
    val two = nat(2)
    val term = plus(two, two)
    assert(MiniTT.check(MiniTT.ctxNil, term, MiniTT.natTy).isRight)
    val reduced = MiniTT.normalize(term).fold(e => fail(e), identity)
    assertEquals(reduced, nat(4))

  test("natRec with wrong-arity / mismatched motive is rejected"):
    // motive returns Type instead of Nat -> succCase's claimed type won't defeq
    val badMotive = MiniTT.lam("_", MiniTT.natTy, MiniTT.sort0)
    val term = MiniTT.natRec(badMotive, MiniTT.zero, MiniTT.lam("k", MiniTT.natTy, MiniTT.lam("ih", MiniTT.natTy, MiniTT.zero)), nat(1))
    assert(MiniTT.check(MiniTT.ctxNil, term, MiniTT.natTy).isLeft)

  // ---- kernel actually gates, not just search (established idiom) ----

  test("Checker.check rejects a forged derivation directly, independent of search"):
    val forgedConclusion = MiniTT.hasType(MiniTT.ctxNil, MiniTT.zero, MiniTT.sort0) // zero : Type, false
    val forged = Derivation("t-zero", forgedConclusion, Nil)
    assert(Checker.check(MiniTT.checkerCfg, forged).isLeft)
