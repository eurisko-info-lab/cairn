package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.core.LanguageChecker.TermError
import cairn.examples.stlc.Stlc

/** Unit tests for [[LanguageChecker]] — the structural gate a STRUCTURED edit
  * (`cairn.editDefAtStructured`) needs and a parsed-from-text term never did:
  * a hand-built `Cst` can name an unknown constructor, the wrong arity, or
  * the wrong sort in a way the grammar parser can never produce.
  */
class LanguageCheckerSuite extends munit.FunSuite:
  private val stlc = Stlc.language

  test("unknown constructor is rejected"):
    val bad = Cst.node("bogus", Cst.Leaf("x"))
    LanguageChecker.checkTerm(stlc, stlc.grammar.top, bad) match
      case Left(List(TermError.UnknownConstructor("bogus"))) => ()
      case other => fail(s"expected UnknownConstructor, got $other")

  test("arity mismatch is rejected"):
    val bad = Cst.node("app", Stlc.tru) // app needs 2 children, got 1
    LanguageChecker.checkTerm(stlc, stlc.grammar.top, bad) match
      case Left(List(TermError.ArityMismatch("app", 2, 1))) => ()
      case other => fail(s"expected ArityMismatch, got $other")

  test("sort mismatch is rejected (a Type term where a Term is expected)"):
    val bad = Cst.node("app", Stlc.tBool, Stlc.tru) // tyBool has sort Type, not Term
    LanguageChecker.checkTerm(stlc, stlc.grammar.top, bad) match
      case Left(errs) => assert(errs.exists(_.isInstanceOf[TermError.SortMismatch]), errs)
      case Right(_)   => fail("expected rejection")

  test("primitive-sort leaf (binder position) is accepted"):
    val good = Stlc.lam1("x", Stlc.tBool, Stlc.v("x"))
    assert(LanguageChecker.checkTerm(stlc, stlc.grammar.top, good).isRight)

  test("a genuinely well-formed term is accepted"):
    val good = Stlc.app1(Stlc.idBool, Stlc.tru)
    assert(LanguageChecker.checkTerm(stlc, stlc.grammar.top, good).isRight)

  test("expectedSortAt derives the sort at a path for `edit`, matching the child there"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru) // app(lam(x,Bool,var(x)), true)
    // path [1] = the second child of `app`, i.e. `true`, sort Term
    assertEquals(LanguageChecker.expectedSortAt(stlc, term, List(1)), Right("Term"))
    // path [0, 1] = idBool's second child (its type annotation), sort Type
    assertEquals(LanguageChecker.expectedSortAt(stlc, term, List(0, 1)), Right("Type"))

  test("expectedSortAt fails cleanly on an out-of-range path"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    assert(LanguageChecker.expectedSortAt(stlc, term, List(5)).isLeft)

  test("a grammar category spanning multiple sorts accepts any member (Search: Fact and Intent)"):
    val packs = cairn.runtime.PackLoader(cairn.systemhandler.EffectContext.forPackLoader())
    val search = cairn.examples.search.Search(packs).language
    val fact = Cst.node("fact", Cst.Leaf("some fact")) // sort Fact
    val intent = Cst.node("intent", Cst.Leaf("explore")) // sort Intent — different sort, same top category
    assert(LanguageChecker.checkTerm(search, search.grammar.top, fact).isRight)
    assert(LanguageChecker.checkTerm(search, search.grammar.top, intent).isRight)
