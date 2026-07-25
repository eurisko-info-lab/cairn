package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.core.SemanticPath.{Claim, Step}
import cairn.examples.stlc.Stlc

/** Unit tests for [[SemanticPath]] — the Kernel-checked typed replacement
  * for raw `List[Int]` structural-edit paths. `LanguageCheckerSuite`'s
  * `expectedSortAt` tests establish the ground truth this must agree with.
  */
class SemanticPathSuite extends munit.FunSuite:
  private val stlc = Stlc.language

  test("fromLegacyPath agrees with LanguageChecker.expectedSortAt on focus sort"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru) // app(lam(x,Bool,var(x)), true)
    val sp1 = SemanticPath.fromLegacyPath(stlc, term, List(1)).fold(e => fail(e), identity)
    assertEquals(sp1.focusSort, "Term")
    assertEquals(sp1.indices, List(1))
    val sp2 = SemanticPath.fromLegacyPath(stlc, term, List(0, 1)).fold(e => fail(e), identity)
    assertEquals(sp2.focusSort, "Type")
    assertEquals(sp2.indices, List(0, 1))

  test("fromLegacyPath recovers Field steps naming the real constructor and position"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val sp = SemanticPath.fromLegacyPath(stlc, term, List(0, 1)).fold(e => fail(e), identity)
    // step 0: into `app`'s child 0 (the lam); step 1: into `lam`'s child 1 (its type).
    // Stlc is hand-authored Scala, not .cairn-composed, so labels are None throughout —
    // position remains the authoritative resolver regardless.
    sp.steps match
      case List(Step.Field("app", None, 0), Step.Field("lam", None, 1)) => ()
      case other => fail(other.toString)

  test("fromLegacyPath fails clean (out of range) matching expectedSortAt's contract"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    assert(SemanticPath.fromLegacyPath(stlc, term, List(5)).swap.exists(_.contains("out of range")))

  test("fromLegacyPath fails clean descending into a leaf"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    // [0,0,0] = lam's child 0 is the binder name leaf "x" — one level too deep.
    assert(SemanticPath.fromLegacyPath(stlc, term, List(0, 0, 0)).swap.exists(_.contains("leaf")))

  test("verify round-trips a Claim built from fromLegacyPath's own steps"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val sp = SemanticPath.fromLegacyPath(stlc, term, List(0, 1)).fold(e => fail(e), identity)
    val claim = Claim(stlc.digest, stlc.grammar.top, sp.steps)
    val verified = SemanticPath.verify(stlc, term, claim).fold(e => fail(e), identity)
    assertEquals(verified.focusSort, sp.focusSort)
    assertEquals(verified.indices, sp.indices)

  test("verify rejects a claimed constructor that doesn't match the actual node"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val claim = Claim(stlc.digest, stlc.grammar.top, List(Step.Field("bogus", None, 0)))
    assert(SemanticPath.verify(stlc, term, claim).swap.exists(_.contains("expected constructor")))

  test("verify rejects a claimed label the constructor's derived metadata doesn't have"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    // Stlc's `app` ctor has no derived labels (hand-authored, not .cairn-composed) —
    // any claimed label at a real position must be rejected.
    val claim = Claim(stlc.digest, stlc.grammar.top, List(Step.Field("app", Some("fn"), 0)))
    assert(SemanticPath.verify(stlc, term, claim).swap.exists(_.contains("not labeled")))

  test("verify rejects a language-digest mismatch"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val wrongDigest = Digest.of(Canon.CStr("not-the-real-language"))
    val claim = Claim(wrongDigest, stlc.grammar.top, List(Step.Field("app", None, 0)))
    assert(SemanticPath.verify(stlc, term, claim).swap.exists(_.contains("language mismatch")))

  test("verify rejects a focusSort claim that doesn't match the walk"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val claim = Claim(stlc.digest, stlc.grammar.top, List(Step.Field("app", None, 1)), focusSort = Some("Type"))
    assert(SemanticPath.verify(stlc, term, claim).swap.exists(_.contains("expected focus sort")))

  test("verify accepts a matching focusSort claim"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val claim = Claim(stlc.digest, stlc.grammar.top, List(Step.Field("app", None, 1)), focusSort = Some("Term"))
    assert(SemanticPath.verify(stlc, term, claim).isRight)

  test("verify rejects an Index step at a real constructor node (not a list/some/none wrapper)"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val claim = Claim(stlc.digest, stlc.grammar.top, List(Step.Index(0)))
    assert(SemanticPath.verify(stlc, term, claim).swap.exists(_.contains("not legal")))

  test("Step canon round-trips for both Field and Index"):
    val field = Step.Field("mixture", Some("of"), 0)
    val index = Step.Index(2)
    assertEquals(Step.fromCanon(field.canon), Right(field))
    assertEquals(Step.fromCanon(index.canon), Right(index))
    val fieldNoLabel = Step.Field("app", None, 1)
    assertEquals(Step.fromCanon(fieldNoLabel.canon), Right(fieldNoLabel))

  test("SemanticPath canon carries language/rootSort/steps/focusSort"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val sp = SemanticPath.fromLegacyPath(stlc, term, List(1)).fold(e => fail(e), identity)
    val c = sp.canon
    assertEquals(c.field("focusSort").asStr, "Term")
    assertEquals(c.field("rootSort").asStr, stlc.grammar.top)
    assertEquals(c.field("language").asStr, stlc.digest.hex)
