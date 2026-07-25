package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.core.SemanticPath.{Claim, Step}

/** Unit tests for FieldId: persistent, canon-included, `.cairn`-declared
  * constructor-argument identity (`ctorDecl`'s `name: Sort` form), independent
  * of the grammar-derived, canon-excluded `CtorDef.argLabels`.
  */
class FieldIdSuite extends munit.FunSuite:

  private def namedSrc = """language t {
    |  fragment t {
    |    sort Bar tree;
    |    ctor foo : Bar(x: X, y: Y);
    |    top Bar;
    |  }
    |}""".stripMargin

  private def bareSrc = """language t {
    |  fragment t {
    |    sort Bar tree;
    |    ctor foo : Bar(X, Y);
    |    top Bar;
    |  }
    |}""".stripMargin

  private def mixedSrc = """language t {
    |  fragment t {
    |    sort Bar tree;
    |    ctor foo : Bar(x: X, Y);
    |    top Bar;
    |  }
    |}""".stripMargin

  private def singleFragment(src: String): Fragment =
    Meta.parseLanguageAst(src).fold(e => fail(e), (_, fs) => fs match
      case List(f) => f
      case other   => fail(s"expected exactly one fragment, got ${other.length}"))

  test("named ctorDecl args produce CtorDef.fieldIds"):
    val foo = singleFragment(namedSrc).constructors.find(_.name == "foo").getOrElse(fail("no 'foo' ctor"))
    assertEquals(foo.argSorts, List("X", "Y"))
    assertEquals(foo.fieldIds, List(Some("x"), Some("y")))

  test("bare ctorDecl args produce fieldIds = Nil, not an all-None list"):
    val foo = singleFragment(bareSrc).constructors.find(_.name == "foo").getOrElse(fail("no 'foo' ctor"))
    assertEquals(foo.argSorts, List("X", "Y"))
    assertEquals(foo.fieldIds, Nil)

  test("named and bare args may mix within one ctorDecl"):
    val foo = singleFragment(mixedSrc).constructors.find(_.name == "foo").getOrElse(fail("no 'foo' ctor"))
    assertEquals(foo.fieldIds, List(Some("x"), None))

  test("encode/elaborate round-trips fieldIds for named, bare, and mixed forms"):
    for src <- List(namedSrc, bareSrc, mixedSrc) do
      val f = singleFragment(src)
      val back = Meta.elaborateFragment(Meta.encode(f)).fold(e => fail(e), identity)
      assertEquals(back, f, src)

  test("fieldIds are part of canon: named vs bare ctorDecl (same argSorts) differ in digest"):
    val named = singleFragment(namedSrc)
    val bare = singleFragment(bareSrc)
    // same shape apart from fieldIds — isolates the assertion to fieldIds' own effect on canon
    assertEquals(named.constructors.map(_.argSorts), bare.constructors.map(_.argSorts))
    assertNotEquals(named.digest, bare.digest)

  test("SemanticPath.fromLegacyPath / verify use fieldId end-to-end on a .cairn-declared language"):
    val lang = Meta.parseFile(namedSrc).fold(e => fail(e), identity)
    assertEquals(lang.constructors("foo").fieldIds, List(Some("x"), Some("y")))
    val term = Cst.Node("foo", List(Cst.Leaf("a"), Cst.Leaf("b")))

    val sp = SemanticPath.fromLegacyPath(lang, term, List(0)).fold(e => fail(e), identity)
    sp.steps match
      case List(Step.Field("foo", _, 0, Some("x"))) => ()
      case other => fail(other.toString)

    val okClaim = Claim(lang.digest, lang.grammar.top, List(Step.Field("foo", None, 0, Some("x"))))
    assert(SemanticPath.verify(lang, term, okClaim).isRight)

    val badClaim = Claim(lang.digest, lang.grammar.top, List(Step.Field("foo", None, 0, Some("wrong"))))
    assert(SemanticPath.verify(lang, term, badClaim).swap.exists(_.contains("fieldId")))
