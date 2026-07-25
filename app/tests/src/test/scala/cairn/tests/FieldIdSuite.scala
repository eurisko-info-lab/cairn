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

  test("SemanticPath.verify follows a fieldId to its real position when the stored position witness is stale"):
    val lang = Meta.parseFile(namedSrc).fold(e => fail(e), identity)
    val term = Cst.Node("foo", List(Cst.Leaf("a"), Cst.Leaf("b")))
    // "y" is really at position 1 — claim (falsely) that it's at position 0.
    // fieldId is authoritative: verify must resolve the REAL position (1)
    // rather than either failing or trusting the stale claimed position.
    val staleClaim = Claim(lang.digest, lang.grammar.top, List(Step.Field("foo", None, 0, Some("y"))))
    val sp = SemanticPath.verify(lang, term, staleClaim).fold(e => fail(e), identity)
    sp.steps match
      case List(Step.Field("foo", _, 1, Some("y"))) => () // position corrected to 1
      case other => fail(other.toString)
    assertEquals(sp.indices, List(1)) // recovered legacy index also reflects the real position

  test("SemanticPath.verify rejects a fieldId no longer declared on the constructor at all"):
    val lang = Meta.parseFile(namedSrc).fold(e => fail(e), identity)
    val term = Cst.Node("foo", List(Cst.Leaf("a"), Cst.Leaf("b")))
    val claim = Claim(lang.digest, lang.grammar.top, List(Step.Field("foo", None, 0, Some("nonexistent"))))
    assert(SemanticPath.verify(lang, term, claim).swap.exists(_.contains("no longer has a field")))

  test("SemanticPath.verify: fieldId = None still resolves by bare position (unchanged behavior)"):
    val lang = Meta.parseFile(bareSrc).fold(e => fail(e), identity)
    val term = Cst.Node("foo", List(Cst.Leaf("a"), Cst.Leaf("b")))
    val claim = Claim(lang.digest, lang.grammar.top, List(Step.Field("foo", None, 1, None)))
    val sp = SemanticPath.verify(lang, term, claim).fold(e => fail(e), identity)
    sp.steps match
      case List(Step.Field("foo", _, 1, None)) => ()
      case other => fail(other.toString)

  test("duplicate fieldIds within one ctorDecl are rejected"):
    val src = """language t {
      |  fragment t {
      |    sort Bar tree;
      |    ctor foo : Bar(x: X, x: Y);
      |    top Bar;
      |  }
      |}""".stripMargin
    assert(Meta.parseLanguageAst(src).swap.exists(_.contains("duplicate fieldId")))

  test("FragmentCodec.fromCanon defaults fieldIds to Nil when the key is absent (pre-FieldId canon)"):
    val f = singleFragment(namedSrc)
    val canon = f.canon
    val topEntries = canon.asMap
    val strippedCtors = Canon.CList(topEntries("constructors").asList.map { k =>
      Canon.cmap(k.asMap.filterNot(_._1 == "fieldIds").toSeq*)
    })
    val strippedCanon = Canon.cmap(topEntries.map {
      case ("constructors", _) => "constructors" -> strippedCtors
      case other                => other
    }.toSeq*)
    val back = FragmentCodec.fromCanon(strippedCanon)
    assert(back.constructors.nonEmpty)
    assertEquals(back.constructors.map(_.fieldIds), List.fill(back.constructors.length)(Nil))

  test("SemanticPath.Step.fromCanon defaults fieldId to None when the key is absent (pre-FieldId canon)"):
    val field = Step.Field("foo", Some("lbl"), 0, Some("fid"))
    val stripped = field.canon match
      case Canon.CTag("field", m) => Canon.CTag("field", Canon.cmap(m.asMap.filterNot(_._1 == "fieldId").toSeq*))
      case other                  => fail(other.toString)
    assertEquals(Step.fromCanon(stripped), Right(Step.Field("foo", Some("lbl"), 0, None)))
