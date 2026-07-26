package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.core.SemanticPath.{Claim, Step}

/** Unit tests for keyed collection paths: `key <sort> by <field>;` declares
  * that a sort's elements in a list are identified by a field value, not
  * position, and [[SemanticPath.Step.KeyedElement]] resolves a list child
  * that way.
  */
class KeyedElementSuite extends munit.FunSuite:

  private def mixtureSrc = """language t {
    |  fragment t {
    |    sort Component tree;
    |    sort Mixture tree;
    |    ctor component : Component(ref: Ref, pct: Pct);
    |    ctor mixture : Mixture(components: Components);
    |    key Component by ref;
    |    top Mixture;
    |  }
    |}""".stripMargin

  private def singleFragment(src: String): Fragment =
    Meta.parseLanguageAst(src).fold(e => fail(e), (_, fs) => fs match
      case List(f) => f
      case other   => fail(s"expected exactly one fragment, got ${other.length}"))

  private def component(ref: String, pct: String): Cst =
    Cst.Node("component", List(Cst.Leaf(ref), Cst.Leaf(pct)))

  test("keyDecl parses into Fragment.keys"):
    val f = singleFragment(mixtureSrc)
    assertEquals(f.keys, List(KeyDef("Component", "ref")))

  test("encode/elaborate round-trips keyDecl"):
    val f = singleFragment(mixtureSrc)
    val back = Meta.elaborateFragment(Meta.encode(f)).fold(e => fail(e), identity)
    assertEquals(back, f)

  test("keys are part of ComposedLanguage and canon/digest identity"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    assertEquals(lang.keys, Map("Component" -> KeyDef("Component", "ref")))
    val noKeySrc = mixtureSrc.replace("key Component by ref;\n    ", "")
    val langNoKey = Meta.parseFile(noKeySrc).fold(e => fail(e), identity)
    assertNotEquals(lang.digest, langNoKey.digest)

  test("Step.KeyedElement resolves the matching list element, not by position"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val list = Cst.Node("list", List(component("acetone", "50"), component("water", "50")))
    val mixture = Cst.Node("mixture", List(list))
    val claim = Claim(lang.digest, lang.grammar.top, List(
      Step.Field("mixture", None, 0, Some("components")),
      Step.KeyedElement("Component", "ref", "acetone")))
    val sp = SemanticPath.verify(lang, mixture, claim).fold(e => fail(e), identity)
    assertEquals(sp.focusSort, "Component")
    assertEquals(sp.indices, List(0, 0))

  test("Step.KeyedElement survives reordering the list (the actual point)"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    // Same elements, "acetone" now second instead of first.
    val list = Cst.Node("list", List(component("water", "50"), component("acetone", "50")))
    val mixture = Cst.Node("mixture", List(list))
    val claim = Claim(lang.digest, lang.grammar.top, List(
      Step.Field("mixture", None, 0, Some("components")),
      Step.KeyedElement("Component", "ref", "acetone")))
    val sp = SemanticPath.verify(lang, mixture, claim).fold(e => fail(e), identity)
    assertEquals(sp.indices, List(0, 1)) // found at position 1 this time, same claim

  test("PR11: legacy paths recover keyed identity and different keyed elements do not overlap"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val mixture = Cst.Node("mixture", List(Cst.Node("list", List(
      component("acetone", "50"), component("water", "50")))))
    val acetone = SemanticPath.fromLegacyPath(lang, mixture, List(0, 0, 1)).fold(e => fail(e), identity)
    val water = SemanticPath.fromLegacyPath(lang, mixture, List(0, 1, 1)).fold(e => fail(e), identity)
    assert(acetone.steps.exists {
      case Step.KeyedElement("Component", "ref", "acetone") => true
      case _ => false
    })
    assert(!SemanticLocation.overlaps(
      SemanticLocation.Subtree("sheet", acetone), SemanticLocation.Subtree("sheet", water)))

  test("Step.KeyedElement: no matching key is a structured error, not a crash"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val list = Cst.Node("list", List(component("acetone", "50")))
    val mixture = Cst.Node("mixture", List(list))
    val claim = Claim(lang.digest, lang.grammar.top, List(
      Step.Field("mixture", None, 0, Some("components")),
      Step.KeyedElement("Component", "ref", "toluene")))
    assert(SemanticPath.verify(lang, mixture, claim).swap.exists(_.contains("no 'Component' with ref='toluene'")))

  test("Step.KeyedElement: a duplicate key is a structured error, not a silent pick"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val list = Cst.Node("list", List(component("acetone", "50"), component("acetone", "30")))
    val mixture = Cst.Node("mixture", List(list))
    val claim = Claim(lang.digest, lang.grammar.top, List(
      Step.Field("mixture", None, 0, Some("components")),
      Step.KeyedElement("Component", "ref", "acetone")))
    assert(SemanticPath.verify(lang, mixture, claim).swap.exists(_.contains("duplicate key")))

  test("Step.KeyedElement: a claim naming the wrong keyField is rejected"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val list = Cst.Node("list", List(component("acetone", "50")))
    val mixture = Cst.Node("mixture", List(list))
    val claim = Claim(lang.digest, lang.grammar.top, List(
      Step.Field("mixture", None, 0, Some("components")),
      Step.KeyedElement("Component", "pct", "50"))) // declared key is "ref", not "pct"
    assert(SemanticPath.verify(lang, mixture, claim).swap.exists(_.contains("declared key field is 'ref'")))

  test("Step.KeyedElement: a sort with no key declaration is rejected"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val mixture = Cst.Node("mixture", List(Cst.Node("list", Nil)))
    val claim = Claim(lang.digest, lang.grammar.top, List(
      Step.Field("mixture", None, 0, Some("components")),
      Step.KeyedElement("Mixture", "x", "y"))) // "Mixture" has no key decl at all
    assert(SemanticPath.verify(lang, mixture, claim).swap.exists(_.contains("no declared key")))

  test("Step.KeyedElement: not legal at a non-list node"):
    val lang = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val mixture = component("acetone", "50") // not a list wrapper at all
    val claim = Claim(lang.digest, lang.grammar.top, List(Step.KeyedElement("Component", "ref", "acetone")))
    assert(SemanticPath.verify(lang, mixture, claim).swap.exists(_.contains("expected a list")))

  test("Migrate.path: KeyedElement passes through unchanged across a migration"):
    val v1 = Meta.parseFile(mixtureSrc).fold(e => fail(e), identity)
    val v2 = Meta.parseFile(mixtureSrc.replace("Pct", "Pct2")).fold(e => fail(e), identity)
    val list1 = Cst.Node("list", List(component("acetone", "50"), component("water", "50")))
    val mixture1 = Cst.Node("mixture", List(list1))
    val claim = Claim(v1.digest, v1.grammar.top, List(
      Step.Field("mixture", None, 0, Some("components")),
      Step.KeyedElement("Component", "ref", "acetone")))
    val p1 = SemanticPath.verify(v1, mixture1, claim).fold(e => fail(e), identity)
    val mig = LangMigration(v1.digest, v2.digest, Map.empty, Map.empty)
    val mixture2 = Migrate.term(mig, v1, v2, mixture1).fold(e => fail(e), identity)
    Migrate.path(mig, v2, mixture2, p1) match
      case Right(Migrate.PathTransport.Transported(p2)) =>
        p2.steps match
          case List(Step.Field("mixture", _, 0, Some("components")), Step.KeyedElement("Component", "ref", "acetone")) => ()
          case other => fail(s"unexpected transported steps: $other")
      case other => fail(other.toString)
