package cairn.tests

import cairn.kernel.*
import cairn.core.*

/** Unit tests for PR9's pack grammar surface: `provider <alias> = language
  * <name>;` declares an alias for a judgment-providing language; `validate
  * ...;` declarations (added incrementally) express [[ModuleStructural.Spec]]s
  * as pack data. This file grows alongside each new `validateX` grammar kind.
  */
class ValidationDeclSuite extends munit.FunSuite:

  private def providerSrc = """language t {
    |  fragment t {
    |    sort Foo tree;
    |    ctor foo : Foo(bar: Bar);
    |    provider clp = language eu-clp;
    |    top Foo;
    |  }
    |}""".stripMargin

  private def singleFragment(src: String): Fragment =
    Meta.parseLanguageAst(src).fold(e => fail(e), (_, fs) => fs match
      case List(f) => f
      case other   => fail(s"expected exactly one fragment, got ${other.length}"))

  test("providerDecl parses into Fragment.providers"):
    val f = singleFragment(providerSrc)
    assertEquals(f.providers, Map("clp" -> "eu-clp"))

  test("encode/elaborate round-trips providerDecl"):
    val f = singleFragment(providerSrc)
    val back = Meta.elaborateFragment(Meta.encode(f)).fold(e => fail(e), identity)
    assertEquals(back, f)

  test("providers merge across composed fragments and are part of ComposedLanguage identity"):
    val f = singleFragment(providerSrc)
    val lang = Compose.compose("t", List(f)).fold(e => fail(e.map(_.render).mkString), identity)
    assertEquals(lang.providers, Map("clp" -> "eu-clp"))
    val noProviderSrc = providerSrc.replace("provider clp = language eu-clp;\n    ", "")
    val langNoProvider = Meta.parseFile(noProviderSrc).fold(e => fail(e), identity)
    assert(lang.digest != langNoProvider.digest, "providers must affect the composed language's digest")

  /** One `validate` declaration per ModuleStructural.Spec kind — the exact
    * shapes SDS's own validationSpecs use (see Sds.scala), expressed as pack
    * grammar instead of Scala. Exercises every validateX production's
    * grammar, elaboration, and encode inverse at once.
    */
  private def allKindsSrc = """language t {
    |  fragment t {
    |    sort Foo tree;
    |    ctor foo : Foo(bar: Bar);
    |    provider clp = language eu-clp;
    |    validate sumleavesatmost mixture [1] 100 "mixture";
    |    validate uniquetuples translationState [[0], [1]] "translationState";
    |    validate nonemptyleaves ctor [1, 2] ["a", "b"];
    |    validate definedref product 1 "product";
    |    validate definedrefs shadow [0, 1] "shadow";
    |    validate definedleaflist product 2 "product";
    |    validate definednodelistrefs mixture 0 [0] "mixture";
    |    validate leafok euSection 0 satisfies clp.sectionNumberOk;
    |    validate leafvalueinctorfield translationState 0 ["phrase", "corpusPhrase"] 0 "translationState";
    |    validate reftagin sectionFieldShadow 0 ["euSection", "identificationSection"] "sectionFieldShadow";
    |    validate uniquetupleslist euSection 1 [[0], [1]] "euSection" ["sectionField", "sectionFieldRef"];
    |    validate uniquetupleslist noTags 1 [[0]] "noTags";
    |    validate listchilddefinedrefs euSection 1 [sectionFieldRef: [[2]]] "euSection";
    |    validate keyedlocaleoverlay identificationSection 6 ["synonyms", "recommendedUse"] fieldLocale fieldLocaleRef 0 1 2 "identificationSection";
    |    validate outlinenums outline 2 [fromleaf "euSection" 0, bytag [s1: 1]] satisfies clp.sectionNumberOk "outline";
    |    top Foo;
    |  }
    |}""".stripMargin

  test("all 14 validateX kinds parse into Fragment.validations"):
    val f = singleFragment(allKindsSrc)
    assertEquals(f.validations.length, 15) // 14 Spec kinds, uniquetupleslist appears twice (with/without childTags)

  test("all 14 validateX kinds round-trip through actual TEXT (print + re-parse), not just Cst-level encode/elaborate"):
    // Cst-level round-trips (encode/elaborate, above) never exercise
    // Printer.print, so a print-rule bug (e.g. a raw Cst.Leaf printing
    // unquoted where a quoted string was required) can hide behind them —
    // this is the exact gap that let one such bug through earlier.
    val f = singleFragment(allKindsSrc)
    val text = Meta.printLanguage("t", List(f)).fold(e => fail(e), identity)
    val (name, fs) = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
    assertEquals(name, "t")
    val reparsed = fs match
      case List(f2) => f2
      case other    => fail(s"expected exactly one fragment, got ${other.length}")
    assertEquals(reparsed, f)

  test("resolved (non-alias) validateX kinds decode to the exact expected Spec"):
    val f = singleFragment(allKindsSrc)
    val specs = f.validations.flatMap(c => scala.util.Try(ModuleStructural.Spec.fromCanon(c)).toOption)
    assert(specs.contains(ModuleStructural.Spec.SumLeavesAtMost("mixture", List(1), 100, "mixture")))
    assert(specs.contains(ModuleStructural.Spec.DefinedRef("product", 1, "product")))
    assert(specs.contains(ModuleStructural.Spec.UniqueTuplesInList(
      "euSection", 1, List(List(0), List(1)), "euSection", Some(Set("sectionField", "sectionFieldRef")))))
    assert(specs.contains(ModuleStructural.Spec.UniqueTuplesInList("noTags", 1, List(List(0)), "noTags", None)))

  test("LeafOk/OutlineNums decls are stored pre-resolution, carrying the provider ALIAS not a digest"):
    val f = singleFragment(allKindsSrc)
    val unresolved = f.validations.collect {
      case c @ Canon.CTag("LeafOkUnresolved", _)      => c
      case c @ Canon.CTag("OutlineNumsUnresolved", _) => c
    }
    assertEquals(unresolved.length, 2)
    val leafOk = unresolved.collectFirst { case Canon.CTag("LeafOkUnresolved", m) => m }.get
    assertEquals(leafOk.field("alias").asStr, "clp")
    assertEquals(leafOk.field("judgmentName").asStr, "sectionNumberOk")

  test("encode/elaborate round-trips all 14 validateX kinds"):
    val f = singleFragment(allKindsSrc)
    val back = Meta.elaborateFragment(Meta.encode(f)).fold(e => fail(e), identity)
    assertEquals(back, f)

  private def judgmentProviderSrc = """language jp {
    |  fragment jp {
    |    sort Dummy tree;
    |    ctor dummy : Dummy;
    |    judgment myJudgment {
    |      rule ok : |- myJudgment("ok");
    |    }
    |    top Dummy;
    |  }
    |}""".stripMargin

  private def judgmentTargetSrc = """language t2 {
    |  fragment t2 {
    |    sort Foo tree;
    |    ctor foo : Foo(bar: Bar);
    |    provider jp = language jp;
    |    validate leafok foo 0 satisfies jp.myJudgment;
    |    top Foo;
    |  }
    |}""".stripMargin

  test("ValidationModelLoader.resolve: a second node reconstructs the ValidationModel from artifacts and reproduces validation"):
    def freshTarget(): ComposedLanguage = Meta.parseFile(judgmentTargetSrc).fold(e => fail(e), identity)
    def freshResolver(langName: String): ComposedLanguage =
      if langName == "jp" then Meta.parseFile(judgmentProviderSrc).fold(e => fail(e), identity)
      else fail(s"unknown provider language '$langName'")

    // "Node 1": elaborates + resolves once.
    val model1 = ValidationModelLoader.resolve(freshTarget(), freshResolver)

    // "Node 2": completely independent re-parse + re-resolve (fresh Scala
    // objects throughout, no shared state with node 1) — identity must match exactly.
    val model2 = ValidationModelLoader.resolve(freshTarget(), freshResolver)
    assertEquals(model2, model1)
    assertEquals(model2.digest, model1.digest)

    // Reproduces the SAME validation result a fresh module is checked against.
    val provider = freshResolver("jp")
    val okModule = Module(List("a" -> Cst.node("foo", Cst.Leaf("ok"))))
    val badModule = Module(List("a" -> Cst.node("foo", Cst.Leaf("nope"))))
    val resolverMap: Map[Digest, ComposedLanguage] = Map(provider.digest -> provider)
    assertEquals(ModuleStructural.run(okModule, model2.specs, resolverMap.get), Nil)
    assert(ModuleStructural.run(badModule, model2.specs, resolverMap.get).nonEmpty)

  test("ValidationModelLoader.resolve throws on an undeclared provider alias"):
    val danglingSrc = judgmentTargetSrc.replace("provider jp = language jp;\n    ", "")
    val target = Meta.parseFile(danglingSrc).fold(e => fail(e), identity)
    intercept[RuntimeException](ValidationModelLoader.resolve(target, _ => fail("resolver should not be called")))
