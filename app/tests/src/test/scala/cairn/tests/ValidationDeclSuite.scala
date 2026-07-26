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
