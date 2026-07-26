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
