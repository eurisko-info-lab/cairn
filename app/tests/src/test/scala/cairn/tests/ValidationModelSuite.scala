package cairn.tests

import cairn.kernel.*
import cairn.core.*

/** Canon round-trip + identity coverage for [[ValidationModel]] — PR9's
  * analog of `ChangeModelCanonSuite` for PR6's `ChangeModel`.
  */
class ValidationModelSuite extends munit.FunSuite:
  import ModuleStructural.Spec

  private val targetLang = Digest.of(Canon.CStr("target-lang"))
  private val providerA = Digest.of(Canon.CStr("provider-a"))
  private val providerB = Digest.of(Canon.CStr("provider-b"))

  private val model1 = ValidationModel(
    targetLang,
    List(
      Spec.DefinedRef("foo", 0, "foo"),
      Spec.LeafOk("bar", 1, JudgmentRef(providerA, "j1"))),
    List(providerA))

  test("ValidationModel: canon round-trip from encoded artifact bytes"):
    val bytes = Canon.encode(model1.canon)
    val decoded = ValidationModel.fromCanon(Canon.decode(bytes).fold(e => fail(e), identity))
    assertEquals(decoded, model1)
    assertEquals(decoded.digest, model1.digest)

  test("ValidationModel: has a stable artifact digest"):
    assertEquals(model1.digest, model1.digest)

  test("ValidationModel: same judgment name under a DIFFERENT provider-language digest produces a different model identity"):
    val model2 = ValidationModel(
      targetLang,
      List(
        Spec.DefinedRef("foo", 0, "foo"),
        Spec.LeafOk("bar", 1, JudgmentRef(providerB, "j1"))),
      List(providerB))
    assertNotEquals(model1.digest, model2.digest)

  test("ValidationModel: different providers list (even with identical specs) changes identity"):
    val model2 = model1.copy(providers = List(providerA, providerB))
    assertNotEquals(model1.digest, model2.digest)

  test("ValidationModel: different targetLanguage changes identity"):
    val model2 = model1.copy(targetLanguage = Digest.of(Canon.CStr("other-target")))
    assertNotEquals(model1.digest, model2.digest)
