package cairn.tests

import cairn.kernel.*
import cairn.core.*

/** PR13: language-associated capabilities select one digest-bound runtime and
  * the same derivation closes over the meta and grammar languages recursively.
  */
class LanguageCapabilitiesSuite extends munit.FunSuite:
  private def artifactRoundTrip(a: Artifact): Artifact =
    Artifact.decode(Canon.encode(a.canon)).fold(e => fail(e), identity)

  private def load(language: ComposedLanguage): ResolvedLanguageCapabilities =
    val original = LanguageCapabilities.standard(language)
    LanguageCapabilities.fromArtifacts(
      artifactRoundTrip(original.descriptor.artifact),
      language,
      artifactRoundTrip(original.change.semantics.artifact),
      artifactRoundTrip(original.change.surface.artifact)).fold(e => fail(e), identity)

  test("capability artifact round-trips and rejects a different language"):
    val loaded = load(Meta.language)
    assertEquals(loaded.descriptor.language, Meta.language.digest)
    val wrong = LanguageCapabilities.fromArtifacts(
      artifactRoundTrip(loaded.descriptor.artifact), Meta.grammarLanguage,
      artifactRoundTrip(loaded.change.semantics.artifact),
      artifactRoundTrip(loaded.change.surface.artifact))
    assert(wrong.isLeft)

  test("loaded Meta bundle derives ΔMeta then Δ(ΔMeta) with one generic operation"):
    val meta = load(Meta.language)
    val deltaMeta = meta.delta.fold(es => fail(es.map(_.render).mkString("\n")), identity)
    val deltaDeltaMeta = deltaMeta.delta.fold(es => fail(es.map(_.render).mkString("\n")), identity)
    assertEquals(deltaMeta.language.name, "Δmeta")
    assertEquals(deltaDeltaMeta.language.name, "ΔΔmeta")
    assertEquals(deltaMeta.descriptor.language, deltaMeta.language.digest)
    assertEquals(deltaDeltaMeta.descriptor.language, deltaDeltaMeta.language.digest)
    assertEquals(deltaDeltaMeta.change.digest, meta.change.digest)

  test("grammar-language bundle has the same recursive closure"):
    val grammar = load(Meta.grammarLanguage)
    val d1 = Delta.deltaOf(grammar).fold(es => fail(es.map(_.render).mkString("\n")), identity)
    val d2 = Delta.deltaOf(d1).fold(es => fail(es.map(_.render).mkString("\n")), identity)
    assertEquals(d1.language.name, "Δgrammar")
    assertEquals(d2.language.name, "ΔΔgrammar")

  test("bundle-selected apply records the selected change semantics"):
    val bundle = load(Meta.grammarLanguage)
    val delta = bundle.delta.fold(es => fail(es.map(_.render).mkString("\n")), identity)
    val change = Parser.parse(delta.language.grammar, "{ }")
      .fold(e => fail(e), identity)
    val (_, evidence) = Delta.apply(bundle, Module(Nil), change).fold(e => fail(e), identity)
    assertEquals(evidence.changeModel, bundle.changeModel.digest)
