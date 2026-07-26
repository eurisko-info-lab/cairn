package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.{ArtifactApplicationResolver, EffectContexts, PackLoader}
import cairn.systemhandler.MemCas
import cairn.user.quicksort.QuickSort2

class ArtifactApplicationSuite extends munit.FunSuite:
  private val language = PackLoader(EffectContexts.forPackLoader()).requireClosed("stlc")
  private val capabilities = LanguageCapabilities.standard(language)
  private val grammarArtifact = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(language.grammar))
  private val manifest = ApplicationManifest("artifact-app", List(ApplicationLanguage(
    "stlc", language.digest, grammarArtifact.digest, capabilities.descriptor.digest)),
    List(ApplicationEntry("quicksort", QuickSort2.module.artifact.digest, ArtifactKind.RosettaDecl)))

  private def source(): MemCas =
    val cas = MemCas()
    language.fragments.foreach(f => cas.put(f.artifact))
    List(language.artifact, grammarArtifact, capabilities.change.semantics.artifact,
      capabilities.change.surface.artifact, capabilities.descriptor.artifact,
      QuickSort2.module.artifact, manifest.artifact).foreach(cas.put)
    cas

  test("one root digest installs and resolves the entire application"):
    val origin = source()
    val installed = MemCas()
    val resolver = ArtifactApplicationResolver(installed)
    val graph = resolver.install(manifest.digest, origin).fold(e => fail(e), identity)
    val app = resolver.resolve(manifest.digest).fold(e => fail(e), identity)
    assertEquals(app.manifest.name, "artifact-app")
    assertEquals(app.languages.keySet, Set("stlc"))
    assertEquals(app.languages("stlc").language.digest, language.digest)
    assertEquals(app.languages("stlc").descriptor, capabilities.descriptor)
    assertEquals(app.entries("quicksort").digest, QuickSort2.module.artifact.digest)
    assertEquals(app.installed, graph)
    assert(graph.contains(language.fragments.head.digest))

  test("dependency discovery follows language fragments and capability selections"):
    val languageDeps = ArtifactDependencies.direct(language.artifact).toOption.get
    assertEquals(languageDeps.toSet, language.fragments.map(_.digest).toSet)
    val capabilityDeps = ArtifactDependencies.direct(capabilities.descriptor.artifact).toOption.get
    assert(capabilityDeps.contains(capabilities.change.semantics.digest))
    assert(capabilityDeps.contains(capabilities.change.surface.digest))

  test("installation fails closed when a discovered dependency is absent"):
    val incomplete = MemCas()
    val origin = source()
    // Copy only the root: its first recursive dependency must be discovered, not supplied by a host map.
    incomplete.put(manifest.artifact)
    assert(ArtifactApplicationResolver(MemCas()).install(manifest.digest, incomplete).isLeft)
    assert(ArtifactApplicationResolver(MemCas()).install(manifest.digest, origin).isRight)

  test("resolution rejects a capability bundle bound to another language"):
    val origin = source()
    val badDescriptor = capabilities.descriptor.copy(language = Meta.language.digest)
    origin.put(badDescriptor.artifact)
    val badRoot = manifest.copy(languages = List(manifest.languages.head.copy(capabilities = badDescriptor.digest)))
    origin.put(badRoot.artifact)
    val installed = MemCas()
    val resolver = ArtifactApplicationResolver(installed)
    resolver.install(badRoot.digest, origin).fold(e => fail(e), identity)
    assert(resolver.resolve(badRoot.digest).left.exists(_.contains("language digest mismatch")))

  test("entry artifact kinds are checked after installation"):
    val origin = source()
    val badRoot = manifest.copy(entries = List(ApplicationEntry(
      "quicksort", QuickSort2.module.artifact.digest, ArtifactKind.Theorem)))
    origin.put(badRoot.artifact)
    val installed = MemCas()
    val resolver = ArtifactApplicationResolver(installed)
    resolver.install(badRoot.digest, origin).toOption.get
    assert(resolver.resolve(badRoot.digest).left.exists(_.contains("expected theorem")))
