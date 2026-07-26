package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

class LanguageStudioSuite extends munit.FunSuite:
  private val target = Stlc.language.digest
  private val meta = LanguageCapabilities.standard(Meta.language)
  private val grammar = LanguageCapabilities.standard(Meta.grammarLanguage)
  private val change = ChangeCapability.standard

  private val ids = Map(
    LanguageAssetKind.Language -> LanguageAssetId(LanguageAssetKind.Language, "stlc"),
    LanguageAssetKind.Grammar -> LanguageAssetId(LanguageAssetKind.Grammar, "default"),
    LanguageAssetKind.ChangeSemantics -> LanguageAssetId(LanguageAssetKind.ChangeSemantics, "delta"),
    LanguageAssetKind.ChangeSurface -> LanguageAssetId(LanguageAssetKind.ChangeSurface, "delta"),
    LanguageAssetKind.LanguageCapabilities -> LanguageAssetId(LanguageAssetKind.LanguageCapabilities, "bundle"),
    LanguageAssetKind.ValidationModel -> LanguageAssetId(LanguageAssetKind.ValidationModel, "validation"),
    LanguageAssetKind.Migration -> LanguageAssetId(LanguageAssetKind.Migration, "next"),
    LanguageAssetKind.ForeignSurface -> LanguageAssetId(LanguageAssetKind.ForeignSurface, "json"),
    LanguageAssetKind.StudioProfileSemantics -> LanguageAssetId(LanguageAssetKind.StudioProfileSemantics, "studio"),
    LanguageAssetKind.StudioProfileSurface -> LanguageAssetId(LanguageAssetKind.StudioProfileSurface, "studio"))

  private def version(n: Int): LanguageStudioProject =
    val validation = ValidationModel(target, Nil, if n == 1 then Nil else List(target))
    val migration = LangMigration(target, target, if n == 1 then Map.empty else Map("old" -> "new"), Map.empty)
    val surface = ForeignSurfaceDescriptor("json", target, ForeignFormat.Json, "application/json",
      SurfaceDirection.Encoding, SurfaceLaw.RoundTrip, Digest.of(Canon.CStr(s"json-provider-$n")))
    val profileSemantics = StudioProfileSemantics(target, List(if n == 1 then "Term" else "TermV2"), Nil, Nil, Nil, List(surface.digest))
    val profileSurface = StudioProfileSurface(profileSemantics.digest, Map.empty, Map("term" -> s"Term $n"), Nil, Map.empty, Nil)
    val semantics = ChangeSemanticsModel(change.semantics.operations.dropRight(n - 1))
    val changeSurface = ChangeSurfaceModel(change.surface.operations.dropRight(n - 1))
    val capabilities = LanguageCapabilities(target, semantics.digest, changeSurface.digest,
      Some(validation.digest), List(migration.artifact.digest), Nil, Nil,
      List(surface.artifact.digest, profileSemantics.artifact.digest, profileSurface.artifact.digest))
    LanguageStudioProject(target, Map(
      ids(LanguageAssetKind.Language) -> Artifact(ArtifactKind.Language, Canon.CStr(s"stlc-language-$n")),
      ids(LanguageAssetKind.Grammar) -> Artifact(ArtifactKind.Grammar, Canon.CStr(s"stlc-grammar-$n")),
      ids(LanguageAssetKind.ChangeSemantics) -> semantics.artifact,
      ids(LanguageAssetKind.ChangeSurface) -> changeSurface.artifact,
      ids(LanguageAssetKind.LanguageCapabilities) -> capabilities.artifact,
      ids(LanguageAssetKind.ValidationModel) -> validation.artifact,
      ids(LanguageAssetKind.Migration) -> migration.artifact,
      ids(LanguageAssetKind.ForeignSurface) -> surface.artifact,
      ids(LanguageAssetKind.StudioProfileSemantics) -> profileSemantics.artifact,
      ids(LanguageAssetKind.StudioProfileSurface) -> profileSurface.artifact))

  test("Language Studio edits the complete capability graph through ΔMeta and ΔGrammar"):
    val before = version(1)
    val after = version(2)
    assertEquals(before.validate, Right(()))
    assertEquals(after.validate, Right(()))
    val edits = after.assets.toList.map(LanguageStudioEdit.apply)
    val proposal = LanguageStudio.propose(before, edits, meta, grammar).fold(e => fail(e), identity)
    assertEquals(proposal.result, after)
    assertEquals(proposal.metaWitness.map(_.language), Some(Meta.language.digest))
    assertEquals(proposal.grammarWitness.map(_.language), Some(Meta.grammarLanguage.digest))
    assert(proposal.deltaMeta.exists(_.render.contains(Delta.tag(Meta.language, "replace"))))
    assert(proposal.deltaGrammar.exists(_.render.contains(Delta.tag(Meta.grammarLanguage, "replace"))))
    assertEquals(proposal.artifact.kind, ArtifactKind.ChangeSet)

  test("grammar assets cannot leak into ΔMeta"):
    val before = version(1)
    val replacement = version(2).assets(ids(LanguageAssetKind.Grammar))
    val proposal = LanguageStudio.propose(before,
      List(LanguageStudioEdit(ids(LanguageAssetKind.Grammar), replacement)), meta, grammar).toOption.get
    assertEquals(proposal.deltaMeta, None)
    assert(proposal.deltaGrammar.nonEmpty)

  test("whole-project validation rejects dangling capability and wrong target models"):
    val project = version(1)
    val wrong = ValidationModel(Meta.language.digest, Nil, Nil).artifact
    val result = LanguageStudio.propose(project,
      List(LanguageStudioEdit(ids(LanguageAssetKind.ValidationModel), wrong)), meta, grammar)
    assert(result.left.exists(message => message.contains("outside the project") || message.contains("another language revision")))

  test("an edit cannot change the declared artifact kind"):
    val project = version(1)
    val result = LanguageStudio.propose(project, List(LanguageStudioEdit(
      ids(LanguageAssetKind.Grammar), Artifact(ArtifactKind.Language, Canon.CStr("not grammar")))), meta, grammar)
    assert(result.isLeft)

  test("Cairn self-host edits retain both ΔMeta and ΔGrammar replay evidence"):
    val before = version(1).copy(targetLanguage = Meta.language.digest)
    val after0 = version(2).copy(targetLanguage = Meta.language.digest)
    // Rebind target-aware assets while retaining real Meta/Grammar interpreters.
    val validation = ValidationModel(Meta.language.digest, Nil, Nil)
    val beforeFixed = before.copy(assets = before.assets
      .updated(ids(LanguageAssetKind.Language), Meta.language.artifact)
      .updated(ids(LanguageAssetKind.Grammar), Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(Meta.language.grammar)))
      .updated(ids(LanguageAssetKind.ValidationModel), validation.artifact)
      .removed(ids(LanguageAssetKind.LanguageCapabilities))
      .removed(ids(LanguageAssetKind.Migration))
      .removed(ids(LanguageAssetKind.ForeignSurface))
      .removed(ids(LanguageAssetKind.StudioProfileSemantics))
      .removed(ids(LanguageAssetKind.StudioProfileSurface)))
    val revisedGrammar = Meta.language.grammar.copy(tokens = Meta.language.grammar.tokens.copy(
      keywords = Meta.language.grammar.tokens.keywords :+ "pr25-self-host"))
    val after = beforeFixed.copy(assets = beforeFixed.assets
      .updated(ids(LanguageAssetKind.Grammar), Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(revisedGrammar)))
      .updated(ids(LanguageAssetKind.ChangeSurface), after0.assets(ids(LanguageAssetKind.ChangeSurface))))
    val edits = after.assets.toList.collect { case (id, artifact) if beforeFixed.assets(id).digest != artifact.digest =>
      LanguageStudioEdit(id, artifact) }
    val (proposal, witness) = SelfHosting.propose(beforeFixed, edits, meta, grammar).fold(e => fail(e), identity)
    assertEquals(proposal.result, after)
    assert(witness.deltaMeta.nonEmpty)
    assert(witness.deltaGrammar.nonEmpty)
    assertEquals(witness.trustedBoundary, TrustedBoundary.minimal.digest)
    assertEquals(witness.artifact.kind, ArtifactKind.Provenance)
