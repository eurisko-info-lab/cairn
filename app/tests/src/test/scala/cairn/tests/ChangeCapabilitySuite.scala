package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** PR12: the complete free-change capability is pack data and survives an
  * artifact-only hop with semantic and surface identities kept separate.
  */
class ChangeCapabilitySuite extends munit.FunSuite:
  private val packSource = java.nio.file.Files.readString(
    java.nio.file.Path.of("content/languages/change-standard.cairn"))

  private val capabilityLanguage =
    val (_, declared) = Meta.parseLanguageAst(packSource).fold(e => fail(e), identity)
    Compose.compose("stlc-with-changes", Stlc.language.fragments ++ declared)
      .fold(es => fail(es.map(_.render).mkString("\n")), identity)

  private val declaredCapability =
    ChangeCapability.fromLanguage(capabilityLanguage).fold(e => fail(e), identity)

  private def artifactRoundTrip(a: Artifact): Artifact =
    Artifact.decode(Canon.encode(a.canon)).fold(e => fail(e), identity)

  private val reconstructed = ChangeCapability.fromArtifacts(
    artifactRoundTrip(declaredCapability.semantics.artifact),
    artifactRoundTrip(declaredCapability.surface.artifact),
    artifactRoundTrip(declaredCapability.artifact)).fold(e => fail(e), identity)

  private val model = reconstructed.model.fold(e => fail(e), identity)
  private val dl = Delta.deltaOf(Stlc.language, reconstructed)
    .fold(es => fail(es.map(_.render).mkString("\n")), identity)
  private def parseChange(s: String): Cst = Parser.parse(dl.grammar, s).fold(e => fail(e), identity)

  test("change operations are declared in .cairn pack data and round-trip through Meta"):
    assertEquals(model.operations.map(_.name).toSet, Set("add", "replace", "remove", "edit", "rename"))
    assertEquals(reconstructed.semantics.digest, ChangeCapability.standard.semantics.digest)
    assertEquals(reconstructed.surface.digest, ChangeCapability.standard.surface.digest)
    val (_, fragments) = Meta.parseLanguageAst(packSource).fold(e => fail(e), identity)
    val printed = Meta.printLanguage("standard_changes", fragments).fold(e => fail(e), identity)
    val (_, reparsed) = Meta.parseLanguageAst(printed).fold(e => fail(e), identity)
    assertEquals(reparsed.flatMap(_.changeSemantics), fragments.flatMap(_.changeSemantics))
    assertEquals(reparsed.flatMap(_.changeSurfaces), fragments.flatMap(_.changeSurfaces))

  test("cosmetic surface edits change surface/capability identity, not semantics"):
    val changedSurface = reconstructed.surface.copy(operations = reconstructed.surface.operations.map {
      case op if op.name == "add" => op.copy(printSegs = PrintSeg.Lit("insert") :: op.printSegs.tail)
      case op => op
    })
    val changed = ChangeCapability(reconstructed.semantics, changedSurface)
    assertEquals(changed.semantics.digest, reconstructed.semantics.digest)
    assertNotEquals(changed.surface.digest, reconstructed.surface.digest)
    assertNotEquals(changed.digest, reconstructed.digest)

  test("artifact-only reconstruction supports parse, print, apply, invert, and format-preserving apply"):
    val change = parseChange("{ replace a = false ; }")
    assertEquals(Printer.print(dl.grammar, change).fold(e => fail(e), identity), "{\n  replace a = false;\n}")
    val base = Module(List("a" -> Stlc.tru, "b" -> Stlc.fls))
    val applied = Delta.apply(Stlc.language, base, change, model).fold(e => fail(e), identity)._1
    assertEquals(applied.get("a"), Some(Stlc.fls))
    val inverse = ChangeAlgebra.invert(Stlc.language, base, change, model).fold(e => fail(e), identity)
    val restored = Delta.apply(Stlc.language, applied, inverse, model).fold(e => fail(e), identity)._1
    assertEquals(restored.digest, base.digest)
    val source = "-- keep\na = true ;\nb = false ;\n"
    val preserved = Delta.applyPreservingFormat(
      Stlc.language, ModuleSurface.grammar(Stlc.language), source, change, model).fold(e => fail(e), identity)
    assert(preserved.contains("-- keep"), preserved)
    assert(preserved.contains("a = false ;"), preserved)

  test("artifact-only reconstruction supports access analysis and witnessed merge"):
    val base = Module(List("sheet" -> Stlc.app1(Stlc.tru, Stlc.fls)))
    val left = parseChange("{ edit sheet at [0] = false ; }")
    val right = parseChange("{ edit sheet at [1] = true ; }")
    val trace = ChangeAlgebra.accessTrace(Stlc.language, base, left, model).fold(e => fail(e.render), identity)
    assert(trace.accesses.exists {
      case SemanticAccess(AccessMode.Write, SemanticLocation.Subtree("sheet", _)) => true
      case _ => false
    })
    Merge.threeWay(Stlc.language, base, left, right, model = model) match
      case Right((merged, _)) => assertEquals(merged.get("sheet"), Some(Stlc.app1(Stlc.fls, Stlc.tru)))
      case Left(conflict) => fail(conflict.render)
