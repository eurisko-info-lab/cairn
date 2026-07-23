package cairn.tests
import cairn.runtime.EffectContexts

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.examples.riemann.RiemannTutorial
import java.nio.file.Files

/** Riemann Hypothesis pack: an open claim, not a parity item (docs/exemplars.md).
  * Exercises the same round-trip/compose/semantic/tutorial discipline as the
  * other exemplar packs, plus positive checks that the pack stays honestly
  * unproven (no Theorem/Certificate, no `sorry`, generated Lean is a `def`).
  */
class RiemannSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContexts.forPackLoader())
  private val Riemann = cairn.examples.riemann.Riemann(packs)

  test("riemann pack loads from languages/*.cairn at runtime"):
    val raw = packs.loadRaw()
    assert(raw.contains("riemann"), raw.keySet.toString)

  test("riemann composes standalone — no unmet requires"):
    val unmet = packs.unmetRequires("riemann", packs.loadRaw())
    assertEquals(unmet, Set.empty[String])
    Riemann.ownCompose match
      case Right(lang) => assert(lang.fragments.exists(_.provides.contains("riemann")))
      case Left(errs)  => fail(errs.map(_.render).mkString)

  test("riemann.cairn text round-trips the meta surface"):
    val fs = packs.requireOwn("riemann")
    val text = Meta.printLanguage("riemann", fs).fold(e => fail(e), identity)
    val back = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
    assertEquals(back._1, "riemann")
    assertEquals(back._2.map(_.digest), fs.map(_.digest))

  test("riemann hypothesis statement round-trips under its own grammar"):
    RoundTrip.check(Riemann.language.grammar, Riemann.riemannHypothesis).fold(e => fail(e), identity)

  test("riemann hypothesis has the expected shape (critical-strip formalization)"):
    Riemann.riemannHypothesis match
      case Cst.Node("forallC", List(Cst.Leaf("s"),
            Cst.Node("rimplies", List(
              Cst.Node("rand", List(
                Cst.Node("isZero", List(Cst.Node("zetaOf", List(Cst.Node("cvar", List(Cst.Leaf("s"))))))),
                Cst.Node("rand", _))),
              Cst.Node("req", List(Cst.Node("reOf", _), Cst.Node("rhalf", _))))))) => ()
      case other => fail(s"unexpected shape: ${other.render}")

  test("riemann_hypothesis is a Claim, not a Theorem"):
    val claim = Riemann.riemannHypothesisClaim
    assertEquals(claim.name, "riemann_hypothesis")
    assertEquals(claim.artifact.kind, ArtifactKind.Claim)
    // No cairn.proof.Theorem / Certificate value is ever constructed for this
    // claim anywhere in Riemann.scala — the "open" status is structural
    // (nothing to inspect), not a flag that could be flipped by mistake.

  test("Lean projection round-trips, references riemannZeta, stays a def (never sorry/theorem)"):
    val text = Riemann.LeanPort.emit(Riemann.riemannHypothesisClaim).fold(e => fail(e), identity)
    assert(text.contains("def riemann_hypothesis : Prop"), text)
    assert(text.contains("riemannZeta"), text)
    assert(text.contains(").re"), text)
    // banner comment explains the absence of sorry/theorem in prose — check
    // only the generated code region, not the whole file
    val code = text.linesIterator.filterNot(_.stripLeading.startsWith("--")).mkString("\n")
    assert(!code.contains("sorry"), code)
    assert(!code.contains("theorem "), code)

  test("obligations manifest marks the claim open, not a theorem"):
    val manifest = Riemann.obligationsManifest(Riemann.riemannHypothesisClaim, "Riemann.lean")
      .fold(e => fail(e), identity)
    assert(manifest.contains("\"kind\""), manifest)
    assert(manifest.contains("\"claim\""), manifest)
    assert(manifest.contains("\"status\""), manifest)
    assert(manifest.contains("\"open\""), manifest)
    assert(!manifest.contains("\"theorem\""), manifest)

  test("RiemannTutorial: reports open status end-to-end"):
    val dir = Files.createTempDirectory("riemann-tutorial")
    val r = RiemannTutorial.run(dir)
    assert(r.languageRequiresMet)
    assert(r.claimRoundTrips)
    assert(r.leanIsDef)
    assert(r.leanReferencesRiemannZeta)
    assert(r.leanHasNoSorry)
    assertEquals(r.obligationStatus, "open")
    assert(!r.certificateExists)
    assert(r.languageProvides.contains("riemann"))
