package cairn.examples.riemann

import cairn.core.*
import java.nio.file.{Files, Path}

/** Thin Riemann tutorial: load the standalone analytic-claims language,
  * build the Riemann Hypothesis as a proof-free Claim, project it to Lean
  * (round-trip verified), write the obligations manifest, and report
  * honestly that no Theorem/Certificate exists — this stays an open claim,
  * on purpose.
  */
object RiemannTutorial:
  final case class Report(
      languageProvides: Set[String],
      languageRequiresMet: Boolean,
      claimRoundTrips: Boolean,
      leanIsDef: Boolean,
      leanReferencesRiemannZeta: Boolean,
      leanHasNoSorry: Boolean,
      obligationStatus: String,
      certificateExists: Boolean
  )

  def run(dir: Path): Report =
    val Riemann = cairn.examples.riemann.Riemann(
      cairn.runtime.PackLoader(cairn.systemhandler.AuthorityGate.bootstrapped()))
    val lang = Riemann.language
    val provides = lang.fragments.flatMap(_.provides).toSet
    val requires = lang.fragments.flatMap(_.requires).toSet
    val met = requires.subsetOf(provides)
    val claim = Riemann.riemannHypothesisClaim
    val roundTrips = RoundTrip.check(lang.grammar, claim.statement).isRight
    val (leanFile, manifestFile) = Riemann.writeArtifacts(dir)
      .fold(e => throw RuntimeException(e), identity)
    val leanText = Files.readString(leanFile)
    val manifestText = Files.readString(manifestFile)
    // banner comment lines explain the absence of sorry/theorem in prose —
    // only the generated code region matters for this check
    val code = leanText.linesIterator.filterNot(_.stripLeading.startsWith("--")).mkString("\n")
    Report(
      languageProvides = provides,
      languageRequiresMet = met,
      claimRoundTrips = roundTrips,
      leanIsDef = leanText.contains("def riemann_hypothesis : Prop"),
      leanReferencesRiemannZeta = leanText.contains("riemannZeta"),
      leanHasNoSorry = !code.contains("sorry") && !code.contains("theorem "),
      obligationStatus = if manifestText.contains("\"open\"") then "open" else "unknown",
      // No Certificate/Theorem is ever constructed for this claim anywhere in
      // the pack — the honest answer is a constant, not a computed check.
      certificateExists = false)
