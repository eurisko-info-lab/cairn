package cairn.tests

import cairn.kernel.*
import cairn.runtime.*

class Pr34EnvelopeSuite extends munit.FunSuite:

  private def dig(label: String): Digest = Digest.of(Canon.CStr(label))

  test("pr34 graph package canon round-trips"):
    val g = Pr34GraphPackage(
      kernelConstitution = dig("k"),
      artifactClosure = dig("sigma"),
      machineClosure = dig("m"),
      runtimeClosure = dig("r"),
      acceptanceClosure = dig("rho"),
      repositoryRoot = dig("repo"),
      finalizedHistory = dig("h"),
      evidenceClosure = dig("eta"),
    )

    val round = Pr34GraphPackage.fromCanon(g.canon).fold(e => fail(e), identity)
    assertEquals(round, g)

  test("pr34 verdict envelope canon round-trips"):
    val g = Pr34GraphPackage(
      kernelConstitution = dig("k"),
      artifactClosure = dig("sigma"),
      machineClosure = dig("m"),
      runtimeClosure = dig("r"),
      acceptanceClosure = dig("rho"),
      repositoryRoot = dig("repo"),
      finalizedHistory = dig("h"),
      evidenceClosure = dig("eta"),
    )

    val v = Pr34VerdictEnvelope(
      kernelConstitution = g.kernelConstitution,
      graphPackage = g.digest,
      verdictClass = Pr34VerdictClass.Valid,
      state = Some(dig("state")),
      evidence = Some(dig("evidence")),
      resourceUse = Pr34ResourceUse(steps = 42, bytesRead = 1024, wallMicros = 777),
    )

    val round = Pr34VerdictEnvelope.fromCanon(v.canon).fold(e => fail(e), identity)
    assertEquals(round, v)

  test("pr34 verdict envelope canon bytes are deterministic"):
    val v1 = Pr34VerdictEnvelope(
      kernelConstitution = dig("k"),
      graphPackage = dig("g"),
      verdictClass = Pr34VerdictClass.Exhausted,
      state = None,
      evidence = None,
      resourceUse = Pr34ResourceUse(steps = 5, bytesRead = 90, wallMicros = 11),
    )
    val v2 = v1.copy()
    assertEquals(Canon.encode(v1.canon).toVector, Canon.encode(v2.canon).toVector)
    assertEquals(v1.digest, v2.digest)

  test("pr34 verdict envelope rejects unknown verdict class"):
    val bad = Canon.CTag("pr34-verdict-envelope-v1", Canon.cmap(
      "kernelConstitution" -> Canon.CStr(dig("k").hex),
      "graphPackage" -> Canon.CStr(dig("g").hex),
      "verdictClass" -> Canon.CStr("mystery"),
      "state" -> Canon.CTag("none", Canon.CInt(0)),
      "evidence" -> Canon.CTag("none", Canon.CInt(0)),
      "resourceUse" -> Canon.cmap(
        "steps" -> Canon.CInt(0),
        "bytesRead" -> Canon.CInt(0),
        "wallMicros" -> Canon.CInt(0),
      ),
    ))
    assert(Pr34VerdictEnvelope.fromCanon(bad).isLeft)

  test("pr34 graph package artifact round-trips and enforces kind"):
    val g = Pr34GraphPackage(
      kernelConstitution = dig("k"),
      artifactClosure = dig("sigma"),
      machineClosure = dig("m"),
      runtimeClosure = dig("r"),
      acceptanceClosure = dig("rho"),
      repositoryRoot = dig("repo"),
      finalizedHistory = dig("h"),
      evidenceClosure = dig("eta"),
    )
    val round = Pr34GraphPackage.fromArtifact(g.artifact).fold(e => fail(e), identity)
    assertEquals(round, g)
    val wrongKind = Artifact(ArtifactKind.Term, g.canon)
    assert(Pr34GraphPackage.fromArtifact(wrongKind).isLeft)

  test("pr34 verdict envelope artifact round-trips and enforces kind"):
    val v = Pr34VerdictEnvelope(
      kernelConstitution = dig("k"),
      graphPackage = dig("g"),
      verdictClass = Pr34VerdictClass.Valid,
      state = Some(dig("state")),
      evidence = Some(dig("evidence")),
      resourceUse = Pr34ResourceUse(steps = 1, bytesRead = 2, wallMicros = 3),
    )
    val round = Pr34VerdictEnvelope.fromArtifact(v.artifact).fold(e => fail(e), identity)
    assertEquals(round, v)
    val wrongKind = Artifact(ArtifactKind.Term, v.canon)
    assert(Pr34VerdictEnvelope.fromArtifact(wrongKind).isLeft)

  test("interop maps CKC valid replay into verdict envelope"):
    val constitution = CKC.KernelConstitution("ckc-v0")
    val graphPackage = dig("graph-package")
    val replay = CKC.Value.ReplayedState(CKC.HistoryReport(
      verifiedTransitions = 2,
      finalState = dig("final-state"),
      finalEpoch = 2L,
    ))
    val result = CKC.KernelResult.Valid(replay, dig("proof-evidence"))
    val env = Pr34EnvelopeInterop.fromCkc(
      constitution,
      graphPackage,
      result,
      Pr34ResourceUse(steps = 10, bytesRead = 500, wallMicros = 1000),
    )

    assertEquals(env.verdictClass, Pr34VerdictClass.Valid)
    assertEquals(env.state, Some(dig("final-state")))
    assertEquals(env.evidence, Some(dig("proof-evidence")))
    assertEquals(env.graphPackage, graphPackage)

  test("interop maps CKC non-valid verdict classes"):
    val constitution = CKC.KernelConstitution("ckc-v0")
    val graphPackage = dig("graph-package")

    val invalidEnv = Pr34EnvelopeInterop.fromCkc(
      constitution,
      graphPackage,
      CKC.KernelResult.Invalid("boom"),
      Pr34ResourceUse(steps = 1, bytesRead = 1, wallMicros = 1),
    )
    assertEquals(invalidEnv.verdictClass, Pr34VerdictClass.Invalid)
    assertEquals(invalidEnv.state, None)
    assertEquals(invalidEnv.evidence, None)

    val missingEnv = Pr34EnvelopeInterop.fromCkc(
      constitution,
      graphPackage,
      CKC.KernelResult.Missing(List(dig("x"))),
      Pr34ResourceUse(steps = 1, bytesRead = 1, wallMicros = 1),
    )
    assertEquals(missingEnv.verdictClass, Pr34VerdictClass.Missing)
    assertEquals(missingEnv.state, None)
    assertEquals(missingEnv.evidence, None)

    val exhaustedEnv = Pr34EnvelopeInterop.fromCkc(
      constitution,
      graphPackage,
      CKC.KernelResult.Exhausted("max_steps"),
      Pr34ResourceUse(steps = 1, bytesRead = 1, wallMicros = 1),
    )
    assertEquals(exhaustedEnv.verdictClass, Pr34VerdictClass.Exhausted)
    assertEquals(exhaustedEnv.state, None)
    assertEquals(exhaustedEnv.evidence, None)
