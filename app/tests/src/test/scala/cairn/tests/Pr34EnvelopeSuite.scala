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
