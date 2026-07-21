package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** Δ(ΔL) — the recursive closure §2b claims (`deltaOf(deltaOf(L))`, doc
  * comment on `Delta.deltaOf`) but that, before this suite, was never
  * actually exercised anywhere in the codebase (confirmed by grep — the
  * phrase appeared only in that one comment). `deltaOf`/`apply` turn out to
  * already be fully generic enough that Δ(ΔL) needs no new production code
  * beyond [[Delta.flatten]] (see its doc comment) — the real deliverable
  * here is proving the closure actually closes, end to end: L ← ΔL ← Δ(ΔL).
  */
class DeltaFlattenSuite extends munit.FunSuite:
  private val dl = Delta.deltaOf(Stlc.language).fold(e => fail(e.map(_.render).mkString), identity)
  private val ddl = Delta.deltaOf(dl).fold(e => fail(e.map(_.render).mkString), identity)
  private def parseDdl(src: String): Cst = Parser.parse(ddl.grammar, src).fold(e => fail(e), identity)
  private def parseDl(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  test("Δ(ΔL) is a real, distinct ComposedLanguage — deltaOf applied to itself"):
    assert(ddl.digest != dl.digest)
    assert(ddl.digest != Stlc.language.digest)
    assert(ddl.constructors.keySet.exists(_.startsWith("add:")))
    assert(ddl.constructors.keySet.exists(_.startsWith("replace:")))

  test("editing a named changeset via Δ(ΔL), flattening, then applying to L closes the loop"):
    val original = parseDl("{ add b = true ; }")
    val patches = Module(List("patch1" -> original))

    val edited = parseDl("{ add b = false ; }")
    val ddlChange = parseDdl("{ replace patch1 = { add b = false ; } ; }")
    val patched = Delta.apply(dl, patches, ddlChange).fold(e => fail(e), identity)._1
    val flattened = Delta.flatten(patched, "patch1").fold(e => fail(e), identity)
    assertEquals(flattened, edited)

    val base = Module(List("a" -> Cst.node("true")))
    val result = Delta.apply(Stlc.language, base, flattened).fold(e => fail(e), identity)._1
    assertEquals(result.get("b"), Some(Cst.node("false")))

  test("Δ(ΔL) no-op replace flattens back to an equal changeset (identity law, η)"):
    val patch1 = parseDl("{ add b = true ; }")
    val patches = Module(List("patch1" -> patch1))
    val noop = parseDdl("{ replace patch1 = { add b = true ; } ; }")
    val patched = Delta.apply(dl, patches, noop).fold(e => fail(e), identity)._1
    val flattened = Delta.flatten(patched, "patch1").fold(e => fail(e), identity)
    assertEquals(flattened, patch1)

  test("Δ(ΔL) add of two named changesets, flattened and composed, matches sequential apply"):
    val addBoth = parseDdl("{ add patch1 = { add x = true ; } ; add patch2 = { add y = false ; } ; }")
    val patched = Delta.apply(dl, Module(Nil), addBoth).fold(e => fail(e), identity)._1
    val p1 = Delta.flatten(patched, "patch1").fold(e => fail(e), identity)
    val p2 = Delta.flatten(patched, "patch2").fold(e => fail(e), identity)
    val composed = Delta.compose(Stlc.language, p1, p2).fold(e => fail(e), identity)

    val base = Module(Nil)
    val viaComposed = Delta.apply(Stlc.language, base, composed).map(_._1.digest)
    val viaSequence = for
      r1 <- Delta.apply(Stlc.language, base, p1)
      r2 <- Delta.apply(Stlc.language, r1._1, p2)
    yield r2._1.digest
    assertEquals(viaComposed, viaSequence)
    assert(viaSequence.isRight, viaSequence.toString)

  test("flatten fails cleanly on a name absent from the Δ(ΔL)-patched module"):
    val patches = Module(List("patch1" -> parseDl("{ add b = true ; }")))
    assert(Delta.flatten(patches, "nope").isLeft)
