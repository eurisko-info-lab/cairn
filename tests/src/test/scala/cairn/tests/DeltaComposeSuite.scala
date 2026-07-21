package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** ΔL changeset composition (list concatenation — associative, `{}` identity)
  * and the separate, optional [[Delta.collapseAdjacent]] canonicalization
  * pass. Composition alone is always correct: [[Delta.apply]]'s sequential
  * fold already gives an uncollapsed changeset the right semantics one step
  * at a time — collapse changes readability, never behavior, which is
  * exactly what these tests check (collapsed vs uncollapsed must reach the
  * identical resulting module digest).
  */
class DeltaComposeSuite extends munit.FunSuite:
  private val dl = Delta.deltaOf(Stlc.language).fold(e => fail(e.map(_.render).mkString), identity)
  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  test("compose is associative (pure list concatenation under the hood)"):
    val c1 = parseChange("{ add a = true ; }")
    val c2 = parseChange("{ add b = false ; }")
    val c3 = parseChange("{ add c = true ; }")
    val left = Delta.compose(Stlc.language, Delta.compose(Stlc.language, c1, c2).toOption.get, c3)
    val right = Delta.compose(Stlc.language, c1, Delta.compose(Stlc.language, c2, c3).toOption.get)
    assertEquals(left, right)

  test("empty changeset `{}` is the identity for compose"):
    val empty = parseChange("{ }")
    val c1 = parseChange("{ add a = true ; }")
    val base = Module(Nil)
    val viaLeftId = Delta.apply(Stlc.language, base, Delta.compose(Stlc.language, empty, c1).toOption.get)
    val viaRightId = Delta.apply(Stlc.language, base, Delta.compose(Stlc.language, c1, empty).toOption.get)
    val direct = Delta.apply(Stlc.language, base, c1)
    assertEquals(viaLeftId.map(_._1.digest), direct.map(_._1.digest))
    assertEquals(viaRightId.map(_._1.digest), direct.map(_._1.digest))

  test("compose(cs1, cs2) applied == cs1 applied then cs2 applied, sequentially"):
    val base = Module(List("a" -> Cst.node("true")))
    val c1 = parseChange("{ add b = true ; }")
    val c2 = parseChange("{ add c = false ; }")
    val composed = Delta.compose(Stlc.language, c1, c2).toOption.get
    val viaCompose = Delta.apply(Stlc.language, base, composed).map(_._1.digest)
    val viaSequence = for
      r1 <- Delta.apply(Stlc.language, base, c1)
      r2 <- Delta.apply(Stlc.language, r1._1, c2)
    yield r2._1.digest
    assertEquals(viaCompose, viaSequence)

  test("collapseAdjacent: rename x→y ; rename y→z ⇒ rename x→z, same resulting module"):
    val base = Module(List(
      "x" -> Cst.node("true"),
      "user" -> Cst.node("var", Cst.Leaf("x"))))
    val r1 = parseChange("{ rename x to y footprint [ user ] ; }")
    val r2 = parseChange("{ rename y to z footprint [ user ] ; }")
    val viaSequence = for
      s1 <- Delta.apply(Stlc.language, base, r1)
      s2 <- Delta.apply(Stlc.language, s1._1, r2)
    yield s2._1

    val composed = Delta.compose(Stlc.language, r1, r2).toOption.get
    val collapsed = Delta.collapse(Stlc.language, composed).toOption.get
    // collapse actually fired: exactly one rename item, x -> z directly
    collapsed match
      case Cst.Node(_, List(Cst.Node("list", List(
            Cst.Node(t, List(Cst.Leaf("x"), Cst.Leaf("z"), _)))))) =>
        assertEquals(t, Delta.tag(Stlc.language, "rename"))
      case other => fail(s"expected a single collapsed rename, got: ${other.render}")

    val viaCollapsed = Delta.apply(Stlc.language, base, collapsed).map(_._1)
    assertEquals(viaCollapsed.map(_.digest), viaSequence.map(_.digest))
    assert(viaSequence.isRight, viaSequence.toString)

  test("collapseAdjacent: remove x ; add x = t ⇒ replace x = t, same resulting module"):
    val base = Module(List("x" -> Cst.node("true")))
    val rm = parseChange("{ remove x ; }")
    val ad = parseChange("{ add x = false ; }")
    val viaSequence = for
      s1 <- Delta.apply(Stlc.language, base, rm)
      s2 <- Delta.apply(Stlc.language, s1._1, ad)
    yield s2._1

    val composed = Delta.compose(Stlc.language, rm, ad).toOption.get
    val collapsed = Delta.collapse(Stlc.language, composed).toOption.get
    collapsed match
      case Cst.Node(_, List(Cst.Node("list", List(
            Cst.Node(t, List(Cst.Leaf("x"), Cst.Leaf("false" | "false()"))))))) =>
        assertEquals(t, Delta.tag(Stlc.language, "replace"))
      case Cst.Node(_, List(Cst.Node("list", List(Cst.Node(t, List(Cst.Leaf("x"), _)))))) =>
        assertEquals(t, Delta.tag(Stlc.language, "replace"))
      case other => fail(s"expected a single collapsed replace, got: ${other.render}")

    val viaCollapsed = Delta.apply(Stlc.language, base, collapsed).map(_._1)
    assertEquals(viaCollapsed.map(_.digest), viaSequence.map(_.digest))
    assert(viaSequence.isRight, viaSequence.toString)

  test("collapseAdjacent leaves unrelated adjacent ops sequenced, not merged"):
    val c1 = parseChange("{ add a = true ; }")
    val c2 = parseChange("{ add b = false ; }")
    val composed = Delta.compose(Stlc.language, c1, c2).toOption.get
    val collapsed = Delta.collapse(Stlc.language, composed).toOption.get
    assertEquals(collapsed, composed)
