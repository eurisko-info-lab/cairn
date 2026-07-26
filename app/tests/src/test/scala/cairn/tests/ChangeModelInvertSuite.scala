package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** Direct unit tests for ChangeAlgebra.invert on all 5 ChangeModel.default
  * operations — no dedicated invert test file existed before ChangeModel
  * (only indirect coverage via PatchGraphSuite's inverseStep). Each test
  * checks the exact inverse Cst shape AND that forward-then-inverse
  * round-trips the module digest back to base.
  */
class ChangeModelInvertSuite extends munit.FunSuite:
  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).fold(e => fail(e.map(_.render).mkString), identity)
  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  private def roundTrips(base: Module, change: Cst): Unit =
    val (m1, _) = Delta.apply(lang, base, change).fold(e => fail(e), identity)
    val inv = ChangeAlgebra.invert(lang, base, change).fold(e => fail(e), identity)
    val (m2, _) = Delta.apply(lang, m1, inv).fold(e => fail(e), identity)
    assertEquals(m2.digest, base.digest, s"forward-then-inverse did not restore base:\n$m2\nvs\n$base")

  /** `invert` returns a full changeset wrapper (correctly — it's the inverse
    * of a change-SET) — unwrap to the single item these single-op tests care about.
    */
  private def soleItem(cs: Cst): Cst = cs match
    case Cst.Node(_, List(Cst.Node("list", List(item)))) => item
    case other => fail(s"expected a single-item changeset, got $other")

  test("invert add -> remove"):
    val base = Module(Nil)
    val ch = parseChange("{ add a = true ; }")
    val inv = ChangeAlgebra.invert(lang, base, ch).fold(e => fail(e), identity)
    soleItem(inv) match
      case Cst.Node(t, List(Cst.Leaf("a"))) => assertEquals(t, Delta.tag(lang, "remove"))
      case other => fail(other.toString)
    roundTrips(base, ch)

  test("invert replace -> replace with the OLD term"):
    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ replace a = false ; }")
    val inv = ChangeAlgebra.invert(lang, base, ch).fold(e => fail(e), identity)
    soleItem(inv) match
      case Cst.Node(t, List(Cst.Leaf("a"), oldTerm)) =>
        assertEquals(t, Delta.tag(lang, "replace"))
        assertEquals(oldTerm, Stlc.tru)
      case other => fail(other.toString)
    roundTrips(base, ch)

  test("invert remove -> add with the OLD term"):
    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ remove a ; }")
    val inv = ChangeAlgebra.invert(lang, base, ch).fold(e => fail(e), identity)
    soleItem(inv) match
      case Cst.Node(t, List(Cst.Leaf("a"), oldTerm)) =>
        assertEquals(t, Delta.tag(lang, "add"))
        assertEquals(oldTerm, Stlc.tru)
      case other => fail(other.toString)
    roundTrips(base, ch)

  test("invert edit -> edit with the OLD subtree at the SAME path"):
    val base = Module(List("id" -> Stlc.idBool)) // lam1("x", tBool, v("x")): children [name, type, body]
    val ch = parseChange("{ edit id at [2] = true ; }")
    val inv = ChangeAlgebra.invert(lang, base, ch).fold(e => fail(e), identity)
    soleItem(inv) match
      case Cst.Node(t, List(Cst.Leaf("id"), pathCst, oldSubtree)) =>
        assertEquals(t, Delta.tag(lang, "edit"))
        assertEquals(Delta.pathOf(pathCst), List(2))
        assertEquals(oldSubtree, Stlc.v("x")) // the body BEFORE the edit replaced it with `true`
      case other => fail(other.toString)
    roundTrips(base, ch)

  test("invert rename -> rename swapping from/to, footprint unchanged"):
    // "usesA"'s entire body is the free variable reference `a` — the
    // simplest possible def whose footprint-referencing "a" is real.
    val base = Module(List("a" -> Stlc.tru, "usesA" -> Stlc.v("a")))
    val ch = parseChange("{ rename a to c footprint [ usesA ] ; }")
    val inv = ChangeAlgebra.invert(lang, base, ch).fold(e => fail(e), identity)
    soleItem(inv) match
      case Cst.Node(t, List(Cst.Leaf(to), Cst.Leaf(from), fpCst)) =>
        assertEquals(t, Delta.tag(lang, "rename"))
        assertEquals(to, "c")
        assertEquals(from, "a")
        val fp = fpCst match
          case Cst.Node("some", List(Cst.Node("list", items))) => items.collect { case Cst.Leaf(n) => n }.toSet
          case _ => Set.empty
        assertEquals(fp, Set("usesA"))
      case other => fail(other.toString)
    roundTrips(base, ch)
