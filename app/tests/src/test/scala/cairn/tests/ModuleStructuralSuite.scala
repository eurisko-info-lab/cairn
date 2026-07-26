package cairn.tests

import cairn.kernel.*
import cairn.core.*

/** Unit tests for [[ModuleStructural.Spec.canon]] — the canonical
  * descriptor [[ModuleGate.fromSpecs]] digests into
  * [[cairn.core.AcceptancePolicy]]'s own identity.
  */
class ModuleStructuralSuite extends munit.FunSuite:
  import ModuleStructural.Spec

  test("Spec.canon is deterministic for identical specs"):
    val a = Spec.DefinedRef("foo", 0, "foo")
    val b = Spec.DefinedRef("foo", 0, "foo")
    assertEquals(a.canon, b.canon)

  test("Spec.canon distinguishes specs differing only in one field"):
    assertNotEquals(
      Spec.DefinedRef("foo", 0, "foo").canon,
      Spec.DefinedRef("foo", 1, "foo").canon)
    assertNotEquals(
      Spec.SumLeavesAtMost("mixture", List(1), 100, "mixture").canon,
      Spec.SumLeavesAtMost("mixture", List(1), 99, "mixture").canon)

  test("Spec.canon distinguishes different spec kinds even with overlapping field values"):
    assertNotEquals(
      Spec.DefinedRef("foo", 0, "foo").canon,
      Spec.DefinedLeafList("foo", 0, "foo").canon)

  test("Spec.canon: Set-valued fields are order-independent"):
    val a = Spec.RefTagIn("ctor", 0, Set("a", "b", "c"), "label")
    val b = Spec.RefTagIn("ctor", 0, Set("c", "b", "a"), "label")
    assertEquals(a.canon, b.canon)

  test("Spec.canon: closure-bearing specs (OutlineNums, LeafOk) ignore the closure itself"):
    val a = Spec.LeafOk("ctor", 0, _ => true, s => s)
    val b = Spec.LeafOk("ctor", 0, _ => false, s => s"different: $s")
    assertEquals(a.canon, b.canon,
      "documented limitation: LeafOk's predicate/detail closures have no canonical form")
    val c = Spec.OutlineNums("ctor", 0, (_, r) => Right(0), "label")
    val d = Spec.OutlineNums("ctor", 0, (_, r) => Left("different"), "label")
    assertEquals(c.canon, d.canon,
      "documented limitation: OutlineNums's resolveNum closure has no canonical form")

  test("Spec.canon: a whole spec list's digest matches ModuleGate.fromSpecs's descriptor"):
    val specs = List(
      Spec.DefinedRef("foo", 0, "foo"),
      Spec.NonEmptyLeaves("bar", List(1, 2), List("a", "b")))
    val gate = ModuleGate.fromSpecs("j", specs)(_ => Right(()))
    assertEquals(gate.descriptor, Some(Digest.of(Canon.CList(specs.map(_.canon)))))
