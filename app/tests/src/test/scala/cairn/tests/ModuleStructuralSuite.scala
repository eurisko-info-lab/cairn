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

  test("Spec.canon: LeafOk is deterministic and distinguishes different judgment names"):
    assertEquals(Spec.LeafOk("ctor", 0, "j1").canon, Spec.LeafOk("ctor", 0, "j1").canon)
    assertNotEquals(Spec.LeafOk("ctor", 0, "j1").canon, Spec.LeafOk("ctor", 0, "j2").canon)

  test("Spec.canon: OutlineNums is deterministic and distinguishes different judgment names / number sources"):
    import ModuleStructural.NumberSource
    val base = Spec.OutlineNums("ctor", 0, List(NumberSource.FromLeaf("t", 0)), "j1", "label")
    assertEquals(base.canon, Spec.OutlineNums("ctor", 0, List(NumberSource.FromLeaf("t", 0)), "j1", "label").canon)
    assertNotEquals(base.canon,
      Spec.OutlineNums("ctor", 0, List(NumberSource.FromLeaf("t", 0)), "j2", "label").canon,
      "different judgmentName must change the descriptor")
    assertNotEquals(base.canon,
      Spec.OutlineNums("ctor", 0, List(NumberSource.ByTag(Map("t" -> 1))), "j1", "label").canon,
      "different numberSources must change the descriptor")

  test("Spec.canon: a whole spec list's digest matches ModuleGate.fromSpecs's descriptor"):
    val specs = List(
      Spec.DefinedRef("foo", 0, "foo"),
      Spec.NonEmptyLeaves("bar", List(1, 2), List("a", "b")))
    val gate = ModuleGate.fromSpecs("j", specs)(_ => Right(()))
    assertEquals(gate.descriptor, Some(Digest.of(Canon.CList(specs.map(_.canon)))))
