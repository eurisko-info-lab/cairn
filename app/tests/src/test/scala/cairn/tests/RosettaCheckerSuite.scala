package cairn.tests

import cairn.kernel.Cst
import cairn.core.*
import cairn.core.RosettaChecker.ModuleError

/** Unit tests for [[RosettaChecker]] — the gate `PortV2.verified` needs
  * before any host emitter sees a `RosettaModule2`: dangling effect
  * references, duplicate declarations, ambiguous effect-operation dispatch
  * (two composed effects declaring the same op name), and multi-constraint
  * defs that only 3 of 4 host ports actually render correctly.
  */
class RosettaCheckerSuite extends munit.FunSuite:
  private def emptyDef(name: String, effects: List[String] = Nil, constraints: List[(String, String)] = Nil,
                       typeParams: List[String] = Nil, params: List[(String, RTy)] = Nil): RDefV2 =
    RDefV2(name, typeParams, constraints, params, RTy.RUnit, Cst.node("rtrue"), effects)

  private def moduleOf(defs: List[RDefV2], effects: List[REffectV2] = Nil, datas: List[RDataV2] = Nil): RosettaModule2 =
    RosettaModule2("m", datas, effects, defs, Nil)

  test("a well-formed module passes clean"):
    val m = moduleOf(
      defs = List(emptyDef("f", effects = List("counter"), constraints = List("a" -> "Ord"))),
      effects = List(REffectV2("counter", List("tick"))))
    assertEquals(RosettaChecker.check(m), Nil)

  test("duplicate data/effect/def names are all rejected"):
    val m = RosettaModule2(
      name = "m",
      datas = List(RDataV2("D", Nil, Nil), RDataV2("D", Nil, Nil)),
      effects = List(REffectV2("e", Nil), REffectV2("e", Nil)),
      defs = List(emptyDef("f"), emptyDef("f")),
      theorems = Nil)
    val errs = RosettaChecker.check(m)
    assert(errs.contains(ModuleError.DuplicateData("D")), errs)
    assert(errs.contains(ModuleError.DuplicateEffect("e")), errs)
    assert(errs.contains(ModuleError.DuplicateDef("f")), errs)

  test("duplicate type parameter / parameter names within one def are rejected"):
    val m = moduleOf(List(emptyDef("f", typeParams = List("a", "a"),
      params = List("x" -> RTy.RInt, "x" -> RTy.RBool))))
    val errs = RosettaChecker.check(m)
    assert(errs.contains(ModuleError.DuplicateTypeParam("f", "a")), errs)
    assert(errs.contains(ModuleError.DuplicateParam("f", "x")), errs)

  test("a def referencing an undeclared effect is rejected"):
    val m = moduleOf(List(emptyDef("f", effects = List("bogus"))))
    assertEquals(RosettaChecker.check(m), List(ModuleError.UnknownEffect("f", "bogus")))

  test("two composed effects sharing an operation name is ambiguous and rejected"):
    val m = moduleOf(
      defs = List(emptyDef("f", effects = List("counter", "logger"))),
      effects = List(REffectV2("counter", List("tick")), REffectV2("logger", List("tick"))))
    val errs = RosettaChecker.check(m)
    errs match
      case List(ModuleError.AmbiguousEffectOp("f", "tick", owners)) =>
        assertEquals(owners.toSet, Set("counter", "logger"))
      case other => fail(s"expected AmbiguousEffectOp, got $other")

  test("two composed effects with distinct operation names are fine"):
    val m = moduleOf(
      defs = List(emptyDef("f", effects = List("counter", "logger"))),
      effects = List(REffectV2("counter", List("tick")), REffectV2("logger", List("mark"))))
    assertEquals(RosettaChecker.check(m), Nil)

  test("a def with more than one constraint is rejected"):
    val m = moduleOf(List(emptyDef("f", constraints = List("a" -> "Ord", "b" -> "Eq"))))
    RosettaChecker.check(m) match
      case List(ModuleError.MultipleConstraints("f", params)) => assertEquals(params, List("a", "b"))
      case other => fail(s"expected MultipleConstraints, got $other")

  test("validate returns Left with all errors, Right when clean"):
    assert(RosettaChecker.validate(moduleOf(List(emptyDef("f", effects = List("bogus"))))).isLeft)
    assert(RosettaChecker.validate(moduleOf(List(emptyDef("f")))).isRight)
