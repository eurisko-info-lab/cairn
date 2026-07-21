package cairn.tests

import cairn.kernel.*
import cairn.systeminterface.Random as RandomEffect
import cairn.systeminterface.Clock as ClockEffect

/** Meta-defined effect interfaces (post-migration priority #1): `Random` and
  * `Clock` are Fragment-defined template families. These are mechanical
  * drift guards, same spirit as `ModuleBoundarySuite`.
  */
class EffectMetaSuite extends munit.FunSuite:

  test("EffectMeta.random composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.random", List(EffectMeta.random)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("actionsOf(Random, ...) matches the hand-tagged Random actions exactly"):
    val derived = EffectMeta.actionsOf(Effects.Family.Random, EffectMeta.random).toSet
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Random).toSet
    assertEquals(derived, handTagged)

  test("system-interface.Random.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Exhaustive match: adding a Request case without updating this table
    // (and EffectMeta.random) surfaces as a non-exhaustive-match warning here.
    def ctorNameOf(r: RandomEffect.Request): String = r match
      case RandomEffect.Request.Bytes(_) => "bytes"
    val scalaReqCases = Set(ctorNameOf(RandomEffect.Request.Bytes(0)))
    val fragmentReqCtors = EffectMeta.random.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.clock composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.clock", List(EffectMeta.clock)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("actionsOf(Clock, ...) matches the hand-tagged Clock actions exactly (incl. the fixed drift)"):
    val derived = EffectMeta.actionsOf(Effects.Family.Clock, EffectMeta.clock).toSet
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Clock).toSet
    assertEquals(derived, handTagged)
    assertEquals(derived.size, 2) // Now + TimestampSlug — was 1 before this slice

  test("system-interface.Clock.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Clock.Request's cases are all parameterless, so .values works directly
    // (unlike Random's Bytes(n: Int), which needed an exhaustive match).
    val scalaReqCases = ClockEffect.Request.values.map(v =>
      (v.toString.head.toLower +: v.toString.tail)).toSet
    val fragmentReqCtors = EffectMeta.clock.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)
