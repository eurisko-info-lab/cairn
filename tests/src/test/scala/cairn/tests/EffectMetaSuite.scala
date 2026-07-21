package cairn.tests

import cairn.kernel.*
import cairn.systeminterface.Random as RandomEffect

/** Meta-defined effect interfaces, first slice (post-migration priority #1):
  * `EffectMeta.random` is the template Fragment for the `Random` family.
  * These are mechanical drift guards, same spirit as `ModuleBoundarySuite`.
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
