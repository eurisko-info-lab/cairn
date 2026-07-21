package cairn.tests

import cairn.kernel.*
import cairn.systeminterface.Random as RandomEffect
import cairn.systeminterface.Clock as ClockEffect
import cairn.systeminterface.Process as ProcessEffect
import cairn.systeminterface.ExternalBackend as BackendEffect

/** Meta-defined effect interfaces (post-migration priority #1): `Random`,
  * `Clock`, `Process`, and `ExternalBackend` are Fragment-defined template
  * families, using EffectMeta's many-to-one requestActions grouping (even
  * though each of these four happens to be 1:1). These are mechanical
  * drift guards, same spirit as `ModuleBoundarySuite`.
  */
class EffectMetaSuite extends munit.FunSuite:

  test("EffectMeta.random composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.random", List(EffectMeta.random.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.random's requestActions is complete and matches the hand-tagged Random actions exactly"):
    assertEquals(EffectMeta.completeness(EffectMeta.random, Effects.Family.Random), Nil)
    val derived = EffectMeta.actions(EffectMeta.random)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Random).toSet
    assertEquals(derived, handTagged)

  test("system-interface.Random.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Exhaustive match: adding a Request case without updating this table
    // (and EffectMeta.random) surfaces as a non-exhaustive-match warning here.
    def ctorNameOf(r: RandomEffect.Request): String = r match
      case RandomEffect.Request.Bytes(_) => "bytes"
    val scalaReqCases = Set(ctorNameOf(RandomEffect.Request.Bytes(0)))
    val fragmentReqCtors = EffectMeta.random.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.clock composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.clock", List(EffectMeta.clock.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.clock's requestActions is complete and matches the hand-tagged actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.clock, Effects.Family.Clock), Nil)
    val derived = EffectMeta.actions(EffectMeta.clock)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Clock).toSet
    assertEquals(derived, handTagged)
    assertEquals(derived.size, 2) // Now + TimestampSlug — was 1 before that slice

  test("system-interface.Clock.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Clock.Request's cases are all parameterless, so .values works directly
    // (unlike Random's Bytes(n: Int), which needed an exhaustive match).
    val scalaReqCases = ClockEffect.Request.values.map(v =>
      (v.toString.head.toLower +: v.toString.tail)).toSet
    val fragmentReqCtors = EffectMeta.clock.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.process composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.process", List(EffectMeta.process.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.process's requestActions is complete and matches the hand-tagged actions (no drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.process, Effects.Family.Process), Nil)
    val derived = EffectMeta.actions(EffectMeta.process)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Process).toSet
    assertEquals(derived, handTagged)
    assertEquals(derived.size, 1) // Run — matched cleanly, unlike Clock

  test("system-interface.Process.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Run(command, cwd, mergeStderr) takes parameters, so exhaustive match
    // like Random rather than Clock's .values.
    def ctorNameOf(r: ProcessEffect.Request): String = r match
      case ProcessEffect.Request.Run(_, _, _) => "run"
    val scalaReqCases = Set(ctorNameOf(ProcessEffect.Request.Run(Nil)))
    val fragmentReqCtors = EffectMeta.process.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.externalBackend composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.externalBackend", List(EffectMeta.externalBackend.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.externalBackend's requestActions is complete and matches the hand-tagged actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.externalBackend, Effects.Family.ExternalBackend), Nil)
    val derived = EffectMeta.actions(EffectMeta.externalBackend)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.ExternalBackend).toSet
    assertEquals(derived, handTagged)
    assertEquals(derived.size, 2) // Find + Run — was 1 (Run only) before that slice

  test("system-interface.ExternalBackend.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Find(host) and Run(host, args, cwd) both take parameters, so
    // exhaustive match, same as Random/Process.
    def ctorNameOf(r: BackendEffect.Request): String = r match
      case BackendEffect.Request.Find(_)       => "find"
      case BackendEffect.Request.Run(_, _, _)  => "run"
    val scalaReqCases = Set(
      ctorNameOf(BackendEffect.Request.Find(BackendEffect.Host.ScalaCli)),
      ctorNameOf(BackendEffect.Request.Run(BackendEffect.Host.ScalaCli, Nil)))
    val fragmentReqCtors = EffectMeta.externalBackend.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)
