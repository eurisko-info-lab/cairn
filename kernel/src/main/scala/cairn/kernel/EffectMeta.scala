package cairn.kernel

/** Meta-defined effect interfaces (post-migration priority #1): each effect
  * family's Request/Response/Error vocabulary as a Kernel-owned [[Fragment]]
  * (sorts + constructors, no grammar — effect requests are host-constructed,
  * not user-typed source text), so the family's rights vocabulary is checked
  * against a composable, Kernel-valid description instead of maintained by
  * hand with nothing to keep it honest. Kept in `kernel` (not
  * `system-interface`), matching [[Authority]]'s stated constraint that the
  * action vocabulary stays inside the TCB.
  *
  * Rights are many-to-one: several `Request`-sorted constructors can share
  * one [[Effects.Action]] (e.g. `Filesystem`'s dozen request shapes reduce
  * to three capability classes — read/write/mkdirs — by design, not by
  * accident). `requestActions` makes that grouping an explicit, checked
  * declaration instead of an inferred name match, which only happened to
  * work for the first four families because each had ≤2 distinct request
  * shapes.
  *
  * `random`/`clock`/`process`/`externalBackend` are the converted families;
  * the remaining live ones (`Filesystem`, `Workspace`, `Terminal`, `Lsp`)
  * are follow-on slices — see `docs/architecture.md`.
  */
object EffectMeta:
  /** A Fragment paired with the explicit constructor-name → Action grouping
    * that governs it. `Action` stays a closed Scala enum in this slice
    * (widening it to fully mint cases from the Fragment is a separate,
    * larger move) — this makes the grouping authoritative and checked
    * ([[completeness]]) rather than replacing `Action` outright.
    */
  final case class EffectFamily(fragment: Fragment, requestActions: Map[String, Effects.Action])

  private val randomFragment: Fragment = Fragment(
    name = "effect.random",
    provides = List("effect.random"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("bytes", "Request", List("Int")),
      CtorDef("bytesValue", "Response", List("Bytes")),
      CtorDef("unavailable", "Error", List("Str"))))

  val random: EffectFamily = EffectFamily(randomFragment, Map(
    "bytes" -> Effects.Action.RandomBytes))

  private val clockFragment: Fragment = Fragment(
    name = "effect.clock",
    provides = List("effect.clock"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("now", "Request", Nil),
      CtorDef("timestampSlug", "Request", Nil),
      CtorDef("instant", "Response", List("Int")),
      CtorDef("slug", "Response", List("Str")),
      CtorDef("unavailable", "Error", List("Str"))))

  val clock: EffectFamily = EffectFamily(clockFragment, Map(
    "now" -> Effects.Action.ClockNow,
    "timestampSlug" -> Effects.Action.ClockTimestampSlug))

  private val processFragment: Fragment = Fragment(
    name = "effect.process",
    provides = List("effect.process"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("run", "Request", List("Command", "Path", "Bool")),
      CtorDef("result", "Response", List("Int", "Str", "Str")),
      CtorDef("notFound", "Error", List("Str")),
      CtorDef("io", "Error", List("Str"))))

  val process: EffectFamily = EffectFamily(processFragment, Map(
    "run" -> Effects.Action.ProcessRun))

  private val externalBackendFragment: Fragment = Fragment(
    name = "effect.externalBackend",
    provides = List("effect.externalBackend"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("find", "Request", List("Host")),
      CtorDef("run", "Request", List("Host", "Args", "Path")),
      CtorDef("found", "Response", List("Path")),
      CtorDef("missing", "Response", List("Host")),
      CtorDef("processResult", "Response", List("Int", "Str")),
      CtorDef("io", "Error", List("Str"))))

  val externalBackend: EffectFamily = EffectFamily(externalBackendFragment, Map(
    "find" -> Effects.Action.BackendFind,
    "run" -> Effects.Action.BackendRun))

  val families: Map[Effects.Family, EffectFamily] = Map(
    Effects.Family.Random -> random,
    Effects.Family.Clock -> clock,
    Effects.Family.Process -> process,
    Effects.Family.ExternalBackend -> externalBackend)

  /** The rights vocabulary for a family: every distinct [[Effects.Action]]
    * its `requestActions` grouping declares.
    */
  def actions(family: EffectFamily): Set[Effects.Action] = family.requestActions.values.toSet

  /** Checked correspondence between a family's Fragment and its grouping:
    * every `Request`-sorted constructor must have a `requestActions` entry
    * (no ungated request), every entry must name a real constructor (no
    * dangling grouping), and every mapped `Action` must actually belong to
    * `expected` (no cross-family mistagging). Empty result = consistent.
    */
  def completeness(family: EffectFamily, expected: Effects.Family): List[String] =
    val reqCtorNames = family.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    val mappedNames = family.requestActions.keySet
    val missing = reqCtorNames.diff(mappedNames).toList.map(n =>
      s"constructor '$n' has no requestActions entry")
    val dangling = mappedNames.diff(reqCtorNames).toList.map(n =>
      s"requestActions entry '$n' has no matching Request constructor")
    val misfamily = family.requestActions.toList.collect {
      case (n, a) if a.family != expected =>
        s"requestActions('$n') = $a is tagged ${a.family}, not $expected"
    }
    missing ++ dangling ++ misfamily
