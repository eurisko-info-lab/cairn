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
  * `random` is the template family; the remaining twelve are follow-on
  * slices — see `docs/architecture.md`.
  */
object EffectMeta:
  val random: Fragment = Fragment(
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

  val clock: Fragment = Fragment(
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

  val fragments: Map[Effects.Family, Fragment] = Map(
    Effects.Family.Random -> random,
    Effects.Family.Clock -> clock)

  /** The rights vocabulary for a family, projected from its Fragment: every
    * declared [[Effects.Action]] whose name matches a `Request`-sorted
    * constructor. `Action` stays a closed Scala enum in this slice (widening
    * it to fully mint cases from the Fragment is a separate, larger move),
    * so this is a checked correspondence, not yet full generation — but it
    * makes the Fragment authoritative: an `Action` with no matching
    * constructor, or a constructor with no matching `Action`, is now a
    * detectable drift rather than a silent hand-maintenance gap.
    */
  def actionsOf(family: Effects.Family, fragment: Fragment): List[Effects.Action] =
    Effects.Action.values.toList.filter(a =>
      a.family == family && fragment.constructors.exists(c => c.sort == "Request" && c.name == a.name))
