package cairn.systemhandler

import cairn.kernel.{Authority, EffectMeta, Effects}

/** Live effect-interface vocabulary injected into [[EffectContext]] and
  * handlers. Built from disk via [[cairn.runtime.EffectBootstrap.Loaded]] (or
  * cold-start [[seeds]] before bootstrap).
  *
  * Handlers must resolve [[EffectMeta.EffectFamily]] / digest-bound
  * [[Effects.ActionKey]]s through this registry — not a private
  * `EffectMeta.filesystem` (etc.) static seed alone — so an accepted
  * effect-interface artifact change updates live authorization vocabulary.
  */
final case class RuntimeEffectRegistry(
    families: Map[Effects.Family, EffectMeta.EffectFamily],
    pinned: Map[Effects.Family, EffectMeta.PinnedInterface],
):
  def family(f: Effects.Family): Either[String, EffectMeta.EffectFamily] =
    families.get(f).toRight(s"effect family $f not in registry")

  def require(f: Effects.Family): EffectMeta.EffectFamily =
    family(f).fold(e => throw IllegalStateException(e), identity)

  def actionKey(f: Effects.Family, name: String): Either[String, Effects.ActionKey] =
    pinned.get(f) match
      case None => Left(s"family $f not pinned in registry")
      case Some(p) => Effects.ActionKey.fromPinned(p, name)

  def allActionKeys: Set[Effects.ActionKey] =
    families.values.flatMap(_.actionKeys).toSet

  /** True when `key` is declared on the pinned interface for its family. */
  def recognizes(key: Effects.ActionKey): Boolean =
    Effects.Family.fromId(key.family) match
      case None => false
      case Some(f) =>
        pinned.get(f).exists(p =>
          p.actions.contains(key.name) && p.actionKey(key.name) == key)

object RuntimeEffectRegistry:
  /** Cold-start host seeds ([[EffectMeta.families]]) — used until disk
    * bootstrap injects a Loaded registry.
    */
  lazy val seeds: RuntimeEffectRegistry =
    val fams = EffectMeta.families
    val pins = fams.map((f, ef) => f -> EffectMeta.pinHost(ef))
    RuntimeEffectRegistry(fams, pins)

  def fromFamilies(
      families: Map[Effects.Family, EffectMeta.EffectFamily]
  ): RuntimeEffectRegistry =
    val pins = families.map((f, ef) => f -> EffectMeta.pinHost(ef))
    RuntimeEffectRegistry(families, pins)

  /** Rebind policy ActionKeys to this registry's digest-bound vocabulary
    * (same capability class). Composition roots that inject a disk registry
    * must rebind seed-built policies so authorize matches live keys.
    */
  def rebind(
      policies: List[Authority.EffectPolicy],
      registry: RuntimeEffectRegistry,
  ): List[Authority.EffectPolicy] =
    policies.map { p =>
      p.action match
        case "*" => p
        case a: Effects.ActionKey =>
          Effects.Family.fromId(a.family) match
            case None => p
            case Some(f) =>
              registry.actionKey(f, a.name) match
                case Right(k) => p.copy(action = k)
                case Left(_)  => p
    }
