package cairn.kernel

/** Unified effect vocabulary (Phase 3). Policies and Kernel authorization
  * reference digest-bound [[ActionKey]]s derived from effect-interface packs
  * (`iface.cairn` / [[EffectMeta]] / [[EffectBootstrap]]). There is **no**
  * hand-maintained action-case enum — action names come from pack
  * declarations.
  *
  * [[Family]] remains a thin JVM routing tag: its cases must match
  * `packDecls` family ids (checked by EffectBootstrap). Pack → Family is
  * [[Family.forPack]] / [[EffectMeta.packFamily]], not a parallel SoT.
  */
object Effects:
  /** Host routing tag for effect interpreters. Registered from loaded
    * `effect-*` packs via [[EffectMeta.packFamily]]; declaration SoT is
    * [[EffectMeta.InterfaceDecl]] / `iface.cairn`.
    */
  enum Family:
    case Filesystem, Cas, Workspace, Process, Clock, Random,
         LedgerTransport, Terminal, Lsp, ExternalBackend

  object Family:
    def fromId(id: String): Option[Family] =
      values.find(_.toString == id)

    /** Thin bridge: pack name under `languages/` → host Family. */
    def forPack(pack: String): Option[Family] =
      EffectMeta.packFamily.get(pack)

    /** Bootstrap invariant: every enum case is named by some packDecls family id. */
    def idsMatchPackDecls: Boolean =
      values.map(_.toString).toSet == EffectMeta.packDecls.values.map(_.familyId).toSet

  /** Typed action key — family id + capability-class name, optionally bound
    * to the effect-interface Fragment digest.
    *
    * Mint via [[ActionKey.fromPinned]] / [[EffectMeta.EffectFamily.actionKey]]
    * so the interface digest is the CAS pin. Equality includes digest when
    * present so unbound host keys and digest-bound Meta keys stay distinct
    * until both sides bind.
    */
  final case class ActionKey(
      family: String,
      name: String,
      interfaceDigest: Option[Digest] = None):
    def id: String = s"$family.$name"
    /** Same capability class ignoring interface binding. */
    def sameClass(other: ActionKey): Boolean =
      family == other.family && name == other.name

  object ActionKey:
    def of(family: Family, name: String): ActionKey = ActionKey(family.toString, name)
    def bound(family: Family, name: String, digest: Digest): ActionKey =
      ActionKey(family.toString, name, Some(digest))

    /** Mint a digest-bound key only from an accepted [[EffectMeta.PinnedInterface]]. */
    def fromPinned(pinned: EffectMeta.PinnedInterface, name: String): Either[String, ActionKey] =
      if !pinned.actions.contains(name) then
        Left(s"action '$name' not declared on pinned ${pinned.family}")
      else Right(pinned.actionKey(name))
