package cairn.kernel

/** Branch manifests + append-only history (S18). Heads are stable typed keys
  * stored as named refs; every head update appends, never overwrites history.
  * Merge acceptance advances the head via `system-handler.Branches.merge`
  * (M17 / SemanticRepository); this type remains the pure Kernel record of
  * accepted state. Pure data — MIGRATION-PLAN.md Phase 1 moves it here (not
  * `system-interface`, where the `Cas` trait it's stored through lives)
  * because its validity is meant to affect accepted repository state, which
  * is a Kernel concern.
  */
final case class BranchManifest(branch: String, head: Option[TypedKey], history: List[TypedKey]):
  def canon: Canon = Canon.cmap(
    "branch" -> Canon.CStr(branch),
    "head" -> head.fold(Canon.CTag("none", Canon.CInt(0)))(k => Canon.CTag("some", keyCanon(k))),
    "history" -> Canon.CList(history.map(keyCanon)))
  def artifact: Artifact = Artifact(ArtifactKind.BranchManifest, canon)
  private def keyCanon(k: TypedKey): Canon = Canon.cmap(
    "kind" -> Canon.CStr(k.kind.name),
    "value" -> Canon.CStr(k.valueHash.hex),
    "type" -> Canon.CStr(k.typeHash.hex))

object BranchManifest:
  def fromCanon(c: Canon): BranchManifest =
    import Canon.*
    def key(k: Canon): TypedKey = TypedKey(
      ArtifactKind.parse(k.field("kind").asStr).toOption.get,
      Digest(k.field("value").asStr), Digest(k.field("type").asStr))
    BranchManifest(
      c.field("branch").asStr,
      c.field("head") match
        case CTag("some", k) => Some(key(k))
        case _               => None,
      c.field("history").asList.map(key))
