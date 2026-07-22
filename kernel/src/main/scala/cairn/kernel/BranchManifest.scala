package cairn.kernel

/** Branch manifests + append-only history (S18). Heads are stable typed keys
  * stored as named refs; every head update appends, never overwrites history.
  * Merge acceptance advances the head via `system-handler.Branches.merge`
  * (M17 / SemanticRepository); this type remains the pure Kernel record of
  * accepted state.
  *
  * Causal fields (`causalHistoryRoot`, `parents`, `acceptedChange`,
  * `changeHistory`, `conflictState`) are CAS digests so semantic history is
  * reachable from the manifest alone. Refs `.change` / `.changes` sidecars
  * remain write-through caches. Older manifests omit new fields (decoded empty).
  */
final case class BranchManifest(
    branch: String,
    head: Option[TypedKey],
    history: List[TypedKey],
    causalHistoryRoot: Option[Digest] = None,
    parents: List[Digest] = Nil,
    acceptedChange: Option[Digest] = None,
    conflictState: Option[Digest] = None,
    /** Oldest → newest ValidatedChangeSet digests (semantic history). */
    changeHistory: List[Digest] = Nil,
):
  def canon: Canon = Canon.cmap(
    "branch" -> Canon.CStr(branch),
    "head" -> head.fold(Canon.CTag("none", Canon.CInt(0)))(k => Canon.CTag("some", keyCanon(k))),
    "history" -> Canon.CList(history.map(keyCanon)),
    "causalHistoryRoot" -> optDigest(causalHistoryRoot),
    "parents" -> Canon.CList(parents.map(d => Canon.CStr(d.hex))),
    "acceptedChange" -> optDigest(acceptedChange),
    "conflictState" -> optDigest(conflictState),
    "changeHistory" -> Canon.CList(changeHistory.map(d => Canon.CStr(d.hex))))
  def artifact: Artifact = Artifact(ArtifactKind.BranchManifest, canon)
  private def keyCanon(k: TypedKey): Canon = Canon.cmap(
    "kind" -> Canon.CStr(k.kind.name),
    "value" -> Canon.CStr(k.valueHash.hex),
    "type" -> Canon.CStr(k.typeHash.hex))
  private def optDigest(o: Option[Digest]): Canon =
    o.fold(Canon.CTag("none", Canon.CStr("")))(d => Canon.CTag("some", Canon.CStr(d.hex)))

object BranchManifest:
  def fromCanon(c: Canon): BranchManifest =
    import Canon.*
    def key(k: Canon): TypedKey = TypedKey(
      ArtifactKind.parse(k.field("kind").asStr).toOption.get,
      Digest(k.field("value").asStr), Digest(k.field("type").asStr))
    def dig(tag: Canon): Option[Digest] = tag match
      case CTag("some", CStr(hex)) => Some(Digest(hex))
      case _                       => None
    val m = c.asMap
    BranchManifest(
      c.field("branch").asStr,
      c.field("head") match
        case CTag("some", k) => Some(key(k))
        case _               => None,
      c.field("history").asList.map(key),
      causalHistoryRoot = m.get("causalHistoryRoot").flatMap(dig),
      parents = m.get("parents").map(_.asList.map(x => Digest(x.asStr))).getOrElse(Nil),
      acceptedChange = m.get("acceptedChange").flatMap(dig),
      conflictState = m.get("conflictState").flatMap(dig),
      changeHistory = m.get("changeHistory").map(_.asList.map(x => Digest(x.asStr))).getOrElse(Nil))
