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
  *
  * `certificates` links approval / signing / publication certificate digests
  * stored as CAS `certificate` artifacts — fully referenced by branch state.
  *
  * ## Domain ancestry (ledger as global trunk)
  *
  * The blockchain / ledger is the root of domains — analogous to DNS TLDs
  * (`.org`, `.com`). A branch with `primaryAncestor = None` hangs directly
  * off that trunk (e.g. `LAW`). A subdomain (e.g. `SDS`) sets
  * `primaryAncestor = Some("LAW")` as its **strongest binding**, and may
  * list other branches in `references` (e.g. `CHEMISTRY`) when it also
  * depends on them without making them the primary parent. Thus a branch
  * may have many ancestors via references, or a single strongest ancestor
  * plus optional soft refs.
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
    /** Linked CAS certificate digests (approval / signature / publication). */
    certificates: List[Digest] = Nil,
    /** Strongest domain binding: parent branch name, or `None` = ledger trunk. */
    primaryAncestor: Option[String] = None,
    /** Soft cross-domain references (additional ancestors by name). */
    references: List[String] = Nil,
):
  def canon: Canon = Canon.cmap(
    "branch" -> Canon.CStr(branch),
    "head" -> head.fold(Canon.CTag("none", Canon.CInt(0)))(k => Canon.CTag("some", keyCanon(k))),
    "history" -> Canon.CList(history.map(keyCanon)),
    "causalHistoryRoot" -> optDigest(causalHistoryRoot),
    "parents" -> Canon.CList(parents.map(d => Canon.CStr(d.hex))),
    "acceptedChange" -> optDigest(acceptedChange),
    "conflictState" -> optDigest(conflictState),
    "changeHistory" -> Canon.CList(changeHistory.map(d => Canon.CStr(d.hex))),
    "certificates" -> Canon.CList(certificates.map(d => Canon.CStr(d.hex))),
    "primaryAncestor" -> primaryAncestor.fold(Canon.CTag("none", Canon.CStr("")))(n =>
      Canon.CTag("some", Canon.CStr(n))),
    "references" -> Canon.CList(references.map(Canon.CStr.apply)))
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
    def name(tag: Canon): Option[String] = tag match
      case CTag("some", CStr(n)) if n.nonEmpty => Some(n)
      case _                                   => None
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
      changeHistory = m.get("changeHistory").map(_.asList.map(x => Digest(x.asStr))).getOrElse(Nil),
      certificates = m.get("certificates").map(_.asList.map(x => Digest(x.asStr))).getOrElse(Nil),
      primaryAncestor = m.get("primaryAncestor").flatMap(name),
      references = m.get("references").map(_.asList.map(_.asStr).filter(_.nonEmpty)).getOrElse(Nil))

/** Pure checks for ledger domain ancestry (trunk / primary / references). */
object DomainBranch:
  /** Well-formed when primary and each reference name a known branch (or
    * primary is absent = hang off the global trunk), and the branch does not
    * list itself as ancestor.
    */
  def wellFormed(m: BranchManifest, knownBranches: Set[String]): Either[String, Unit] =
    if m.primaryAncestor.contains(m.branch) then Left(s"domain: '${m.branch}' cannot be its own primary ancestor")
    else if m.references.contains(m.branch) then Left(s"domain: '${m.branch}' cannot reference itself")
    else if m.primaryAncestor.exists(m.references.contains) then
      Left(s"domain: primary ancestor '${m.primaryAncestor.get}' must not also appear in references")
    else
      m.primaryAncestor match
        case Some(p) if !knownBranches.contains(p) =>
          Left(s"domain: primary ancestor '$p' is not a known branch")
        case _ =>
          m.references.find(r => !knownBranches.contains(r)) match
            case Some(r) => Left(s"domain: reference '$r' is not a known branch")
            case None    => Right(())
