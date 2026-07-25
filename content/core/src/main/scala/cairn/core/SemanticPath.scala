package cairn.core

import cairn.kernel.*

/** A Kernel-checked address into a term — the typed replacement for a raw
  * `List[Int]` child-index path as the canonical semantic identity of a
  * structural edit (`edit foo at [0, 2, 1] = ...`). The numeric path is
  * still a valid low-level serialization/surface syntax; `SemanticPath` is
  * what the Kernel actually validates a structural edit against.
  *
  * "Typed" means validated by the dynamic Cairn language (constructor
  * metadata + grammar via [[ComposedLanguage]]/[[CtorDef.argLabels]]), not
  * encoded as static Scala generics — the same sense `ValidatedChangeSet`
  * and `SemanticRepository.ValidatedTip` are "typed."
  *
  * Modeled directly on [[Delta.ValidatedChangeSet]]: an unchecked [[Claim]]
  * anyone can construct from untrusted input, a privately-gated
  * constructor, and [[verify]] as the only path from claim to validated
  * value — by replaying the walk against the actual term and language,
  * never by trusting the claim's self-reported fields.
  */
private[core] final case class SemanticPathRepr(
    language: Digest,
    rootSort: String,
    steps: List[SemanticPath.Step],
    focusSort: String,
    /** Recovered legacy `List[Int]` indices. Format-preserving splice keys
      * source spans by `Cst` node INSTANCE identity (`SpanMap` is an
      * `IdentityHashMap`) — callers that must walk the exact original `Cst`
      * (not a reconstruction) use these with the existing
      * `Delta.subtreeAt`/`replaceAt`, which `SemanticPath` wraps rather
      * than replaces.
      */
    indices: List[Int],
)

opaque type SemanticPath = SemanticPathRepr

object SemanticPath:

  /** One hop in a path. */
  enum Step:
    /** The child of a node built by constructor `ctor`. When [[fieldId]] is
      * `Some`, it is AUTHORITATIVE: the real position is resolved fresh via
      * `cd.fieldIds.indexOf(Some(fieldId))` every time, and `position` is
      * only a witness/cache — a stale `position` (the field moved since
      * this path was captured) is silently corrected, not a failure, as
      * long as the fieldId still resolves to some position. Only when
      * [[fieldId]] is `None` (positional-only constructors, e.g. every
      * hand-authored Scala fragment) does `position` itself resolve the
      * walk. `label`, when `Some`, is always an additional same-commit
      * consistency check against whichever position the walk actually
      * used — never an alternative resolution mechanism.
      */
    case Field(
      ctor: String, label: Option[String], position: Int,
      /** Persistent semantic field identity ([[CtorDef.fieldIds]]) —
        * author-assigned in `ctorDecl`, independent of [[label]] (which is
        * grammar/surface-derived and may drift across surface revisions).
        * `None` where the constructor's argument was declared with the bare
        * (unnamed) `ctorDecl` form. Trailing with a default so every
        * existing 3-arg `Step.Field(ctor, label, position)` call site keeps
        * compiling unchanged.
        */
      fieldId: Option[String] = None,
    )
    /** The child at `position` of a `list`/`some`/`none` wrapper node
      * (Star/Opt/SepBy1 grammar productions) — sort-preserving, no
      * constructor involved. Not a keyed selector: this is the honest
      * positional equivalent of what wrapper-node indexing already does
      * today; resolving a list element by a key match against a field
      * (e.g. SDS's mixture components by `Ref`) is future work.
      */
    case Index(position: Int)

    def canon: Canon = this match
      case Field(ctor, label, position, fieldId) =>
        Canon.CTag("field", Canon.cmap(
          "ctor" -> Canon.CStr(ctor),
          "label" -> label.fold(Canon.CTag("none", Canon.CInt(0)))(l => Canon.CTag("some", Canon.CStr(l))),
          "position" -> Canon.CInt(position),
          "fieldId" -> fieldId.fold(Canon.CTag("none", Canon.CInt(0)))(f => Canon.CTag("some", Canon.CStr(f)))))
      case Index(position) =>
        Canon.CTag("index", Canon.cmap("position" -> Canon.CInt(position)))

  object Step:
    def fromCanon(c: Canon): Either[String, Step] = c match
      case Canon.CTag("field", m) =>
        val label = m.field("label") match
          case Canon.CTag("some", Canon.CStr(l)) => Some(l)
          case _                                 => None
        // Absent on any Step canon persisted before fieldId existed — default
        // to None rather than throwing (`m.field` would on a missing key).
        val fieldId = m.asMap.get("fieldId") match
          case Some(Canon.CTag("some", Canon.CStr(f))) => Some(f)
          case _                                       => None
        Right(Step.Field(m.field("ctor").asStr, label, m.field("position").asInt.toInt, fieldId))
      case Canon.CTag("index", m) =>
        Right(Step.Index(m.field("position").asInt.toInt))
      case other => Left(s"unknown SemanticPath step: $other")

  /** Unchecked claim about a path into a term — decode input, never trust
    * without [[verify]]. `focusSort`: `None` discovers the sort reached by
    * walking `steps` (internal, self-derived use — see [[fromLegacyPath]]);
    * `Some` additionally requires the walk to reach exactly that sort
    * (future untrusted/external claims, e.g. an LSP client).
    */
  final case class Claim(
      language: Digest,
      rootSort: String,
      steps: List[Step],
      focusSort: Option[String] = None,
  )

  extension (p: SemanticPath)
    def language: Digest = p.language
    def rootSort: String = p.rootSort
    def steps: List[Step] = p.steps
    def focusSort: String = p.focusSort
    def indices: List[Int] = p.indices
    def canon: Canon = Canon.cmap(
      "language" -> Canon.CStr(p.language.hex),
      "rootSort" -> Canon.CStr(p.rootSort),
      "steps" -> Canon.CList(p.steps.map(_.canon)),
      "focusSort" -> Canon.CStr(p.focusSort))

  private def mint(
      language: Digest, rootSort: String, steps: List[Step], focusSort: String, indices: List[Int],
  ): SemanticPath = SemanticPathRepr(language, rootSort, steps, focusSort, indices)

  /** One hop of the shared walk: step into constructor `node`'s child.
    * `claimedCtor`/`claimedLabel`, when `Some`, are checked against the node
    * actually encountered rather than trusted — `None` means "read from the
    * node" (self-deriving / discovery use, see [[fromLegacyPath]]).
    *
    * When `claimedFieldId` is `Some`, it — not `position` — resolves which
    * child to step into: the real position is looked up fresh via
    * `cd.fieldIds.indexOf(Some(id))`, so a `position` witness that's gone
    * stale (the field moved since the path was captured) is corrected
    * rather than rejected. `claimedFieldId = None` falls back to `position`
    * exactly as before (positional-only constructors). Returns the child's
    * sort, the child itself, and the position actually used (== `position`
    * unless a fieldId resolution corrected it).
    */
  private def stepField(
      language: ComposedLanguage,
      node: Cst,
      claimedCtor: Option[String],
      claimedLabel: Option[String],
      claimedFieldId: Option[String],
      position: Int,
  ): Either[String, (String, Cst, Int)] = node match
    case Cst.Node(ctor, children) =>
      claimedCtor match
        case Some(claimed) if claimed != ctor =>
          Left(s"SemanticPath: expected constructor '$claimed', found '$ctor'")
        case _ =>
          language.constructors.get(ctor) match
            case None => Left(s"SemanticPath: unknown constructor '$ctor'")
            case Some(cd) =>
              val resolvedPos = claimedFieldId match
                case Some(id) =>
                  cd.fieldIds.indexOf(Some(id)) match
                    case -1  => Left(s"SemanticPath: '$ctor' no longer has a field with fieldId '$id'")
                    case idx => Right(idx)
                case None => Right(position)
              resolvedPos.flatMap { pos =>
                if pos < 0 || pos >= cd.argSorts.length || pos >= children.length then
                  Left(s"path index $pos out of range for '$ctor' (${children.length} children)")
                else
                  val labelOk = claimedLabel.forall(l => cd.argLabels.lift(pos).flatten.contains(l))
                  if !labelOk then
                    Left(s"SemanticPath: '$ctor' position $pos is not labeled '${claimedLabel.getOrElse("")}'")
                  else Right((cd.argSorts(pos), children(pos), pos))
              }
    case Cst.Leaf(x) => Left(s"path descends into leaf '$x'")

  /** The only way to obtain a [[SemanticPath]] from an untrusted [[Claim]]:
    * walk `root` per `claim.steps` from `claim.rootSort`, checking at each
    * step that (a) the node encountered matches `Field`'s claimed
    * constructor and has an argSort at the resolved position, (b) a claimed
    * `label` matches that position's grammar-derived label, (c) `Index`
    * steps only address `list`/`some`/`none` wrapper nodes, (d) the
    * resulting focus sort matches `claim.focusSort` when supplied, and (e)
    * `claim.language` equals `language.digest`. Never trusts the claim's
    * self-reported fields — every one is re-derived from `root`/`language`
    * and compared.
    *
    * The minted [[SemanticPath]]'s steps carry the RESOLVED position for
    * each `Field` hop, not the claim's — when a hop's `fieldId` is `Some`
    * and its stale `position` witness was corrected mid-walk (see
    * [[stepField]]), the correction is baked into the result, so a later
    * caller reading `.steps`/`.indices` sees where the field actually is
    * now, not where the claim thought it was.
    */
  def verify(language: ComposedLanguage, root: Cst, claim: Claim): Either[String, SemanticPath] =
    def go(t: Cst, sort: String, steps: List[Step], idx: List[Int]): Either[String, (String, List[Int], List[Step])] =
      steps match
        case Nil => Right((sort, idx, Nil))
        case Step.Index(pos) :: rest =>
          t match
            case Cst.Node("list" | "some" | "none", children) if pos >= 0 && pos < children.length =>
              go(children(pos), sort, rest, idx :+ pos).map((s, i, tail) => (s, i, Step.Index(pos) :: tail))
            case Cst.Node(c, _) =>
              Left(s"SemanticPath: Index($pos) is not legal at '$c' (expected a list/some/none wrapper)")
            case Cst.Leaf(x) => Left(s"SemanticPath: Index($pos) descends into leaf '$x'")
        case Step.Field(ctor, label, pos, fieldId) :: rest =>
          stepField(language, t, Some(ctor), label, fieldId, pos).flatMap { (childSort, child, resolvedPos) =>
            go(child, childSort, rest, idx :+ resolvedPos).map((s, i, tail) =>
              (s, i, Step.Field(ctor, label, resolvedPos, fieldId) :: tail))
          }
    if language.digest != claim.language then
      Left(s"SemanticPath language mismatch: claim ${claim.language.short} ≠ ${language.digest.short}")
    else
      go(root, claim.rootSort, claim.steps, Nil).flatMap { (discovered, idx, correctedSteps) =>
        claim.focusSort match
          case Some(expected) if expected != discovered =>
            Left(s"SemanticPath: expected focus sort '$expected', walk reached '$discovered'")
          case _ => Right(mint(claim.language, claim.rootSort, correctedSteps, discovered, idx))
      }

  /** Build a [[SemanticPath]] from the legacy raw `List[Int]` representation
    * by walking `root` (starting at `language.grammar.top`, matching
    * `LanguageChecker.expectedSortAt`'s starting point), recovering the
    * `Field`/`Index` step and grammar-derived label at each hop from the
    * term itself. Self-deriving, not an untrusted claim — the recovered
    * steps are correct by construction of the walk, same as
    * `expectedSortAt`'s sort discovery was — but the result is a properly
    * typed, reusable [[SemanticPath]] instead of a bare sort string,
    * closing over the exact same walk `Delta.applyTyped`,
    * `Delta.applyOnePreserving`, and `ChangeAlgebra.invert` each used to
    * perform independently.
    */
  def fromLegacyPath(language: ComposedLanguage, root: Cst, path: List[Int]): Either[String, SemanticPath] =
    def go(t: Cst, sort: String, remaining: List[Int], steps: List[Step]): Either[String, (String, List[Step])] =
      remaining match
        case Nil => Right((sort, steps))
        case i :: rest =>
          t match
            case Cst.Node("list" | "some" | "none", children) if i >= 0 && i < children.length =>
              go(children(i), sort, rest, steps :+ Step.Index(i))
            case Cst.Node("list" | "some" | "none", children) =>
              Left(s"SemanticPath: path index $i out of range (${children.length} children)")
            case Cst.Node(ctor, _) =>
              stepField(language, t, None, None, None, i).flatMap { (childSort, child, _) =>
                val label = language.constructors.get(ctor).flatMap(_.argLabels.lift(i).flatten)
                val fieldId = language.constructors.get(ctor).flatMap(_.fieldIds.lift(i).flatten)
                go(child, childSort, rest, steps :+ Step.Field(ctor, label, i, fieldId))
              }
            case Cst.Leaf(x) => Left(s"SemanticPath: path index $i descends into leaf '$x'")
    go(root, language.grammar.top, path, Nil).map { (focusSort, steps) =>
      mint(language.digest, language.grammar.top, steps, focusSort, path)
    }
