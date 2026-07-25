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
object SemanticPath:

  /** One hop in a path. */
  enum Step:
    /** The child at `position` of a node built by constructor `ctor`
      * (checked against [[CtorDef.argSorts]]). `label`, when `Some`, must
      * match that position's grammar-derived label
      * ([[CtorDef.argLabels]]) — an additional consistency check, not an
      * alternative resolution mechanism; `position` always resolves the
      * walk, so a `None` label (positional-only constructors, e.g. every
      * hand-authored Scala fragment) is always legal.
      */
    case Field(ctor: String, label: Option[String], position: Int)
    /** The child at `position` of a `list`/`some`/`none` wrapper node
      * (Star/Opt/SepBy1 grammar productions) — sort-preserving, no
      * constructor involved. Not a keyed selector: this is the honest
      * positional equivalent of what wrapper-node indexing already does
      * today; resolving a list element by a key match against a field
      * (e.g. SDS's mixture components by `Ref`) is future work.
      */
    case Index(position: Int)

    def canon: Canon = this match
      case Field(ctor, label, position) =>
        Canon.CTag("field", Canon.cmap(
          "ctor" -> Canon.CStr(ctor),
          "label" -> label.fold(Canon.CTag("none", Canon.CInt(0)))(l => Canon.CTag("some", Canon.CStr(l))),
          "position" -> Canon.CInt(position)))
      case Index(position) =>
        Canon.CTag("index", Canon.cmap("position" -> Canon.CInt(position)))

  object Step:
    def fromCanon(c: Canon): Either[String, Step] = c match
      case Canon.CTag("field", m) =>
        val label = m.field("label") match
          case Canon.CTag("some", Canon.CStr(l)) => Some(l)
          case _                                 => None
        Right(Step.Field(m.field("ctor").asStr, label, m.field("position").asInt.toInt))
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

  private[SemanticPath] final case class Repr(
      language: Digest,
      rootSort: String,
      steps: List[Step],
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

  opaque type SemanticPath = Repr

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
  ): SemanticPath = Repr(language, rootSort, steps, focusSort, indices)

  /** One hop of the shared walk: step into constructor `node`'s child at
    * `position`. `claimedCtor`/`claimedLabel`, when `Some`, are checked
    * against the node actually encountered rather than trusted — `None`
    * means "read from the node" (self-deriving / discovery use, see
    * [[fromLegacyPath]]). Returns the child's sort and the child itself.
    */
  private def stepField(
      language: ComposedLanguage,
      node: Cst,
      claimedCtor: Option[String],
      claimedLabel: Option[String],
      position: Int,
  ): Either[String, (String, Cst)] = node match
    case Cst.Node(ctor, children) =>
      claimedCtor match
        case Some(claimed) if claimed != ctor =>
          Left(s"SemanticPath: expected constructor '$claimed', found '$ctor'")
        case _ =>
          language.constructors.get(ctor) match
            case None => Left(s"SemanticPath: unknown constructor '$ctor'")
            case Some(cd) =>
              if position < 0 || position >= cd.argSorts.length || position >= children.length then
                Left(s"path index $position out of range for '$ctor' (${children.length} children)")
              else
                val labelOk = claimedLabel.forall(l => cd.argLabels.lift(position).flatten.contains(l))
                if !labelOk then
                  Left(s"SemanticPath: '$ctor' position $position is not labeled '${claimedLabel.getOrElse("")}'")
                else Right((cd.argSorts(position), children(position)))
    case Cst.Leaf(x) => Left(s"path descends into leaf '$x'")

  /** The only way to obtain a [[SemanticPath]] from an untrusted [[Claim]]:
    * walk `root` per `claim.steps` from `claim.rootSort`, checking at each
    * step that (a) the node encountered matches `Field`'s claimed
    * constructor and has an argSort at `position`, (b) a claimed `label`
    * matches that position's grammar-derived label, (c) `Index` steps only
    * address `list`/`some`/`none` wrapper nodes, (d) the resulting focus
    * sort matches `claim.focusSort` when supplied, and (e) `claim.language`
    * equals `language.digest`. Never trusts the claim's self-reported
    * fields — every one is re-derived from `root`/`language` and compared.
    */
  def verify(language: ComposedLanguage, root: Cst, claim: Claim): Either[String, SemanticPath] =
    def go(t: Cst, sort: String, steps: List[Step], idx: List[Int]): Either[String, (String, List[Int])] =
      steps match
        case Nil => Right((sort, idx))
        case Step.Index(pos) :: rest =>
          t match
            case Cst.Node("list" | "some" | "none", children) if pos >= 0 && pos < children.length =>
              go(children(pos), sort, rest, idx :+ pos)
            case Cst.Node(c, _) =>
              Left(s"SemanticPath: Index($pos) is not legal at '$c' (expected a list/some/none wrapper)")
            case Cst.Leaf(x) => Left(s"SemanticPath: Index($pos) descends into leaf '$x'")
        case Step.Field(ctor, label, pos) :: rest =>
          stepField(language, t, Some(ctor), label, pos).flatMap { (childSort, child) =>
            go(child, childSort, rest, idx :+ pos)
          }
    if language.digest != claim.language then
      Left(s"SemanticPath language mismatch: claim ${claim.language.short} ≠ ${language.digest.short}")
    else
      go(root, claim.rootSort, claim.steps, Nil).flatMap { (discovered, idx) =>
        claim.focusSort match
          case Some(expected) if expected != discovered =>
            Left(s"SemanticPath: expected focus sort '$expected', walk reached '$discovered'")
          case _ => Right(mint(claim.language, claim.rootSort, claim.steps, discovered, idx))
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
              stepField(language, t, None, None, i).flatMap { (childSort, child) =>
                val label = language.constructors.get(ctor).flatMap(_.argLabels.lift(i).flatten)
                go(child, childSort, rest, steps :+ Step.Field(ctor, label, i))
              }
            case Cst.Leaf(x) => Left(s"SemanticPath: path index $i descends into leaf '$x'")
    go(root, language.grammar.top, path, Nil).map { (focusSort, steps) =>
      mint(language.digest, language.grammar.top, steps, focusSort, path)
    }
