package cairn.core

import cairn.kernel.*

/** Data description of ΔL's operations — what [[Delta.deltaOf]] (grammar),
  * [[Delta.applyTyped]] (interpretation), and [[ChangeAlgebra.footprint]]/
  * [[ChangeAlgebra.invert]] all drove from three separately hand-written
  * Scala switches over the same operation names. One [[ChangeModel]] now
  * drives all four: adding an operation is adding [[ChangeOpDef]] data, not
  * editing an interpreter (see `ChangeModelCopySuite` for the proof).
  *
  * The three harder existing operations (`remove`/`rename`/`edit`) are not
  * simple insert/replace/delete CRUD — `remove`/`rename` need a whole-module
  * free-reference scan, `rename`'s effect is capture-avoiding substitution
  * across every referencing def (not just the renamed one), and `edit`
  * resolves a [[SemanticPath]] and splices via [[Delta.replaceAt]].
  * Reimplementing those generically from scratch would re-derive already-
  * correct kernel machinery, riskily. Instead, [[ChangeStep]]'s [[ChangeQuery]]/
  * [[Mutation]] leaves are typed invocations of a small, FIXED set of
  * existing primitives (never grows when a new operation is added — only
  * the data in [[ChangeModel.operations]] grows).
  */

/** The four surface shapes a ΔL operation parameter can take today. */
enum ChangeParamKind:
  /** A definition name — `Elem.NameLeaf`; `CtorDef` argSort `"Name"`. */
  case NameK
  /** An object-language term — `Elem.Cat(termCat)`; argSort `termCat`. */
  case TermK
  /** A child-index path, `[0, 2, 1]` — argSort `"Path"`. */
  case PathK
  /** A footprint name list, `[a, b]` — argSort `"Footprint"`. */
  case FootprintK

  def canon: Canon = Canon.CStr(this.toString)

object ChangeParamKind:
  def fromCanon(c: Canon): ChangeParamKind = c match
    case Canon.CStr("NameK")      => NameK
    case Canon.CStr("TermK")      => TermK
    case Canon.CStr("PathK")      => PathK
    case Canon.CStr("FootprintK") => FootprintK
    case other => throw CodecError(s"unknown ChangeParamKind: $other")

final case class ChangeParam(
    fieldName: String,
    kind: ChangeParamKind,
    /** Keyword immediately preceding this parameter's own syntax, if any
      * (`Some("=")` for add/replace/edit's term, `Some("to")`/`Some("footprint")`
      * for rename's 2nd/3rd param, `Some("at")` for edit's path). `None` for
      * a parameter with no introducing token (e.g. rename's `from`, the
      * first parameter right after the operation keyword).
      */
    precedingToken: Option[String] = None,
):
  def canon: Canon = Canon.cmap(
    "fieldName" -> Canon.CStr(fieldName),
    "kind" -> kind.canon,
    "precedingToken" -> precedingToken.fold(Canon.CTag("none", Canon.CInt(0)))(s => Canon.CTag("some", Canon.CStr(s))))

object ChangeParam:
  def fromCanon(c: Canon): ChangeParam =
    val precedingToken = c.field("precedingToken") match
      case Canon.CTag("some", Canon.CStr(s)) => Some(s)
      case _                                 => None
    ChangeParam(c.field("fieldName").asStr, ChangeParamKind.fromCanon(c.field("kind")), precedingToken)

/** Where a [[ChangeStep]] reads a value from: a raw parameter of the parsed
  * change term, or a value a prior [[ChangeStep.Bind]] computed.
  */
enum ValueRef:
  case Param(i: Int)
  case Bound(name: String)

  def canon: Canon = this match
    case Param(i)    => Canon.CTag("param", Canon.CInt(i))
    case Bound(name) => Canon.CTag("bound", Canon.CStr(name))

object ValueRef:
  def fromCanon(c: Canon): ValueRef = c match
    case Canon.CTag("param", Canon.CInt(i)) => Param(i.toInt)
    case Canon.CTag("bound", Canon.CStr(n)) => Bound(n)
    case other => throw CodecError(s"unknown ValueRef: $other")

/** What a [[ChangeStep.Bind]] can produce — a closed, fixed set of shapes
  * every [[ChangeQuery]] result and every parameter decode falls into.
  */
enum BoundValue:
  case VName(name: String)
  case VTerm(term: Cst)
  case VPath(path: List[Int])
  case VNames(names: Set[String])
  /** The sort a resolved [[SemanticPath]] focuses on — the only part of the
    * resolution `edit`'s program actually needs (its splice uses the raw
    * path directly, matching [[Delta.applyTyped]]'s current behavior).
    */
  case VFocusSort(sort: String)

enum BoolExpr:
  case IsDefined(v: ValueRef)
  case Not(e: BoolExpr)
  case NamesEmpty(v: ValueRef)
  case NamesEqual(a: ValueRef, b: ValueRef)

  def canon: Canon = this match
    case IsDefined(v)     => Canon.CTag("is-defined", v.canon)
    case Not(e)           => Canon.CTag("not", e.canon)
    case NamesEmpty(v)    => Canon.CTag("names-empty", v.canon)
    case NamesEqual(a, b) => Canon.CTag("names-equal", Canon.cmap("a" -> a.canon, "b" -> b.canon))

object BoolExpr:
  def fromCanon(c: Canon): BoolExpr = c match
    case Canon.CTag("is-defined", v)  => IsDefined(ValueRef.fromCanon(v))
    case Canon.CTag("not", e)         => Not(fromCanon(e))
    case Canon.CTag("names-empty", v) => NamesEmpty(ValueRef.fromCanon(v))
    case Canon.CTag("names-equal", m) => NamesEqual(ValueRef.fromCanon(m.field("a")), ValueRef.fromCanon(m.field("b")))
    case other => throw CodecError(s"unknown BoolExpr: $other")

/** Which expected sort a [[ChangeStep.CheckTerm]] validates a term against. */
enum SortRef:
  /** `language.grammar.top` — what add/replace/edit-without-a-path validate against. */
  case TopSort
  /** A sort discovered by an earlier `Bind(name, ResolveSemanticPath(...))` step. */
  case FromBound(name: String)

  def canon: Canon = this match
    case TopSort         => Canon.CTag("top-sort", Canon.CInt(0))
    case FromBound(name) => Canon.CTag("from-bound", Canon.CStr(name))

object SortRef:
  def fromCanon(c: Canon): SortRef = c match
    case Canon.CTag("top-sort", _)                => TopSort
    case Canon.CTag("from-bound", Canon.CStr(n))  => FromBound(n)
    case other => throw CodecError(s"unknown SortRef: $other")

/** Reads that may themselves fail (a malformed [[SemanticPath]] claim) —
  * bound via [[ChangeStep.Bind]] before being used by a later [[BoolExpr]]/
  * [[Mutation]]/[[SortRef]].
  */
enum ChangeQuery:
  /** Every OTHER def that free-references `name` — wraps the same
    * `Binding.freeVars`-based scan `remove`/`rename` have always used.
    */
  case ReferencingNames(name: ValueRef)
  /** `module.get(name)` at the CURRENT walk state (used by `copy`'s forward
    * effect — the same primitive [[InverseArg.LookupOld]] uses, just invoked
    * forward instead of at invert time).
    */
  case ReadTerm(name: ValueRef)
  /** Resolves a legacy `List[Int]` path against `name`'s current term via
    * [[SemanticPath.fromLegacyPath]], binding its focus sort.
    */
  case ResolveSemanticPath(name: ValueRef, path: ValueRef)

  def canon: Canon = this match
    case ReferencingNames(name) => Canon.CTag("referencing-names", name.canon)
    case ReadTerm(name)         => Canon.CTag("read-term", name.canon)
    case ResolveSemanticPath(name, path) =>
      Canon.CTag("resolve-semantic-path", Canon.cmap("name" -> name.canon, "path" -> path.canon))

object ChangeQuery:
  def fromCanon(c: Canon): ChangeQuery = c match
    case Canon.CTag("referencing-names", v) => ReferencingNames(ValueRef.fromCanon(v))
    case Canon.CTag("read-term", v)         => ReadTerm(ValueRef.fromCanon(v))
    case Canon.CTag("resolve-semantic-path", m) =>
      ResolveSemanticPath(ValueRef.fromCanon(m.field("name")), ValueRef.fromCanon(m.field("path")))
    case other => throw CodecError(s"unknown ChangeQuery: $other")

/** Module mutations — may themselves fail (a malformed path). */
enum Mutation:
  case InsertDef(name: ValueRef, term: ValueRef)
  case ReplaceDef(name: ValueRef, term: ValueRef)
  case DeleteDef(name: ValueRef)
  /** Wraps [[Delta.replaceAt]] against the raw path (not a resolved
    * [[SemanticPath]] — matches `applyTyped`'s current `edit` case exactly,
    * which splices via the original index list, not `sp.indices`).
    */
  case ReplaceSubtreeAt(name: ValueRef, path: ValueRef, term: ValueRef)
  /** Wraps `Binding.rename` across every def in `footprint` (including the
    * renamed def's own name) — capture-avoiding substitution, not a bare
    * string swap.
    */
  case RenameOccurrences(from: ValueRef, to: ValueRef, footprint: ValueRef)

  def canon: Canon = this match
    case InsertDef(name, term)  => Canon.CTag("insert-def", Canon.cmap("name" -> name.canon, "term" -> term.canon))
    case ReplaceDef(name, term) => Canon.CTag("replace-def", Canon.cmap("name" -> name.canon, "term" -> term.canon))
    case DeleteDef(name)        => Canon.CTag("delete-def", name.canon)
    case ReplaceSubtreeAt(name, path, term) =>
      Canon.CTag("replace-subtree-at", Canon.cmap("name" -> name.canon, "path" -> path.canon, "term" -> term.canon))
    case RenameOccurrences(from, to, footprint) =>
      Canon.CTag("rename-occurrences", Canon.cmap("from" -> from.canon, "to" -> to.canon, "footprint" -> footprint.canon))

object Mutation:
  def fromCanon(c: Canon): Mutation = c match
    case Canon.CTag("insert-def", m)  => InsertDef(ValueRef.fromCanon(m.field("name")), ValueRef.fromCanon(m.field("term")))
    case Canon.CTag("replace-def", m) => ReplaceDef(ValueRef.fromCanon(m.field("name")), ValueRef.fromCanon(m.field("term")))
    case Canon.CTag("delete-def", v)  => DeleteDef(ValueRef.fromCanon(v))
    case Canon.CTag("replace-subtree-at", m) =>
      ReplaceSubtreeAt(ValueRef.fromCanon(m.field("name")), ValueRef.fromCanon(m.field("path")), ValueRef.fromCanon(m.field("term")))
    case Canon.CTag("rename-occurrences", m) =>
      RenameOccurrences(ValueRef.fromCanon(m.field("from")), ValueRef.fromCanon(m.field("to")), ValueRef.fromCanon(m.field("footprint")))
    case other => throw CodecError(s"unknown Mutation: $other")

/** Materializes one of [[Delta.Rejection]]'s existing cases — never a new
  * shape, so every existing `Rejection.render`/`.canon` stays meaningful.
  */
enum RejectionSpec:
  case AlreadyDefinedR(opLabel: String, v: ValueRef)
  case NotDefinedR(opLabel: String, v: ValueRef)
  case StillReferencedR(name: ValueRef, refs: ValueRef)
  case FootprintMismatchR(name: ValueRef, declared: ValueRef, actual: ValueRef)

  def canon: Canon = this match
    case AlreadyDefinedR(opLabel, v) => Canon.CTag("already-defined", Canon.cmap("opLabel" -> Canon.CStr(opLabel), "v" -> v.canon))
    case NotDefinedR(opLabel, v)     => Canon.CTag("not-defined", Canon.cmap("opLabel" -> Canon.CStr(opLabel), "v" -> v.canon))
    case StillReferencedR(name, refs) =>
      Canon.CTag("still-referenced", Canon.cmap("name" -> name.canon, "refs" -> refs.canon))
    case FootprintMismatchR(name, declared, actual) =>
      Canon.CTag("footprint-mismatch", Canon.cmap("name" -> name.canon, "declared" -> declared.canon, "actual" -> actual.canon))

object RejectionSpec:
  def fromCanon(c: Canon): RejectionSpec = c match
    case Canon.CTag("already-defined", m) => AlreadyDefinedR(m.field("opLabel").asStr, ValueRef.fromCanon(m.field("v")))
    case Canon.CTag("not-defined", m)     => NotDefinedR(m.field("opLabel").asStr, ValueRef.fromCanon(m.field("v")))
    case Canon.CTag("still-referenced", m) =>
      StillReferencedR(ValueRef.fromCanon(m.field("name")), ValueRef.fromCanon(m.field("refs")))
    case Canon.CTag("footprint-mismatch", m) =>
      FootprintMismatchR(ValueRef.fromCanon(m.field("name")), ValueRef.fromCanon(m.field("declared")), ValueRef.fromCanon(m.field("actual")))
    case other => throw CodecError(s"unknown RejectionSpec: $other")

/** One step of an operation's program. Preconditions and effect are ONE
  * ordered sequence, not two separate lists — `rename`'s real order
  * interleaves two checks around a computed reference scan, and `edit`
  * resolves a [[SemanticPath]] (which can itself fail) between two checks;
  * neither fits "all checks, then one effect."
  */
enum ChangeStep:
  case Check(require: BoolExpr, onFail: RejectionSpec)
  /** Wraps `LanguageChecker.checkTerm` — the structural gate every op that
    * introduces/replaces a term already runs today.
    */
  case CheckTerm(term: ValueRef, expectedSort: SortRef, nameForError: ValueRef)
  case Bind(as: String, query: ChangeQuery)
  case Mutate(m: Mutation)

  def canon: Canon = this match
    case Check(require, onFail) => Canon.CTag("check", Canon.cmap("require" -> require.canon, "onFail" -> onFail.canon))
    case CheckTerm(term, expectedSort, nameForError) =>
      Canon.CTag("check-term", Canon.cmap("term" -> term.canon, "expectedSort" -> expectedSort.canon, "nameForError" -> nameForError.canon))
    case Bind(as, query) => Canon.CTag("bind", Canon.cmap("as" -> Canon.CStr(as), "query" -> query.canon))
    case Mutate(m)       => Canon.CTag("mutate", m.canon)

object ChangeStep:
  def fromCanon(c: Canon): ChangeStep = c match
    case Canon.CTag("check", m) => Check(BoolExpr.fromCanon(m.field("require")), RejectionSpec.fromCanon(m.field("onFail")))
    case Canon.CTag("check-term", m) =>
      CheckTerm(ValueRef.fromCanon(m.field("term")), SortRef.fromCanon(m.field("expectedSort")), ValueRef.fromCanon(m.field("nameForError")))
    case Canon.CTag("bind", m) => Bind(m.field("as").asStr, ChangeQuery.fromCanon(m.field("query")))
    case Canon.CTag("mutate", m) => Mutate(Mutation.fromCanon(m))
    case other => throw CodecError(s"unknown ChangeStep: $other")

/** Purely syntactic — evaluated over the parsed change term's raw children,
  * no module needed, matching [[ChangeAlgebra.footprint]]'s current nature.
  */
enum FootprintExpr:
  case NameOf(param: Int)
  case NamesOf(param: Int)
  case Union(parts: List[FootprintExpr])

  def canon: Canon = this match
    case NameOf(param)  => Canon.CTag("name-of", Canon.CInt(param))
    case NamesOf(param) => Canon.CTag("names-of", Canon.CInt(param))
    case Union(parts)   => Canon.CTag("union", Canon.CList(parts.map(_.canon)))

object FootprintExpr:
  def fromCanon(c: Canon): FootprintExpr = c match
    case Canon.CTag("name-of", Canon.CInt(i))  => NameOf(i.toInt)
    case Canon.CTag("names-of", Canon.CInt(i)) => NamesOf(i.toInt)
    case Canon.CTag("union", Canon.CList(xs))  => Union(xs.map(fromCanon))
    case other => throw CodecError(s"unknown FootprintExpr: $other")

/** One argument of a constructed inverse change term. */
enum InverseArg:
  /** Raw passthrough of parameter `param` from the ORIGINAL change term.
    * Reordering `Copy` calls against the target op's own parameter order is
    * how `rename`'s inverse "swaps" from/to — no separate swap primitive.
    */
  case Copy(param: Int)
  /** `module.get(name)` as of the walk's PRE-state (before this op ran) —
    * what `replace`/`remove`'s inverses need for the overwritten content.
    */
  case LookupOld(nameParam: Int)
  /** The subtree a path addressed, read from the PRE-state — what `edit`'s
    * inverse needs.
    */
  case SubtreeAtOld(nameParam: Int, pathParam: Int)

  def canon: Canon = this match
    case Copy(param) => Canon.CTag("copy", Canon.CInt(param))
    case LookupOld(nameParam) => Canon.CTag("lookup-old", Canon.CInt(nameParam))
    case SubtreeAtOld(nameParam, pathParam) =>
      Canon.CTag("subtree-at-old", Canon.cmap("nameParam" -> Canon.CInt(nameParam), "pathParam" -> Canon.CInt(pathParam)))

object InverseArg:
  def fromCanon(c: Canon): InverseArg = c match
    case Canon.CTag("copy", Canon.CInt(i))        => Copy(i.toInt)
    case Canon.CTag("lookup-old", Canon.CInt(i))  => LookupOld(i.toInt)
    case Canon.CTag("subtree-at-old", m) =>
      SubtreeAtOld(m.field("nameParam").asInt.toInt, m.field("pathParam").asInt.toInt)
    case other => throw CodecError(s"unknown InverseArg: $other")

/** Names a target operation (by name — possibly different from the source
  * operation, e.g. `add`'s inverse is `remove`) plus how to build its
  * arguments from the original change term and the pre-state module.
  */
final case class InverseExpr(targetOp: String, args: List[InverseArg]):
  def canon: Canon = Canon.cmap("targetOp" -> Canon.CStr(targetOp), "args" -> Canon.CList(args.map(_.canon)))

object InverseExpr:
  def fromCanon(c: Canon): InverseExpr =
    InverseExpr(c.field("targetOp").asStr, c.field("args").asList.map(InverseArg.fromCanon))

final case class ChangeOpDef(
    name: String,
    params: List[ChangeParam],
    program: List[ChangeStep],
    footprint: FootprintExpr,
    inverse: InverseExpr,
    /** Hand-written per operation, same as today — deriving these from
      * `params` via `PrintDerive` is possible but deliberately out of scope
      * for this pass (keeps this refactor to grammar/interpretation/
      * footprint/inversion, not a second simplification at the same time).
      */
    printSegs: List[PrintSeg],
):
  /** `printSegs` is excluded — purely presentational surface syntax,
    * consulted only by `deltaOf`'s grammar generation, never by
    * `run`/`footprintOf`/`inverseOf` — the same reason `Fragment.grammar`
    * itself is excluded from `Fragment.canon`.
    */
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "params" -> Canon.CList(params.map(_.canon)),
    "program" -> Canon.CList(program.map(_.canon)),
    "footprint" -> footprint.canon,
    "inverse" -> inverse.canon)

object ChangeOpDef:
  def fromCanon(c: Canon): ChangeOpDef = ChangeOpDef(
    name = c.field("name").asStr,
    params = c.field("params").asList.map(ChangeParam.fromCanon),
    program = c.field("program").asList.map(ChangeStep.fromCanon),
    footprint = FootprintExpr.fromCanon(c.field("footprint")),
    inverse = InverseExpr.fromCanon(c.field("inverse")),
    printSegs = Nil)

final case class ChangeModel(operations: List[ChangeOpDef]):
  def find(name: String): Option[ChangeOpDef] = operations.find(_.name == name)

  /** Sorted by name — construction order must never affect the digest,
    * mirroring `Module.canon`'s `sorted.defs`.
    */
  def canon: Canon = Canon.cmap(
    "operations" -> Canon.CList(operations.sortBy(_.name).map(_.canon)))
  def artifact: Artifact = Artifact(ArtifactKind.ChangeModel, canon)
  def digest: Digest = artifact.digest

object ChangeModel:
  def fromCanon(c: Canon): ChangeModel =
    ChangeModel(c.field("operations").asList.map(ChangeOpDef.fromCanon))

  private def p(i: Int): ValueRef = ValueRef.Param(i)
  private def b(name: String): ValueRef = ValueRef.Bound(name)

  private val addOp = ChangeOpDef(
    name = "add",
    params = List(ChangeParam("name", ChangeParamKind.NameK), ChangeParam("term", ChangeParamKind.TermK, Some("="))),
    program = List(
      ChangeStep.Check(BoolExpr.Not(BoolExpr.IsDefined(p(0))), RejectionSpec.AlreadyDefinedR("add", p(0))),
      ChangeStep.CheckTerm(p(1), SortRef.TopSort, p(0)),
      ChangeStep.Mutate(Mutation.InsertDef(p(0), p(1)))),
    footprint = FootprintExpr.Union(List(FootprintExpr.NameOf(0))),
    inverse = InverseExpr("remove", List(InverseArg.Copy(0))),
    printSegs = List(
      PrintSeg.Lit("add"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
      PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(";")))

  private val replaceOp = ChangeOpDef(
    name = "replace",
    params = List(ChangeParam("name", ChangeParamKind.NameK), ChangeParam("term", ChangeParamKind.TermK, Some("="))),
    program = List(
      ChangeStep.Check(BoolExpr.IsDefined(p(0)), RejectionSpec.NotDefinedR("replace", p(0))),
      ChangeStep.CheckTerm(p(1), SortRef.TopSort, p(0)),
      ChangeStep.Mutate(Mutation.ReplaceDef(p(0), p(1)))),
    footprint = FootprintExpr.Union(List(FootprintExpr.NameOf(0))),
    inverse = InverseExpr("replace", List(InverseArg.Copy(0), InverseArg.LookupOld(0))),
    printSegs = List(
      PrintSeg.Lit("replace"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
      PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(";")))

  private val removeOp = ChangeOpDef(
    name = "remove",
    params = List(ChangeParam("name", ChangeParamKind.NameK)),
    program = List(
      ChangeStep.Check(BoolExpr.IsDefined(p(0)), RejectionSpec.NotDefinedR("remove", p(0))),
      ChangeStep.Bind("refs", ChangeQuery.ReferencingNames(p(0))),
      ChangeStep.Check(BoolExpr.NamesEmpty(b("refs")), RejectionSpec.StillReferencedR(p(0), b("refs"))),
      ChangeStep.Mutate(Mutation.DeleteDef(p(0)))),
    footprint = FootprintExpr.Union(List(FootprintExpr.NameOf(0))),
    inverse = InverseExpr("add", List(InverseArg.Copy(0), InverseArg.LookupOld(0))),
    printSegs = List(PrintSeg.Lit("remove"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit(";")))

  private val editOp = ChangeOpDef(
    name = "edit",
    params = List(
      ChangeParam("name", ChangeParamKind.NameK),
      ChangeParam("path", ChangeParamKind.PathK, Some("at")),
      ChangeParam("term", ChangeParamKind.TermK, Some("="))),
    program = List(
      ChangeStep.Check(BoolExpr.IsDefined(p(0)), RejectionSpec.NotDefinedR("edit", p(0))),
      ChangeStep.Bind("sp", ChangeQuery.ResolveSemanticPath(p(0), p(1))),
      ChangeStep.CheckTerm(p(2), SortRef.FromBound("sp"), p(0)),
      ChangeStep.Mutate(Mutation.ReplaceSubtreeAt(p(0), p(1), p(2)))),
    footprint = FootprintExpr.Union(List(FootprintExpr.NameOf(0))),
    inverse = InverseExpr("edit", List(InverseArg.Copy(0), InverseArg.Copy(1), InverseArg.SubtreeAtOld(0, 1))),
    printSegs = List(
      PrintSeg.Lit("edit"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
      PrintSeg.Lit("at"), PrintSeg.Space, PrintSeg.Lit("["), PrintSeg.Field(1), PrintSeg.Lit("]"),
      PrintSeg.Space, PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(2), PrintSeg.Lit(";")))

  private val renameOp = ChangeOpDef(
    name = "rename",
    params = List(
      ChangeParam("from", ChangeParamKind.NameK),
      ChangeParam("to", ChangeParamKind.NameK, Some("to")),
      ChangeParam("footprint", ChangeParamKind.FootprintK, Some("footprint"))),
    program = List(
      ChangeStep.Check(BoolExpr.IsDefined(p(0)), RejectionSpec.NotDefinedR("rename", p(0))),
      ChangeStep.Check(BoolExpr.Not(BoolExpr.IsDefined(p(1))), RejectionSpec.AlreadyDefinedR("rename-target", p(1))),
      ChangeStep.Bind("actual", ChangeQuery.ReferencingNames(p(0))),
      ChangeStep.Check(BoolExpr.NamesEqual(p(2), b("actual")), RejectionSpec.FootprintMismatchR(p(0), p(2), b("actual"))),
      ChangeStep.Mutate(Mutation.RenameOccurrences(p(0), p(1), b("actual")))),
    footprint = FootprintExpr.Union(List(FootprintExpr.NameOf(0), FootprintExpr.NameOf(1), FootprintExpr.NamesOf(2))),
    inverse = InverseExpr("rename", List(InverseArg.Copy(1), InverseArg.Copy(0), InverseArg.Copy(2))),
    printSegs = List(
      PrintSeg.Lit("rename"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
      PrintSeg.Lit("to"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
      PrintSeg.Lit("footprint"), PrintSeg.Space, PrintSeg.Lit("["),
      PrintSeg.Field(2), PrintSeg.Lit("]"), PrintSeg.Lit(";")))

  val default: ChangeModel = ChangeModel(List(addOp, replaceOp, removeOp, editOp, renameOp))
