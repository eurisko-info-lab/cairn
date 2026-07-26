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
)

/** Where a [[ChangeStep]] reads a value from: a raw parameter of the parsed
  * change term, or a value a prior [[ChangeStep.Bind]] computed.
  */
enum ValueRef:
  case Param(i: Int)
  case Bound(name: String)

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

/** Which expected sort a [[ChangeStep.CheckTerm]] validates a term against. */
enum SortRef:
  /** `language.grammar.top` — what add/replace/edit-without-a-path validate against. */
  case TopSort
  /** A sort discovered by an earlier `Bind(name, ResolveSemanticPath(...))` step. */
  case FromBound(name: String)

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

/** Materializes one of [[Delta.Rejection]]'s existing cases — never a new
  * shape, so every existing `Rejection.render`/`.canon` stays meaningful.
  */
enum RejectionSpec:
  case AlreadyDefinedR(opLabel: String, v: ValueRef)
  case NotDefinedR(opLabel: String, v: ValueRef)
  case StillReferencedR(name: ValueRef, refs: ValueRef)
  case FootprintMismatchR(name: ValueRef, declared: ValueRef, actual: ValueRef)

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

/** Purely syntactic — evaluated over the parsed change term's raw children,
  * no module needed, matching [[ChangeAlgebra.footprint]]'s current nature.
  */
enum FootprintExpr:
  case NameOf(param: Int)
  case NamesOf(param: Int)
  case Union(parts: List[FootprintExpr])

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

/** Names a target operation (by name — possibly different from the source
  * operation, e.g. `add`'s inverse is `remove`) plus how to build its
  * arguments from the original change term and the pre-state module.
  */
final case class InverseExpr(targetOp: String, args: List[InverseArg])

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
)

final case class ChangeModel(operations: List[ChangeOpDef]):
  def find(name: String): Option[ChangeOpDef] = operations.find(_.name == name)

object ChangeModel:
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
