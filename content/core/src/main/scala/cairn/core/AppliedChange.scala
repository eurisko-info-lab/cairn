package cairn.core

import cairn.kernel.*

/** Derived evidence from one [[ChangeModelInterp.runTraced]] execution — which
  * of the fixed, never-growing [[Mutation]] primitives actually fired, and
  * with what resolved values. Consumed by [[Delta.applyPreservingFormat]] to
  * splice source text without any operation-name switch: the format-preserving
  * layer only needs to understand these 5 cases, never an operation like
  * `copy`.
  *
  * Deliberately carries no `canon`/digest anywhere — this is NOT a semantic
  * input. [[ChangeModel.digest]]/[[Delta.ValidatedChangeSet]] identity is
  * about semantic execution only; concrete trivia behavior must never alter
  * VCS consensus identity. Kept in its own file (not folded into
  * `ChangeModel.scala`) so that boundary stays visually obvious.
  */
final case class AppliedChange(result: Module, trace: List[AppliedMutation], accessTrace: AccessTrace = AccessTrace.empty)

enum AppliedMutation:
  case InsertedDef(name: String, term: Cst)
  case ReplacedDef(name: String, oldTerm: Cst, newTerm: Cst)
  case DeletedDef(name: String, oldTerm: Cst)
  /** `path` is informational (a resolved [[SemanticPath]] for diagnostics/
    * querying) — splicing only needs `oldSubtree`/`newSubtree`, both
    * reference-identical to (or freshly parsed for) the source this trace
    * was produced against.
    */
  case ReplacedSubtree(name: String, path: SemanticPath, oldSubtree: Cst, newSubtree: Cst)
  /** `affectedDefs` is the footprint whose BODY got rewritten — the renamed
    * def's own name change is separately `from`/`to`.
    */
  case RenamedOccurrences(from: String, to: String, affectedDefs: Set[String])
