package cairn.core

import cairn.kernel.*

/** Thin spine composing Cairn's existing change calculus into one operational
  * story (constitution: native semantic repository):
  *
  * {{{
  * branch state
  * → causal semantic change
  * → dependency validation
  * → commutation
  * → merge
  * → conflict artifact
  * → migration
  * → accepted new branch state
  * }}}
  *
  * Pure Core proposals only — [[Delta]], [[ChangeAlgebra]], [[Merge]],
  * [[Migrate]]. Persistence of accepted heads / conflict artifacts lives in
  * `system-handler.Branches`. Not a parallel VCS: wiring of engines Cairn
  * already has.
  */
object SemanticRepository:

  /** A branch tip expressed relative to a shared base module. */
  final case class Tip(base: Module, tip: Module, change: Cst):
    def tipDigest: Digest = tip.digest
    def baseDigest: Digest = base.digest

  /** Outcome of integrating two tips. */
  enum Outcome:
    /** Merged (and optionally migrated) module ready to become a new head. */
    case Accepted(
        module: Module,
        vcs: Delta.ValidatedChangeSet,
        mergedChange: Cst,
        migrated: Boolean,
    )
    /** Structured conflict naming both change-sets — never a textual diff. */
    case Conflicted(conflict: Merge.Conflict)

  /** Causal step: validate + apply a ΔL change against the current tip. */
  def commit(
      language: ComposedLanguage,
      tip: Module,
      change: Cst,
  ): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, tip, change)

  /** Build a [[Tip]] by applying `change` to `base` (validates dependencies). */
  def tipAfter(
      language: ComposedLanguage,
      base: Module,
      change: Cst,
  ): Either[String, Tip] =
    commit(language, base, change).map((tip, _) => Tip(base, tip, change))

  /** Footprint commutation (M16): disjoint writes ⇒ reorderable. */
  def commutes(language: ComposedLanguage, a: Cst, b: Cst): Boolean =
    ChangeAlgebra.commutes(language, a, b)

  /** Three-way semantic merge over change histories (M17). */
  def merge(
      language: ComposedLanguage,
      base: Module,
      changeA: Cst,
      changeB: Cst,
  ): Either[Merge.Conflict, (Module, Delta.ValidatedChangeSet)] =
    Merge.threeWay(language, base, changeA, changeB)

  /** Transport a module across a language migration (M18). */
  def migrateModule(
      mig: LangMigration,
      target: ComposedLanguage,
      module: Module,
  ): Either[String, Module] =
    Migrate.module(mig, target, module)

  /** Transport a change-set across a language migration (M18). */
  def migrateChange(
      mig: LangMigration,
      source: ComposedLanguage,
      target: ComposedLanguage,
      change: Cst,
  ): Either[String, Cst] =
    Migrate.changeset(mig, source, target, change)

  /** Full integrate story for two branch tips that share `base`.
    *
    * 1. Dependency validation is embedded in [[Merge.threeWay]] / [[Delta.apply]].
    * 2. Commutation decides clean merge vs conflict artifact.
    * 3. Optional [[LangMigration]] transports the merged module before accept.
    */
  def integrate(
      language: ComposedLanguage,
      base: Module,
      changeA: Cst,
      changeB: Cst,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
  ): Either[String, Outcome] =
    merge(language, base, changeA, changeB) match
      case Left(conflict) => Right(Outcome.Conflicted(conflict))
      case Right((merged, vcs)) =>
        migration match
          case None =>
            Right(Outcome.Accepted(merged, vcs, vcs.change, migrated = false))
          case Some((mig, target)) =>
            migrateChange(mig, language, target, vcs.change).flatMap { ch2 =>
              migrateModule(mig, target, base).flatMap { base2 =>
                Delta.apply(target, base2, ch2).map { (mod2, vcs2) =>
                  Outcome.Accepted(mod2, vcs2, ch2, migrated = true)
                }
              }
            }

  /** Integrate two [[Tip]]s that claim the same base digest. */
  def integrateTips(
      language: ComposedLanguage,
      ours: Tip,
      theirs: Tip,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
  ): Either[String, Outcome] =
    if ours.baseDigest != theirs.baseDigest then
      Left(s"branch tips do not share a base: ${ours.baseDigest.short} vs ${theirs.baseDigest.short}")
    else integrate(language, ours.base, ours.change, theirs.change, migration)
