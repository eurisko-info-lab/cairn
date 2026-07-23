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

  /** Proposed tip — Core may construct this; Branches accepts only
    * [[ValidatedTip]] after `apply(language, base, change) = tip`.
    */
  final case class Tip(base: Module, tip: Module, change: Cst):
    def tipDigest: Digest = tip.digest
    def baseDigest: Digest = base.digest

  /** Opaque tip whose `apply(language, base, change) = tip` has been checked.
    * Carries the minting [[Delta.ValidatedChangeSet]].
    */
  opaque type ValidatedTip = ValidatedTip.Repr
  object ValidatedTip:
    private[SemanticRepository] final case class Repr(
        base: Module, tip: Module, change: Cst, vcs: Delta.ValidatedChangeSet)

    private[SemanticRepository] def mint(
        base: Module, tip: Module, change: Cst, vcs: Delta.ValidatedChangeSet
    ): ValidatedTip =
      Repr(base, tip, change, vcs)

    /** Check a proposed [[Tip]]: replay apply and require digest equality. */
    def check(language: ComposedLanguage, proposed: Tip): Either[String, ValidatedTip] =
      Delta.apply(language, proposed.base, proposed.change).flatMap { (result, vcs) =>
        if result.digest != proposed.tip.digest then
          Left(s"tip forgery: apply yielded ${result.digest.short}, claimed ${proposed.tip.digest.short}")
        else if vcs.base != proposed.base.digest then
          Left(s"tip base mismatch: ${vcs.base.short} ≠ ${proposed.base.digest.short}")
        else Right(mint(proposed.base, result, proposed.change, vcs))
      }

    extension (t: ValidatedTip)
      def base: Module = t.base
      def tip: Module = t.tip
      def change: Cst = t.change
      def vcs: Delta.ValidatedChangeSet = t.vcs
      def tipDigest: Digest = t.tip.digest
      def baseDigest: Digest = t.base.digest
      def asTip: Tip = Tip(t.base, t.tip, t.change)

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

  /** Mint a [[ValidatedTip]] by applying `change` to `base`. */
  def tipAfter(
      language: ComposedLanguage,
      base: Module,
      change: Cst,
  ): Either[String, ValidatedTip] =
    commit(language, base, change).map((tip, vcs) => ValidatedTip.mint(base, tip, change, vcs))

  /** Footprint commutation (M16): disjoint writes ⇒ reorderable. */
  def commutes(language: ComposedLanguage, a: Cst, b: Cst): Boolean =
    ChangeAlgebra.commutes(language, a, b)

  /** Three-way semantic merge over change histories (M17). Optional
    * [[ModuleGate]] re-checks the merged module (SDS / Search / …).
    */
  def merge(
      language: ComposedLanguage,
      base: Module,
      changeA: Cst,
      changeB: Cst,
      gate: ModuleGate = ModuleGate.passthrough,
  ): Either[Merge.Conflict, (Module, Delta.ValidatedChangeSet)] =
    Merge.threeWay(language, base, changeA, changeB, gate)

  /** Transport a module across a language migration (M18), then require an
    * optional [[ModuleGate]] on the transported module.
    */
  def migrateModule(
      mig: LangMigration,
      target: ComposedLanguage,
      module: Module,
      gate: ModuleGate = ModuleGate.passthrough,
  ): Either[String, Module] =
    Migrate.module(mig, target, module).flatMap { m2 =>
      ModuleGate.require(gate, m2).map(_ => m2)
    }

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
      gate: ModuleGate = ModuleGate.passthrough,
  ): Either[String, Outcome] =
    merge(language, base, changeA, changeB, gate) match
      case Left(conflict) => Right(Outcome.Conflicted(conflict))
      case Right((merged, vcs)) =>
        migration match
          case None =>
            Right(Outcome.Accepted(merged, vcs, vcs.change, migrated = false))
          case Some((mig, target)) =>
            migrateChange(mig, language, target, vcs.change).flatMap { ch2 =>
              // Transport base without re-running the merge gate; gate the
              // post-migration tip instead (module shape may have changed).
              Migrate.module(mig, target, base).flatMap { base2 =>
                Delta.apply(target, base2, ch2).flatMap { (mod2, vcs2) =>
                  ModuleGate.require(gate, mod2).map(_ =>
                    Outcome.Accepted(mod2, vcs2, ch2, migrated = true))
                }
              }
            }

  /** Integrate two [[ValidatedTip]]s that claim the same base digest. */
  def integrateTips(
      language: ComposedLanguage,
      ours: ValidatedTip,
      theirs: ValidatedTip,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
  ): Either[String, Outcome] =
    if ours.baseDigest != theirs.baseDigest then
      Left(s"branch tips do not share a base: ${ours.baseDigest.short} vs ${theirs.baseDigest.short}")
    else integrate(language, ours.base, ours.change, theirs.change, migration)
