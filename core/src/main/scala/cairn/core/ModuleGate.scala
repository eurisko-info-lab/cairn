package cairn.core

import cairn.kernel.*

/** Whole-module validation after ΔL application — the host/judgment gate
  * that [[Merge.threeWay]] commutation alone does not cover (SDS percentage
  * sums, Search board well-formedness, language-specific invariants).
  *
  * `judgment` names the evidence tag committed into
  * [[Merge.ConflictWitness.DomainValidationFailed]]; free-form diagnostic
  * strings are never the sole consensus identity.
  */
final case class ModuleGate(
    judgment: String,
    check: Module => Either[Canon, Unit],
):
  /** `None` when this gate is a no-op (empty judgment). */
  def apply(m: Module): Either[Merge.ConflictWitness, Unit] =
    if judgment.isEmpty then Right(())
    else check(m).left.map(detail =>
      Merge.ConflictWitness.DomainValidationFailed(judgment, detail))

object ModuleGate:
  /** No module-level check — only ΔL + witnessed commutation. */
  val passthrough: ModuleGate = ModuleGate("", _ => Right(()))

  /** Host check whose failure detail is a single canonical string. Prefer
    * structured [[Canon]] payloads when the domain can supply them.
    */
  def host(judgment: String)(f: Module => Either[String, Unit]): ModuleGate =
    ModuleGate(judgment, m => f(m).left.map(Canon.CStr.apply))

  /** Run `gate` after a successful ΔL apply (migration, branch accept,
    * structured edit). Failure is a judgment-tagged [[Canon]] detail.
    */
  def require(gate: ModuleGate, m: Module): Either[String, Unit] =
    gate(m).left.map {
      case Merge.ConflictWitness.DomainValidationFailed(j, detail) =>
        s"module gate '$j' failed: ${detail match
          case Canon.CStr(s) => s
          case other         => other.toString}"
      case other => other.render
    }
