package cairn.core

import cairn.kernel.*

/** The single content-addressed constitution for executing one domain.
  * Every governed operation names this artifact instead of independently
  * selecting a language, capability bundle, and acceptance constitution.
  */
final case class DomainRuntime(
    language: Digest,
    capabilities: Digest,
    acceptance: Digest,
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex),
    "capabilities" -> Canon.CStr(capabilities.hex),
    "acceptance" -> Canon.CStr(acceptance.hex))
  def artifact: Artifact = Artifact(ArtifactKind.DomainRuntime, canon)
  def digest: Digest = artifact.digest

object DomainRuntime:
  def fromCanon(c: Canon): Either[String, DomainRuntime] =
    try Right(DomainRuntime(
      Digest(c.field("language").asStr),
      Digest(c.field("capabilities").asStr),
      Digest(c.field("acceptance").asStr)))
    catch case e: Exception => Left(s"invalid domain runtime: ${e.getMessage}")

  def fromArtifact(a: Artifact): Either[String, DomainRuntime] =
    if a.kind != ArtifactKind.DomainRuntime then Left("expected domain-runtime artifact")
    else fromCanon(a.body)

/** Fully resolved and cross-checked runtime. Construction is intentionally
  * gated: a constitution from another release cannot be paired with a
  * language or capability bundle merely because their Scala types fit.
  */
final case class ResolvedDomainRuntime private (
    descriptor: DomainRuntime,
    capabilities: ResolvedLanguageCapabilities,
    acceptance: AcceptanceConstitution,
):
  def digest: Digest = descriptor.digest
  def language: ComposedLanguage = capabilities.language
  def changeModel: ChangeModel = capabilities.changeModel
  def moduleGate(resolveProvider: Digest => Option[ComposedLanguage] = _ => None): ModuleGate =
    capabilities.moduleGate(resolveProvider)

object ResolvedDomainRuntime:
  def check(
      descriptor: DomainRuntime,
      capabilities: ResolvedLanguageCapabilities,
      acceptance: AcceptanceConstitution,
  ): Either[String, ResolvedDomainRuntime] =
    for
      _ <- Either.cond(descriptor.language == capabilities.language.digest, (),
        "domain runtime language digest mismatch")
      _ <- Either.cond(descriptor.capabilities == capabilities.descriptor.digest, (),
        "domain runtime capability digest mismatch")
      _ <- Either.cond(descriptor.acceptance == acceptance.digest, (),
        "domain runtime acceptance digest mismatch")
      _ <- Either.cond(acceptance.changeModel == capabilities.changeModel.digest, (),
        "domain runtime acceptance selects a different change model")
      _ <- Either.cond(acceptance.validationModel == capabilities.validation.map(_.digest), (),
        "domain runtime acceptance selects a different validation model")
      _ <- Either.cond(
        acceptance.migrationRules.allowed.subsetOf(capabilities.descriptor.migrations.toSet), (),
        "domain runtime acceptance permits a migration outside the capability bundle")
    yield ResolvedDomainRuntime(descriptor, capabilities, acceptance)

  def create(
      capabilities: ResolvedLanguageCapabilities,
      acceptance: AcceptanceConstitution,
  ): Either[String, ResolvedDomainRuntime] =
    check(DomainRuntime(capabilities.language.digest, capabilities.descriptor.digest, acceptance.digest),
      capabilities, acceptance)
