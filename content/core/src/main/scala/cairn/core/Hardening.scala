package cairn.core

import cairn.kernel.*

/** The deliberately small boundary whose implementation must be trusted.
  * Studio, application assembly and caches are explicitly outside it: their
  * products are checked by these mechanisms before they acquire authority. */
final case class TrustedBoundary(mechanisms: List[String], excluded: List[String]):
  def canon: Canon = Canon.cmap(
    "mechanisms" -> Canon.cstrs(mechanisms.distinct.sorted),
    "excluded" -> Canon.cstrs(excluded.distinct.sorted))
  def digest: Digest = Digest.of(canon)

object TrustedBoundary:
  val minimal: TrustedBoundary = TrustedBoundary(
    List("canonical-codec", "sha256-artifact-identity", "language-checker",
      "change-replay", "validation-checker", "ledger-transition").sorted,
    List("language-studio", "application-resolver", "dependency-cache",
      "surface-renderers", "audit-orchestration").sorted)

final case class SelfHostingWitness(
    baseProject: Digest,
    resultProject: Digest,
    deltaMeta: Option[Digest],
    deltaGrammar: Option[Digest],
    trustedBoundary: Digest,
):
  def canon: Canon = Canon.cmap(
    "baseProject" -> Canon.CStr(baseProject.hex),
    "resultProject" -> Canon.CStr(resultProject.hex),
    "deltaMeta" -> optional(deltaMeta),
    "deltaGrammar" -> optional(deltaGrammar),
    "trustedBoundary" -> Canon.CStr(trustedBoundary.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Provenance,
    Canon.CTag("self-hosting-witness", canon))
  private def optional(value: Option[Digest]): Canon = value.fold[Canon](
    Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex)))

object SelfHosting:
  /** The only self-edit entry point. Both language partitions are delegated to
    * Language Studio and the returned replay witnesses are retained. */
  def propose(
      project: LanguageStudioProject,
      edits: List[LanguageStudioEdit],
      meta: ResolvedLanguageCapabilities,
      grammar: ResolvedLanguageCapabilities,
  ): Either[String, (LanguageStudioProposal, SelfHostingWitness)] =
    LanguageStudio.propose(project, edits, meta, grammar).flatMap { proposal =>
      val metaDigest = proposal.metaWitness.map(_.artifact.digest)
      val grammarDigest = proposal.grammarWitness.map(_.artifact.digest)
      val editedMeta = edits.exists(_.id.kind != LanguageAssetKind.Grammar)
      val editedGrammar = edits.exists(_.id.kind == LanguageAssetKind.Grammar)
      for
        _ <- Either.cond(!editedMeta || metaDigest.nonEmpty, (), "self-host edit lacks a ΔMeta replay witness")
        _ <- Either.cond(!editedGrammar || grammarDigest.nonEmpty, (), "self-host edit lacks a ΔGrammar replay witness")
      yield proposal -> SelfHostingWitness(project.digest, proposal.result.digest,
        metaDigest, grammarDigest, TrustedBoundary.minimal.digest)
    }

final case class HardeningAuditReport(
    root: Digest,
    application: String,
    closure: List[Digest],
    kinds: Map[String, Int],
    languages: Map[String, Digest],
    trustedBoundary: TrustedBoundary,
):
  def canon: Canon = Canon.cmap(
    "root" -> Canon.CStr(root.hex),
    "application" -> Canon.CStr(application),
    "closure" -> Canon.cstrs(closure.distinct.sortBy(_.hex).map(_.hex)),
    "kinds" -> Canon.cmap(kinds.toList.map((k, v) => k -> Canon.CInt(v))*),
    "languages" -> Canon.cmap(languages.toList.map((k, v) => k -> Canon.CStr(v.hex))*),
    "trustedBoundary" -> trustedBoundary.canon)
  def artifact: Artifact = Artifact(ArtifactKind.AuditReport, canon)

object HardeningAuditReport:
  def fromArtifact(artifact: Artifact): Either[String, HardeningAuditReport] =
    if artifact.kind != ArtifactKind.AuditReport then Left("artifact is not a hardening audit report")
    else try Right(HardeningAuditReport(
      Digest(artifact.body.field("root").asStr),
      artifact.body.field("application").asStr,
      artifact.body.field("closure").asList.map(x => Digest(x.asStr)),
      artifact.body.field("kinds").asMap.map((k, v) => k -> v.asInt.toInt).toMap,
      artifact.body.field("languages").asMap.map((k, v) => k -> Digest(v.asStr)).toMap,
      TrustedBoundary(
        artifact.body.field("trustedBoundary").field("mechanisms").asList.map(_.asStr),
        artifact.body.field("trustedBoundary").field("excluded").asList.map(_.asStr))))
    catch case e: Exception => Left(s"invalid hardening audit report: ${e.getMessage}")
