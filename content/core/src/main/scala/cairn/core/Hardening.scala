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
    MachineComponent.values.toList.map(_.id).sorted,
    List("language-studio", "application-resolver", "dependency-cache",
      "surface-renderers", "audit-orchestration").sorted)

enum TrustBasis:
  case IndependentlyChecked, Replayed, DigestBound, Signed, HostCode, ExternalNativeTool

final case class InterpreterIdentity(name: String, interfaceDigest: Digest, implementationDigest: Digest):
  def canon: Canon = Canon.cmap("name" -> Canon.CStr(name),
    "interface" -> Canon.CStr(interfaceDigest.hex), "implementation" -> Canon.CStr(implementationDigest.hex))

final case class ProviderIdentity(name: String, identity: Digest, basis: TrustBasis):
  def canon: Canon = Canon.cmap("name" -> Canon.CStr(name), "identity" -> Canon.CStr(identity.hex),
    "basis" -> Canon.CStr(basis.toString))

final case class ExternalAssumption(name: String, statement: String):
  def canon: Canon = Canon.cmap("name" -> Canon.CStr(name), "statement" -> Canon.CStr(statement))

final case class TrustedEvidence(artifact: Digest, basis: TrustBasis):
  def canon: Canon = Canon.cmap("artifact" -> Canon.CStr(artifact.hex), "basis" -> Canon.CStr(basis.toString))

/** Complete, machine-readable accounting of why each part of a resolved
  * application is trusted. No evidence category implies another. */
final case class TrustedClosure(
    root: Digest, semanticArtifacts: List[Digest], hostInterpreters: List[InterpreterIdentity],
    nativeProviders: List[ProviderIdentity], externalAssumptions: List[ExternalAssumption],
    checkedEvidence: List[TrustedEvidence],
):
  def normalized: TrustedClosure = copy(
    semanticArtifacts = semanticArtifacts.distinct.sortBy(_.hex),
    hostInterpreters = hostInterpreters.sortBy(_.name),
    nativeProviders = nativeProviders.sortBy(_.name),
    externalAssumptions = externalAssumptions.sortBy(_.name),
    checkedEvidence = checkedEvidence.sortBy(e => (e.artifact.hex, e.basis.toString)))
  def canon: Canon = Canon.cmap(
    "root" -> Canon.CStr(root.hex),
    "semanticArtifacts" -> Canon.cstrs(semanticArtifacts.distinct.sortBy(_.hex).map(_.hex)),
    "hostInterpreters" -> Canon.CList(hostInterpreters.sortBy(_.name).map(_.canon)),
    "nativeProviders" -> Canon.CList(nativeProviders.sortBy(_.name).map(_.canon)),
    "externalAssumptions" -> Canon.CList(externalAssumptions.sortBy(_.name).map(_.canon)),
    "checkedEvidence" -> Canon.CList(checkedEvidence.sortBy(e => (e.artifact.hex, e.basis.toString)).map(_.canon)))

object TrustedClosure:
  val assumptions: List[ExternalAssumption] = List(
    ExternalAssumption("sha-256", "SHA-256 collision and second-preimage resistance"),
    ExternalAssumption("ed25519", "Ed25519 verification authenticates possession of the signing key"),
    ExternalAssumption("durable-io", "successful CAS and ledger writes survive process restart"))

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
    trustedClosure: TrustedClosure,
):
  def canon: Canon = Canon.cmap(
    "root" -> Canon.CStr(root.hex),
    "application" -> Canon.CStr(application),
    "closure" -> Canon.cstrs(closure.distinct.sortBy(_.hex).map(_.hex)),
    "kinds" -> Canon.cmap(kinds.toList.map((k, v) => k -> Canon.CInt(v))*),
    "languages" -> Canon.cmap(languages.toList.map((k, v) => k -> Canon.CStr(v.hex))*),
    "trustedBoundary" -> trustedBoundary.canon,
    "trustedClosure" -> trustedClosure.canon)
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
        artifact.body.field("trustedBoundary").field("excluded").asList.map(_.asStr)),
      trustedClosure(artifact.body.field("trustedClosure"))))
    catch case e: Exception => Left(s"invalid hardening audit report: ${e.getMessage}")

  private def trustedClosure(c: Canon): TrustedClosure =
    def basis(x: Canon) = TrustBasis.values.find(_.toString == x.asStr).getOrElse(throw CodecError("unknown trust basis"))
    TrustedClosure(Digest(c.field("root").asStr),
      c.field("semanticArtifacts").asList.map(x => Digest(x.asStr)),
      c.field("hostInterpreters").asList.map(x => InterpreterIdentity(x.field("name").asStr,
        Digest(x.field("interface").asStr), Digest(x.field("implementation").asStr))),
      c.field("nativeProviders").asList.map(x => ProviderIdentity(x.field("name").asStr,
        Digest(x.field("identity").asStr), basis(x.field("basis")))),
      c.field("externalAssumptions").asList.map(x => ExternalAssumption(x.field("name").asStr, x.field("statement").asStr)),
      c.field("checkedEvidence").asList.map(x => TrustedEvidence(Digest(x.field("artifact").asStr), basis(x.field("basis"))))).normalized
