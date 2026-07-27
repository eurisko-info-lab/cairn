package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{Keypair, Node}
import cairn.systeminterface.Cas

/** Reconstructs the Language Studio asset graph from an artifact-resolved
  * application. No host-built asset or capability map is accepted. */
object ArtifactLanguageStudio:
  private def id(kind: LanguageAssetKind, name: String) = LanguageAssetId(kind, name)

  def project(application: ResolvedApplication, cas: Cas, languageName: String): Either[String, LanguageStudioProject] = for
    spec <- application.manifest.languages.find(_.name == languageName).toRight(s"application has no language '$languageName'")
    resolved <- application.languages.get(languageName).toRight(s"language '$languageName' was not resolved")
    language <- cas.getByDigest(spec.language)
    grammar <- cas.getByDigest(spec.grammar)
    descriptor <- cas.getByDigest(spec.capabilities)
    semantics <- cas.getByDigest(resolved.descriptor.changeSemantics)
    surface <- cas.getByDigest(resolved.descriptor.changeSurface)
    extensions <- (resolved.descriptor.validation.toList ++ resolved.descriptor.migrations ++
      resolved.descriptor.queries ++ resolved.descriptor.policies ++ resolved.descriptor.projections)
      .foldLeft[Either[String, List[Artifact]]](Right(Nil)) { (acc, digest) =>
        for xs <- acc; artifact <- cas.getByDigest(digest) yield xs :+ artifact }
    extensionAssets <- extensions.foldLeft[Either[String, List[(LanguageAssetId, Artifact)]]](Right(Nil)) {
      (acc, artifact) => acc.flatMap(xs => classify(artifact).map(id => xs :+ (id -> artifact))) }
    assets = Map(
      id(LanguageAssetKind.Language, languageName) -> language,
      id(LanguageAssetKind.Grammar, languageName) -> grammar,
      id(LanguageAssetKind.ChangeSemantics, languageName) -> semantics,
      id(LanguageAssetKind.ChangeSurface, languageName) -> surface,
      id(LanguageAssetKind.LanguageCapabilities, languageName) -> descriptor) ++ extensionAssets
    project = LanguageStudioProject(resolved.language.digest, assets)
    _ <- project.validate
  yield project

  private def classify(artifact: Artifact): Either[String, LanguageAssetId] =
    val kind = artifact.kind match
      case ArtifactKind.ValidationModel => LanguageAssetKind.ValidationModel
      case ArtifactKind.Migration => LanguageAssetKind.Migration
      case ArtifactKind.QueryResult => LanguageAssetKind.Query
      case ArtifactKind.Policy => LanguageAssetKind.Policy
      case ArtifactKind.ProjectionEvidence => LanguageAssetKind.Projection
      case ArtifactKind.ForeignSurface => LanguageAssetKind.ForeignSurface
      case ArtifactKind.StudioProfileSemantics => LanguageAssetKind.StudioProfileSemantics
      case ArtifactKind.StudioProfileSurface => LanguageAssetKind.StudioProfileSurface
      case other => return Left(s"unsupported language capability asset ${other.name}")
    val name = kind match
      case LanguageAssetKind.ValidationModel | LanguageAssetKind.ForeignSurface |
          LanguageAssetKind.StudioProfileSemantics | LanguageAssetKind.StudioProfileSurface => "default"
      case _ => artifact.digest.short
    Right(id(kind, name))

final case class SelfHostingCeremonyResult(
    initialBundle: Digest, successorBundle: Digest, successorApplication: Digest,
    proposal: Digest, witness: Digest, firstAudit: Digest, secondAudit: Digest,
    firstClosure: Set[Digest], secondClosure: Set[Digest], reopenedProject: Digest,
    continuationWitness: Digest,
):
  def reproducible: Boolean = firstAudit == secondAudit && firstClosure == secondClosure

/** The PR25 acceptance ceremony. It starts two empty CASes from one trusted,
  * signed bundle digest and carries a real Language Studio edit through to a
  * published successor that the second node independently reconstructs. */
object SelfHostingCeremony:
  def run(
      initialBundle: Digest, targetLanguage: String, edits: List[LanguageStudioEdit],
      evidence: List[Artifact], publisher: Keypair, node: Node,
      authorities: Map[String, Vector[Byte]], policy: EcosystemTrustPolicy,
      first: Cas, second: Cas,
      metaLanguage: String = "meta", grammarLanguage: String = "grammar",
  ): Either[String, SelfHostingCeremonyResult] =
    for
      published0 <- node.state(authorities).map(_.published)
      initialGraph <- EcosystemReplication.pull(initialBundle, node.cas, first)
      initial <- EcosystemRegistry(first, policy, published0).ingest(initialBundle)
      _ <- Either.cond(initial.release.rootKind == EcosystemRootKind.Application, (), "self-host root is not an application")
      resolver1 = ArtifactApplicationResolver(first)
      application <- resolver1.resolve(initial.release.root)
      project <- ArtifactLanguageStudio.project(application, first, targetLanguage)
      meta <- application.languages.get(metaLanguage).toRight(s"application has no '$metaLanguage' interpreter bundle")
      grammar <- application.languages.get(grammarLanguage).toRight(s"application has no '$grammarLanguage' interpreter bundle")
      selfHosted <- SelfHosting.propose(project, edits, meta, grammar)
      (proposal, witness) = selfHosted
      _ = proposal.result.assets.values.foreach(first.put)
      _ = List(proposal.artifact, witness.artifact).foreach(first.put)
      _ = evidence.foreach(first.put)
      oldSpec <- application.manifest.languages.find(_.name == targetLanguage).toRight("target language disappeared")
      language = proposal.result.assets(LanguageAssetId(LanguageAssetKind.Language, targetLanguage))
      grammarArtifact = proposal.result.assets(LanguageAssetId(LanguageAssetKind.Grammar, targetLanguage))
      capabilities = proposal.result.assets(LanguageAssetId(LanguageAssetKind.LanguageCapabilities, targetLanguage))
      oldRuntime <- application.runtimes.get(targetLanguage).toRight("target language has no domain runtime")
      revisedRuntime = DomainRuntime(language.digest, capabilities.digest, oldRuntime.acceptance.digest)
      _ = first.put(revisedRuntime.artifact)
      revisedSpec = oldSpec.copy(language = language.digest, grammar = grammarArtifact.digest,
        capabilities = capabilities.digest, runtime = Some(revisedRuntime.digest))
      revisedEntries = application.manifest.entries ++ List(
        ApplicationEntry("self-hosting-proposal", proposal.artifact.digest, proposal.artifact.kind),
        ApplicationEntry("self-hosting-witness", witness.artifact.digest, witness.artifact.kind)) ++
        evidence.zipWithIndex.map((a, i) => ApplicationEntry(s"self-hosting-evidence-$i", a.digest, a.kind))
      successor = application.manifest.copy(
        name = application.manifest.name + "-successor",
        languages = application.manifest.languages.map(s => if s.name == targetLanguage then revisedSpec else s),
        entries = revisedEntries)
      _ = first.put(successor.artifact)
      firstResolved <- ArtifactApplicationResolver(first).resolve(successor.digest)
      version = initial.release.version.copy(patch = initial.release.version.patch + 1)
      bundle = EcosystemBundles.sign(initial.release.namespace, version, successor.digest,
        EcosystemRootKind.Application, initial.release.migrations, List(initialBundle), publisher)
      _ = first.put(bundle.artifact)
      firstAudit <- ApplicationHardeningAuditor(first, ArtifactApplicationResolver(first)).audit(bundle.digest)
      _ = first.put(firstAudit.artifact)
      _ <- ArtifactApplicationResolver(node.cas).install(successor.digest, first)
      _ <- EcosystemBundles.publish(bundle, node, publisher, authorities)
      published1 <- node.state(authorities).map(_.published)
      _ <- EcosystemRegistry(node.cas, policy, published1).ingest(bundle.digest)
      secondClosureWithBundle <- EcosystemReplication.pull(bundle.digest, node.cas, second)
      _ <- EcosystemRegistry(second, policy, published1).ingest(bundle.digest)
      resolver2 = ArtifactApplicationResolver(second)
      secondApplication <- resolver2.resolve(successor.digest)
      reopened <- ArtifactLanguageStudio.project(secondApplication, second, targetLanguage)
      secondAudit <- ApplicationHardeningAuditor(second, resolver2).audit(bundle.digest)
      _ <- Either.cond(firstAudit.artifact.digest == secondAudit.artifact.digest, (), "nodes disagree on trusted closure audit")
      // A fresh resolver/project reconstruction is the restart boundary: no process-local identity survives.
      restarted <- ArtifactApplicationResolver(second).resolve(successor.digest)
      reopenedAgain <- ArtifactLanguageStudio.project(restarted, second, targetLanguage)
      _ <- Either.cond(reopenedAgain.digest == reopened.digest, (), "Studio project changed after restart")
      meta2 <- restarted.languages.get(metaLanguage).toRight("restarted application lost Meta capabilities")
      grammar2 <- restarted.languages.get(grammarLanguage).toRight("restarted application lost Grammar capabilities")
      continuationEdit = LanguageStudioEdit(LanguageAssetId(LanguageAssetKind.Grammar, targetLanguage),
        reopenedAgain.assets(LanguageAssetId(LanguageAssetKind.Grammar, targetLanguage)))
      continued <- SelfHosting.propose(reopenedAgain, List(continuationEdit), meta2, grammar2)
      _ = second.put(continued._2.artifact)
      firstClosure = firstResolved.installed ++ initialGraph + bundle.digest
      secondClosure = secondClosureWithBundle
      _ <- Either.cond(firstClosure == secondClosure, (), "successor application graph did not reproduce exactly")
    yield SelfHostingCeremonyResult(initialBundle, bundle.digest, successor.digest,
      proposal.artifact.digest, witness.artifact.digest, firstAudit.artifact.digest,
      secondAudit.artifact.digest, firstClosure, secondClosure, reopened.digest,
      continued._2.artifact.digest)
