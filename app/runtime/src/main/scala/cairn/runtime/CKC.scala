package cairn.runtime

import cairn.core.*
import cairn.kernel.*
import cairn.systemhandler.{DiskCas, FederationFinality, Node}
import java.nio.file.Path

/** CKC façade for Scala: one query/result vocabulary over the existing
  * federation loaders and verifiers. This keeps the Scala side aligned with
  * the Lean and Rust CKC layers without reintroducing a second semantics
  * stack.
  */
object CKC:
  final case class KernelConstitution(kernelId: String = "ckc-v0")
  final case class Budget(maxSteps: Long = 100000L)

  enum Query:
    case Resolve(casRoot: String, digest: Digest)
    case VerifyCertBinding(casRoot: String, cert: Digest, proposal: Digest, manifest: Digest)
    case ReplayHistory(nodeRoot: String, federationId: Digest, genesisState: Digest)

  enum SemanticQuery:
    case Resolve(digest: Digest)
    case VerifyCertBinding(cert: Digest, proposal: Digest, manifest: Digest)
    case ReplayHistory(federationId: Digest, genesisState: Digest)

  enum Value:
    case ArtifactValue(artifact: Artifact)
    case CertBinding(cert: Digest, proposal: Digest, manifest: Digest)
    case ReplayedState(report: HistoryReport)

  enum KernelResult:
    case Valid(value: Value, evidence: Digest)
    case Invalid(error: String)
    case Missing(closure: List[Digest])
    case Exhausted(limit: String)

  final case class HistoryReport(verifiedTransitions: Int, finalState: Digest, finalEpoch: Long)

  final case class ReplayBundle(
      nodeRoot: String,
      genesisState: FederationState,
  )

  final case class Context(
      artifacts: Map[Digest, Artifact] = Map.empty,
      replay: Option[ReplayBundle] = None,
  ):
    def withArtifact(artifact: Artifact): Context = copy(artifacts = artifacts + (artifact.digest -> artifact))
    def withReplay(bundle: ReplayBundle): Context = copy(replay = Some(bundle))

  private def evidenceOf(constitution: KernelConstitution, query: SemanticQuery, value: Value): Digest =
    val queryCanon = query match
      case SemanticQuery.Resolve(digest) => Canon.CTag("resolve", Canon.CStr(digest.hex))
      case SemanticQuery.VerifyCertBinding(cert, proposal, manifest) => Canon.CTag("verify-cert-binding", Canon.cmap(
        "cert" -> Canon.CStr(cert.hex),
        "proposal" -> Canon.CStr(proposal.hex),
        "manifest" -> Canon.CStr(manifest.hex)))
      case SemanticQuery.ReplayHistory(federationId, genesisState) => Canon.CTag("replay-history", Canon.cmap(
        "federationId" -> Canon.CStr(federationId.hex),
        "genesisState" -> Canon.CStr(genesisState.hex)))
    val valueCanon = value match
      case Value.ArtifactValue(artifact) => Canon.CTag("artifact", Canon.CStr(artifact.digest.hex))
      case Value.CertBinding(cert, proposal, manifest) => Canon.CTag("cert-binding", Canon.cmap(
        "cert" -> Canon.CStr(cert.hex),
        "proposal" -> Canon.CStr(proposal.hex),
        "manifest" -> Canon.CStr(manifest.hex)))
      case Value.ReplayedState(report) => Canon.CTag("replayed-state", Canon.cmap(
        "verifiedTransitions" -> Canon.CInt(report.verifiedTransitions),
        "finalState" -> Canon.CStr(report.finalState.hex),
        "finalEpoch" -> Canon.CInt(report.finalEpoch)))
    Digest.ofBytes(Canon.encode(Canon.cmap(
      "kernelId" -> Canon.CStr(constitution.kernelId),
      "query" -> queryCanon,
      "value" -> valueCanon)))

  private def classifyError(message: String): KernelResult =
    if message.startsWith("kernel exhausted:") then
      KernelResult.Exhausted(message.stripPrefix("kernel exhausted:").trim)
    else if message.contains("not in CAS") then
      KernelResult.Missing(Nil)
    else KernelResult.Invalid(message)

  private def loadResolve(casRoot: String, digest: Digest): Either[String, Context] =
    val cas = DiskCas(Path.of(casRoot))
    cas.getByDigest(digest).map(artifact => Context().withArtifact(artifact))

  private def loadVerifyCert(casRoot: String, cert: Digest, proposal: Digest, manifest: Digest): Either[String, Context] =
    val cas = DiskCas(Path.of(casRoot))
    for
      certArtifact <- cas.getByDigest(cert)
      proposalArtifact <- cas.getByDigest(proposal)
      manifestArtifact <- cas.getByDigest(manifest)
    yield Context()
      .withArtifact(certArtifact)
      .withArtifact(proposalArtifact)
      .withArtifact(manifestArtifact)

  private def loadReplay(nodeRoot: String, federationId: Digest, genesisState: Digest): Either[String, ReplayBundle] =
    val cas = DiskCas(Path.of(nodeRoot))
    val _ = federationId
    for
      genesisArtifact <- cas.getByDigest(genesisState)
      decodedGenesis <- FederationState.fromArtifact(genesisArtifact)
    yield ReplayBundle(nodeRoot, decodedGenesis)

  private def deriveSemantic(constitution: KernelConstitution, budget: Budget, query: SemanticQuery, context: Context): KernelResult =
    query match
      case SemanticQuery.Resolve(digest) =>
        context.artifacts.get(digest) match
          case Some(artifact) => KernelResult.Valid(Value.ArtifactValue(artifact), evidenceOf(constitution, query, Value.ArtifactValue(artifact)))
          case None => KernelResult.Missing(List(digest))

      case SemanticQuery.VerifyCertBinding(cert, proposal, manifest) =>
        context.artifacts.get(cert) match
          case None => KernelResult.Missing(List(cert))
          case Some(certArtifact) =>
            context.artifacts.get(proposal) match
              case None => KernelResult.Missing(List(proposal))
              case Some(proposalArtifact) =>
                context.artifacts.get(manifest) match
                  case None => KernelResult.Missing(List(manifest))
                  case Some(manifestArtifact) =>
                    (for
                      certValue <- FederationFinality.FederationFinalityCertificate.fromCanon(certArtifact.body)
                      proposalValue <- FederationFinality.FederationProposal.fromArtifact(proposalArtifact)
                      manifestValue <- ReplicaSetManifest.fromCanon(manifestArtifact.body)
                      _ <- FederationFinality.verifyCertificateForProposal(certValue, proposalValue, manifestValue)
                    yield () ) match
                      case Left(err) => classifyError(err)
                      case Right(_) =>
                        val value = Value.CertBinding(cert, proposal, manifest)
                        KernelResult.Valid(value, evidenceOf(constitution, query, value))

      case SemanticQuery.ReplayHistory(federationId, genesisState) =>
        context.replay match
          case None => KernelResult.Invalid("missing replay bundle")
          case Some(bundle) =>
            val node = Node(Path.of(bundle.nodeRoot), EffectContexts.forLedger())
            FederationGc.orderedTransitionDigests(node) match
              case Left(err) => classifyError(err)
              case Right(history) =>
                if history.length > budget.maxSteps then
                  KernelResult.Exhausted(s"max_steps ${budget.maxSteps} exceeded by ${history.length} transitions")
                else
                  FederationHistory.replayFromGenesis(node, node.cas, bundle.genesisState, federationId) match
                    case Left(err) => classifyError(err)
                    case Right(finalState) =>
                      val epochArtifact = node.cas.getByDigest(finalState.gcEpoch) match
                        case Left(err) => return classifyError(err)
                        case Right(artifact) => artifact
                      ReplicatedGcEpoch.fromArtifact(epochArtifact) match
                        case Left(err) => classifyError(err)
                        case Right(epoch) =>
                          val report = HistoryReport(epoch.number.toInt, finalState.digest, epoch.number)
                          val value = Value.ReplayedState(report)
                          KernelResult.Valid(value, evidenceOf(constitution, query, value))

  def derive(constitution: KernelConstitution, budget: Budget, query: Query): KernelResult =
    query match
      case Query.Resolve(casRoot, digest) =>
        loadResolve(casRoot, digest) match
          case Left(err) => classifyError(err)
          case Right(context) => deriveSemantic(constitution, budget, SemanticQuery.Resolve(digest), context)

      case Query.VerifyCertBinding(casRoot, cert, proposal, manifest) =>
        loadVerifyCert(casRoot, cert, proposal, manifest) match
          case Left(err) => classifyError(err)
          case Right(context) => deriveSemantic(constitution, budget, SemanticQuery.VerifyCertBinding(cert, proposal, manifest), context)

      case Query.ReplayHistory(nodeRoot, federationId, genesisState) =>
        loadReplay(nodeRoot, federationId, genesisState) match
          case Left(err) => classifyError(err)
          case Right(bundle) =>
            deriveSemantic(constitution, budget, SemanticQuery.ReplayHistory(federationId, genesisState), Context(replay = Some(bundle)))