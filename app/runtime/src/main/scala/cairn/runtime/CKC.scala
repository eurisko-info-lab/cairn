package cairn.runtime

import cairn.core.*
import cairn.kernel.*
import cairn.runtime.EffectContexts
import cairn.systemhandler.{DiskCas, FederationFinality, Node}
import cairn.systeminterface.Cas
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
      node: Node,
      cas: Cas,
      closure: List[Digest],
      history: List[Digest],
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
      certValue <- FederationFinality.FederationFinalityCertificate.fromCanon(certArtifact.body)
      proposalArtifact <- cas.getByDigest(proposal)
      proposalValue <- FederationFinality.FederationProposal.fromArtifact(proposalArtifact)
      manifestArtifact <- cas.getByDigest(manifest)
      manifestValue <- ReplicaSetManifest.fromCanon(manifestArtifact.body)
      _ <- FederationFinality.verifyCertificateForProposal(certValue, proposalValue, manifestValue)
    yield Context()
      .withArtifact(certArtifact)
      .withArtifact(proposalArtifact)
      .withArtifact(manifestArtifact)

  private def loadReplay(nodeRoot: String, federationId: Digest, genesisState: Digest): Either[String, ReplayBundle] =
    val node = Node(Path.of(nodeRoot), EffectContexts.forLedger())
    for
      digests <- FederationGc.orderedTransitionDigests(node)
      closure <- FederationGc.permanentHistoryRoots(node, node.cas).map(_.toList.sortBy(_.hex))
      genesisArtifact <- node.cas.getByDigest(genesisState)
      genesis <- FederationState.fromArtifact(genesisArtifact)
    yield ReplayBundle(node, node.cas, closure, digests, genesis)

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
            if bundle.history.length > budget.maxSteps then
              KernelResult.Exhausted(s"max_steps ${budget.maxSteps} exceeded by ${bundle.history.length} transitions")
            else
              FederationHistory.replayFromGenesis(bundle.node, bundle.cas, bundle.genesisState, federationId) match
                case Left(err) => classifyError(err)
                case Right(finalState) =>
                  val report = HistoryReport(bundle.history.length, finalState.digest, bundle.history.length.toLong)
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