package cairn.core

import cairn.kernel.*

enum EquivalenceKind(val id: String):
  case DependencyDiscovery extends EquivalenceKind("dependency-discovery-cache")
  case GrammarInterpreter extends EquivalenceKind("grammar-compiled-parser-printer")
  case RuleInterpreter extends EquivalenceKind("rule-compiled-rewrite-search")
  case ChangeInterpreter extends EquivalenceKind("change-optimized-replay")
  case ProofChecker extends EquivalenceKind("proof-alternate-verifier")
  case RepositorySemantics extends EquivalenceKind("repository-incremental-engine")
  case EffectDispatcher extends EquivalenceKind("effect-native-provider-adapter")
  case MigrationInterpreter extends EquivalenceKind("migration-compiled-migration")

object EquivalenceKind:
  def parse(id: String): Either[String, EquivalenceKind] =
    values.find(_.id == id).toRight(s"unknown semantic-equivalence kind '$id'")

enum AlternativeSource:
  case Optimized
  case ExternalNative(provider: Digest)
  def canon: Canon = this match
    case Optimized => Canon.CTag("optimized", Canon.CInt(0))
    case ExternalNative(provider) => Canon.CTag("external-native", Canon.CStr(provider.hex))

final case class SemanticEquivalence(
    kind: EquivalenceKind, semanticModel: Digest, interpreterImplementation: Digest,
    input: Digest, referenceResult: Digest, alternativeResult: Digest,
    source: AlternativeSource,
):
  def valid: Boolean = referenceResult == alternativeResult
  def canon: Canon = Canon.cmap("kind" -> Canon.CStr(kind.id),
    "semanticModel" -> Canon.CStr(semanticModel.hex),
    "interpreterImplementation" -> Canon.CStr(interpreterImplementation.hex),
    "input" -> Canon.CStr(input.hex), "referenceResult" -> Canon.CStr(referenceResult.hex),
    "alternativeResult" -> Canon.CStr(alternativeResult.hex), "source" -> source.canon)
  def artifact: Artifact = Artifact(ArtifactKind.SemanticEquivalence, canon)

object SemanticEquivalence:
  def outcome(result: Either[String, Canon]): Digest = result match
    case Right(value) => Digest.of(Canon.CTag("success", value))
    case Left(error) => Digest.of(Canon.CTag("failure", Canon.CStr(error)))

  def certify(kind: EquivalenceKind, semanticModel: Digest, implementation: Digest,
      input: Digest, reference: Either[String, Canon], alternative: Either[String, Canon],
      source: AlternativeSource = AlternativeSource.Optimized): Either[String, SemanticEquivalence] =
    val evidence = SemanticEquivalence(kind, semanticModel, implementation, input,
      outcome(reference), outcome(alternative), source)
    Either.cond(evidence.valid, evidence,
      s"${kind.id}: reference ${evidence.referenceResult.short} != alternative ${evidence.alternativeResult.short}")

  def fromArtifact(a: Artifact): Either[String, SemanticEquivalence] =
    if a.kind != ArtifactKind.SemanticEquivalence then Left("not semantic-equivalence evidence")
    else try
      for
        kind <- EquivalenceKind.parse(a.body.field("kind").asStr)
        source <- a.body.field("source") match
          case Canon.CTag("optimized", _) => Right(AlternativeSource.Optimized)
          case Canon.CTag("external-native", Canon.CStr(d)) => Digest.parse(d).map(AlternativeSource.ExternalNative(_))
          case _ => Left("invalid equivalence source")
        evidence = SemanticEquivalence(kind, Digest(a.body.field("semanticModel").asStr),
          Digest(a.body.field("interpreterImplementation").asStr), Digest(a.body.field("input").asStr),
          Digest(a.body.field("referenceResult").asStr), Digest(a.body.field("alternativeResult").asStr), source)
        _ <- Either.cond(evidence.valid, (), "semantic-equivalence evidence records disagreement")
      yield evidence
    catch case e: Exception => Left(s"invalid semantic-equivalence evidence: ${e.getMessage}")
