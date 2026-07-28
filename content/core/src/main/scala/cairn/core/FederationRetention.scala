package cairn.core

import cairn.kernel.*

/** PR35: explicit retention governance for federation history.
  *
  * Prior behavior (PR32) implicitly retained transition/state metadata forever
  * while reclaiming superseded semantic closures. This model makes retention
  * a deliberate policy choice, with optional per-namespace overrides.
  */
enum FederationRetentionMode:
  /** Keep only the current finalized generation's closure and live epoch roots. */
  case CurrentStateOnly
  /** Keep thin transition/state metadata spine (PR32 default). */
  case TransitionMetadataOnly
  /** Keep full semantic closure of every historical generation. */
  case FullSemanticHistory
  /** Keep semantic closure only at attested checkpoint states. */
  case CheckpointedArchive

object FederationRetentionMode:
  def fromString(s: String): Either[String, FederationRetentionMode] =
    values.find(_.toString == s).toRight(s"unknown federation retention mode '$s'")

/** Federation retention constitution: default policy plus optional
  * namespace-level overrides and optional checkpoint/archive attestations.
  *
  * Namespace overrides are resolved conservatively in GC by selecting the
  * strongest required retention among participating namespaces.
  */
final case class FederationRetentionConstitution(
    defaultMode: FederationRetentionMode,
    namespaceModes: Map[String, FederationRetentionMode] = Map.empty,
    checkpointStates: Set[Digest] = Set.empty,
    archiveAttestation: Option[Digest] = None,
):
  def canon: Canon = Canon.CTag("federation-retention-v1", Canon.cmap(
    "defaultMode" -> Canon.CStr(defaultMode.toString),
    "namespaceModes" -> Canon.CList(namespaceModes.toList.sortBy(_._1).map { (ns, mode) =>
      Canon.cmap("namespace" -> Canon.CStr(ns), "mode" -> Canon.CStr(mode.toString))
    }),
    "checkpointStates" -> Canon.cstrs(checkpointStates.toList.map(_.hex).sorted),
    "archiveAttestation" -> archiveAttestation.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d =>
      Canon.CTag("some", Canon.CStr(d.hex)))))

  def artifact: Artifact = Artifact(ArtifactKind.Policy, canon)
  def digest: Digest = artifact.digest

  private def strength(mode: FederationRetentionMode): Int = mode match
    case FederationRetentionMode.CurrentStateOnly       => 0
    case FederationRetentionMode.TransitionMetadataOnly => 1
    case FederationRetentionMode.CheckpointedArchive    => 2
    case FederationRetentionMode.FullSemanticHistory    => 3

  /** Conservative effective policy for reclaim: strongest requirement wins. */
  def effectiveMode: FederationRetentionMode =
    (defaultMode :: namespaceModes.values.toList).maxBy(strength)

object FederationRetentionConstitution:
  val transitionMetadataDefault: FederationRetentionConstitution =
    FederationRetentionConstitution(FederationRetentionMode.TransitionMetadataOnly)

  def currentStateOnly: FederationRetentionConstitution =
    FederationRetentionConstitution(FederationRetentionMode.CurrentStateOnly)

  def fullSemanticHistory: FederationRetentionConstitution =
    FederationRetentionConstitution(FederationRetentionMode.FullSemanticHistory)

  def checkpointed(
      checkpointStates: Set[Digest],
      archiveAttestation: Digest,
      defaultMode: FederationRetentionMode = FederationRetentionMode.CheckpointedArchive,
      namespaceModes: Map[String, FederationRetentionMode] = Map.empty,
  ): FederationRetentionConstitution =
    FederationRetentionConstitution(defaultMode, namespaceModes, checkpointStates, Some(archiveAttestation))

  def fromArtifact(artifact: Artifact): Either[String, FederationRetentionConstitution] =
    if artifact.kind != ArtifactKind.Policy then Left("artifact is not a policy")
    else artifact.body match
      case Canon.CTag("federation-retention-v1", m) =>
        try
          val defaultMode = FederationRetentionMode.fromString(m.field("defaultMode").asStr).fold(e => throw CodecError(e), identity)
          val namespaceModes = m.field("namespaceModes").asList.map { row =>
            val ns = row.field("namespace").asStr
            val mode = FederationRetentionMode.fromString(row.field("mode").asStr).fold(e => throw CodecError(e), identity)
            ns -> mode
          }.toMap
          val checkpoints = m.field("checkpointStates").asList.map(x => Digest(x.asStr)).toSet
          val att = m.field("archiveAttestation") match
            case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d))
            case _                                   => None
          Right(FederationRetentionConstitution(defaultMode, namespaceModes, checkpoints, att))
        catch case e: CodecError => Left(e.getMessage)
      case _ => Left("expected federation-retention-v1 body")
