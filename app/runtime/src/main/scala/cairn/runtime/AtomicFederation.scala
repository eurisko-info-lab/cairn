package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{DurableIo, Keypair, Node}
import java.nio.file.{Files, Path}

enum FederationCrashPoint:
  case None, AfterPrepare, AfterLedger

final case class FederationPublication(commit: FederationCommit, block: Block)

/** PR31's atomic-visibility invariant, generalized from [[AtomicFederation.verify]]
  * (one namespace) to a complete [[FederationState]] (every namespace): a
  * node must never expose a new branch, release, or ledger head unless it
  * possesses and has certified the complete corresponding repository,
  * runtime, machine, evidence, and application closure — for ALL six
  * `FederationState` roots, not just the one namespace/branch being updated.
  *
  * Composes exactly the functions the invariant names: [[NativeRepository.
  * verifyFromRoots]] per namespace (repository closure — "certified" here
  * means each resident change's acceptance-evidence artifact genuinely
  * decodes as [[AcceptanceEvidence]], the same depth [[AtomicFederation.verify]]
  * already checks; full ΔL re-replay already happened once at PR30
  * ingestion time and is not re-run on every visibility check), and
  * [[ArtifactApplicationResolver.audit]]/`.resolve` (application/machine
  * closure, and — via the full walk from `state.digest` — every other root
  * transitively).
  */
def verifyFederationState(state: FederationState, cas: Cas): Either[String, Set[Digest]] =
  def certifyStructurally(c: CausalChange): Either[String, Unit] =
    c.acceptanceEvidence match
      case None => Right(())
      case Some(d) => cas.getByDigest(d).flatMap(a =>
        Either.cond(a.kind == ArtifactKind.AcceptanceEvidence, (), s"federation state: evidence ${d.short} has wrong kind"))
  for
    _ <- cas.getByDigest(state.ledger)
    repoIndexArtifact <- cas.getByDigest(state.repository)
    repoIndex <- RepositoryIndex.fromArtifact(repoIndexArtifact)
    _ <- repoIndex.namespaces.toList.foldLeft[Either[String, Unit]](Right(())) { case (acc, (ns, d)) =>
      acc.flatMap(_ => cas.getByDigest(d).flatMap(NativeRepository.fromArtifact).flatMap { repo =>
        repo.verifyFromRoots(certifyStructurally).left.map(e => s"federation state: namespace '$ns' repository: $e").map(_ => ())
      })
    }
    appIndexArtifact <- cas.getByDigest(state.applications)
    appIndex <- ApplicationIndex.fromArtifact(appIndexArtifact)
    _ <- appIndex.releases.toList.foldLeft[Either[String, Unit]](Right(())) { case (acc, (ns, d)) =>
      acc.flatMap(_ => ArtifactApplicationResolver(cas).resolve(d).left.map(e => s"federation state: namespace '$ns' application: $e").map(_ => ()))
    }
    nsIndexArtifact <- cas.getByDigest(state.namespaces)
    nsIndex <- NamespaceIndex.fromArtifact(nsIndexArtifact)
    _ <- nsIndex.manifests.toList.foldLeft[Either[String, Unit]](Right(())) { case (acc, (ns, d)) =>
      acc.flatMap(_ => cas.getByDigest(d).flatMap(NamespaceTrustManifest.fromArtifact)
        .left.map(e => s"federation state: namespace '$ns' trust manifest: $e").map(_ => ()))
    }
    trustArtifact <- cas.getByDigest(state.trustRoots)
    _ <- ReplicaSetManifest.fromCanon(trustArtifact.body).left.map(e => s"federation state: trust roots: $e")
    epochArtifact <- cas.getByDigest(state.gcEpoch)
    _ <- ReplicatedGcEpoch.fromArtifact(epochArtifact)
    closure <- ArtifactApplicationResolver(cas).audit(state.digest)
  yield closure

/** Recoverable bridge between content closure and one ledger generation.
  * The ledger points at the federation commit, never at a branch module whose
  * repository/runtime/application roots could be published separately. */
final class AtomicFederation(home: Path, source: Cas, node: Node):
  private val intent = home.resolve("federation.intent")
  private val visible = home.resolve("federation.current")

  private def write(path: Path, value: String): Either[String, Unit] =
    DurableIo.writeConsensus(path, (value + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private def read(path: Path): Either[String, String] = try
    Right(String(Files.readAllBytes(path), java.nio.charset.StandardCharsets.UTF_8).trim)
  catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.getClass.getSimpleName))

  def current: Either[String, Option[Digest]] =
    if !Files.exists(visible) then Right(None) else read(visible).flatMap(Digest.parse).map(Some(_))

  def verify(commit: FederationCommit, cas: Cas = source): Either[String, Set[Digest]] = for
    graphArtifact <- cas.getByDigest(commit.repositoryGraph)
    graph <- NativeRepository.fromArtifact(graphArtifact)
    branchArtifact <- cas.getByDigest(commit.branchView)
    _ <- Either.cond(branchArtifact.kind == ArtifactKind.BranchManifest, (), "federation branch view has wrong kind")
    branch = BranchManifest.fromCanon(branchArtifact.body)
    _ <- Either.cond(branch.branch == commit.branch && branch.repositoryGraph.contains(commit.repositoryGraph) &&
      branch.acceptanceEvidence.contains(commit.acceptanceEvidence) && branch.domainRuntime.contains(commit.runtime), (),
      "federation branch view does not bind its graph, evidence, and runtime")
    evidence <- cas.getByDigest(commit.acceptanceEvidence)
    _ <- Either.cond(evidence.kind == ArtifactKind.AcceptanceEvidence, (), "federation acceptance evidence has wrong kind")
    runtimeArtifact <- cas.getByDigest(commit.runtime)
    _ <- DomainRuntime.fromArtifact(runtimeArtifact)
    application <- ArtifactApplicationResolver(cas).resolve(commit.application)
    _ <- Either.cond(application.runtimes.values.exists(_.digest == commit.runtime), (),
      "federation application does not select the branch runtime")
    releaseArtifact <- cas.getByDigest(commit.ecosystemRelease)
    release <- SignedEcosystemBundle.fromArtifact(releaseArtifact)
    _ <- Either.cond(release.release.namespace == commit.namespace && release.release.root == commit.application, (),
      "federation release does not publish the selected namespace/application")
    epochArtifact <- cas.getByDigest(commit.gcEpoch)
    epoch <- ReplicatedGcEpoch.fromArtifact(epochArtifact)
    required = graph.gcRoots ++ Set(commit.repositoryGraph, commit.branchView, commit.acceptanceEvidence,
      commit.runtime, commit.application, commit.ecosystemRelease)
    _ <- Either.cond(required.subsetOf(epoch.roots), (), "federation GC epoch omits live generation roots")
    closure <- ArtifactApplicationResolver(cas).audit(commit.digest)
  yield closure

  def publish(
      commit: FederationCommit, authority: Keypair, authorities: Map[String, Vector[Byte]],
      crash: FederationCrashPoint = FederationCrashPoint.None,
  ): Either[String, FederationPublication] =
    source.put(commit.artifact)
    for
      _ <- verify(commit)
      _ <- ArtifactApplicationResolver(node.cas).install(commit.digest, source).map(_ => ())
      _ <- write(intent, s"prepared:${commit.digest.hex}")
      _ <- Either.cond(crash != FederationCrashPoint.AfterPrepare, (), "simulated crash after federation prepare")
      block <- node.append(authority, authorities, List(
        authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes)),
        authority.signTx(Tx.PublishArtifact(commit.artifact.key)),
        authority.signTx(Tx.SetBranchHead(s"${commit.namespace}/${commit.branch}", commit.artifact.key))))
      _ <- write(intent, s"published:${commit.digest.hex}:${block.digest.hex}")
      _ <- Either.cond(crash != FederationCrashPoint.AfterLedger, (), "simulated crash after federation ledger append")
      _ <- write(visible, commit.digest.hex)
      _ = Files.deleteIfExists(intent)
    yield FederationPublication(commit, block)

  /** Prepared-only work is safely abandoned. A ledger-published generation is
    * completed after independently checking both its closure and ledger head. */
  def recover(authorities: Map[String, Vector[Byte]]): Either[String, Option[Digest]] =
    if !Files.exists(intent) then current
    else read(intent).flatMap { text => text.split(':').toList match
      case "prepared" :: digest :: Nil =>
        Files.deleteIfExists(intent); current
      case "published" :: digest :: _ :: Nil =>
        val d = Digest(digest)
        for
          artifact <- node.cas.getByDigest(d)
          commit <- FederationCommit.fromArtifact(artifact)
          _ <- verify(commit, node.cas)
          state <- node.state(authorities)
          _ <- Either.cond(state.published.contains(artifact.key.render) &&
            state.heads.get(s"${commit.namespace}/${commit.branch}").contains(artifact.key), (),
            "federation recovery cannot prove the ledger generation")
          _ <- write(visible, d.hex)
          _ = Files.deleteIfExists(intent)
        yield Some(d)
      case _ => Left("invalid federation recovery journal") }
