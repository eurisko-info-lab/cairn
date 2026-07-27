package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{DurableIo, FederationFinality, Keypair, Node}
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

/** Journal phase reached when a crash interrupts [[FederationTransactionCoordinator.publish]]. */
enum FederationTransactionPhase:
  case None, AfterStaged, AfterProposed, AfterCertified, AfterLedgered

/** PR31's six-phase crash-recoverable transition protocol, generalizing
  * [[AtomicFederation.publish]]/`.recover`'s two-phase (`prepared:`/
  * `published:`) journal to a full [[FederationState]] finalized by real
  * BFT quorum rather than a single authority's ledger append.
  *
  * Phases: '''stage''' (verify + install the complete closure) →
  * '''propose''' (broadcast to the replica quorum) → '''certify'''
  * ([[FederationFinality.agreeNetworkRemote]] mints a
  * [[FederationFinality.FederationFinalityCertificate]]) → '''ledger'''
  * (one batched append: the new state, every touched namespace's head, and
  * the certificate anchor) → '''GC epoch advanced''' (no separate journal
  * entry — the certified/ledgered state's own `gcEpoch` is now usable by
  * [[FederationGc.reclaimAgainstFinalizedEpoch]]; the certificate itself is
  * the authorization) → '''expose''' (`federation.current` updated).
  *
  * Recovery collapses the first three phases to one action: anything before
  * the ledger append has no durable external side effect yet, so a crash
  * there is always safely abandoned (old state stands) — the finer-grained
  * phase vocabulary exists so each boundary can be crash-tested
  * independently, not because recovery needs to distinguish them. Only a
  * crash AFTER the ledger append requires completing forward, and even then
  * only after independently re-verifying the certificate and the ledger's
  * own recorded state — never trusting the journal string alone, mirroring
  * [[AtomicFederation.recover]]'s exact posture.
  */
final class FederationTransactionCoordinator(
    home: Path, source: Cas, node: Node,
    replicaUrls: Map[String, String], activeManifest: ReplicaSetManifest, federationId: Digest,
):
  private val intent = home.resolve("federation-state.intent")
  private val visible = home.resolve("federation-state.current")

  private def write(path: Path, value: String): Either[String, Unit] =
    DurableIo.writeConsensus(path, (value + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private def read(path: Path): Either[String, String] = try
    Right(String(Files.readAllBytes(path), java.nio.charset.StandardCharsets.UTF_8).trim)
  catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.getClass.getSimpleName))

  /** Every namespace whose trust manifest changed (including newly
    * appearing) plus, if it changed, the replica set — the digests
    * [[VerifiedFederationTransition]] requires `transition.approvals` to
    * list as this generation's explicit authority/replica-rotation
    * constituents. Purely derived from `priorState`/`newState` content
    * already resident in `cas`; the caller supplies nothing extra.
    */
  private val emptyNamespaceIndexDigest: Digest = NamespaceIndex(Map.empty).digest

  /** A digest equal to the well-known empty `NamespaceIndex`'s own digest
    * resolves to that empty index without a CAS lookup — content addressing
    * means the digest already tells us what it would decode to. This covers
    * `FederationState.genesis`, which names that digest without ever
    * persisting the (trivial, always-empty) artifact itself.
    */
  private def decodeNamespaceIndex(digest: Digest, cas: Cas): Either[String, NamespaceIndex] =
    if digest == emptyNamespaceIndexDigest then Right(NamespaceIndex(Map.empty))
    else cas.getByDigest(digest).flatMap(NamespaceIndex.fromArtifact)

  private def computeApprovals(priorState: FederationState, newState: FederationState, cas: Cas): Either[String, List[Digest]] =
    for
      priorNs <- decodeNamespaceIndex(priorState.namespaces, cas)
      newNs <- decodeNamespaceIndex(newState.namespaces, cas)
    yield
      val nsRotations = newNs.manifests.toList.collect {
        case (ns, d) if !priorNs.manifests.get(ns).contains(d) => d
      }
      val trustRotation = if priorState.trustRoots != newState.trustRoots then List(newState.trustRoots) else Nil
      nsRotations ++ trustRotation

  def current: Either[String, Option[Digest]] =
    if !Files.exists(visible) then Right(None)
    else read(visible).flatMap(Digest.parse).flatMap(d =>
      node.cas.getByDigest(d).flatMap(FederationTransition.fromArtifact).map(t => Some(t.after)))

  /** `epoch` is the ledger height this generation is anchored at (shared
    * clock with `newState`'s own gcEpoch/namespace-trust activation
    * points); `priorState` is the exact predecessor the minted certificate
    * must chain from. Mints (via `mintCert`, see [[publish]]/
    * [[publishLocalTestOnly]]), verifies, and ledger-anchors (in the same
    * block as `newState` itself) a [[FederationTransition]] binding this
    * whole generation — the canonical, replayable history object (PR32) —
    * though the public return type stays the certificate/block pair
    * callers already depend on; the transition becomes externally visible
    * through `current`/`recover`/[[FederationHistory]].
    *
    * The transition named in the PROPOSAL (`finality = None`) is a
    * DIFFERENT artifact/digest from the one ultimately ledgered
    * (`finality = Some(cert.digest)`) — a replica verifying before any
    * quorum exists structurally checks the former
    * (`VerifiedFederationTransition.verifyStructural`, PR33 slice 4); only
    * once a certificate exists can the latter, fully bound transition be
    * built and pass the full `.verify` (finality-binding included) this
    * function itself still requires before ledgering. Building `approvals`/
    * the pre-cert transition earlier than the original single-transition
    * version did changes no crash-recovery semantics: the `intent` journal's
    * strings/ordering/checkpoints are unchanged, and everything moved
    * earlier is a pure, non-durable, side-effect-free computation.
    */
  private def publishWithCert(
      transactions: List[FederationCommit],
      priorState: FederationState,
      newState: FederationState,
      epoch: Long,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
      crash: FederationTransactionPhase,
      mintCert: FederationFinality.FederationProposal => Either[String, FederationFinality.FederationFinalityCertificate],
  ): Either[String, (FederationFinality.FederationFinalityCertificate, Block)] =
    source.put(newState.artifact)
    for
      _ <- verifyFederationState(newState, source)
      _ <- write(intent, s"staged:${newState.digest.hex}")
      _ <- Either.cond(crash != FederationTransactionPhase.AfterStaged, (), "simulated crash after federation-state staged")
      _ <- ArtifactApplicationResolver(node.cas).install(newState.digest, source).map(_ => ())
      // A shallow put, not a full install: `priorState`'s own bare artifact
      // must be resolvable from `node.cas` for recovery to decode
      // `transition.before` later, but its full closure need not be —
      // genesis's own empty-index digests are never separately persisted
      // anywhere (see VerifiedFederationTransition.decodeIndices), so a
      // recursive install would fail on exactly that case.
      _ = node.cas.put(priorState.artifact)
      // Likewise shallow: a FederationCommit is referenced only through
      // FederationTransition.transactions, never through FederationState's
      // own dependency closure, so `install(newState.digest, ...)` above
      // never reaches it — the ledger transactions below only publish its
      // KEY, not its bytes. Recovery needs the bare artifact to decode it.
      _ = transactions.foreach(c => node.cas.put(c.artifact))
      approvals <- computeApprovals(priorState, newState, source)
      preCertTransition = FederationTransition(priorState.digest, transactions.map(_.digest), newState.digest, approvals, None)
      _ = source.put(preCertTransition.artifact)
      _ = node.cas.put(preCertTransition.artifact)
      proposal = FederationFinality.FederationProposal(
        federationId, preCertTransition.digest, priorState.digest, newState.digest, epoch, activeManifest.replicaSetDigest)
      _ <- write(intent, s"proposed:${newState.digest.hex}:$epoch")
      _ <- Either.cond(crash != FederationTransactionPhase.AfterProposed, (), "simulated crash after federation-state proposed")
      cert <- mintCert(proposal)
      _ = node.cas.put(cert.artifact)
      transition = preCertTransition.copy(finality = Some(cert.digest))
      _ <- VerifiedFederationTransition.verify(transition, priorState, newState, transactions, cert, federationId, source)
      _ = source.put(transition.artifact)
      _ = node.cas.put(transition.artifact)
      _ <- write(intent, s"certified:${newState.digest.hex}:${cert.digest.hex}")
      _ <- Either.cond(crash != FederationTransactionPhase.AfterCertified, (), "simulated crash after federation-state certified")
      block <- node.append(authority, authorities,
        authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes)) ::
        authority.signTx(Tx.PublishArtifact(newState.artifact.key)) ::
        authority.signTx(Tx.PublishArtifact(transition.artifact.key)) ::
        authority.signTx(Tx.RecordCertificate(cert.digest, "federation-finality")) ::
        transactions.flatMap(c => List(
          authority.signTx(Tx.PublishArtifact(c.artifact.key)),
          authority.signTx(Tx.SetBranchHead(s"${c.namespace}/${c.branch}", c.artifact.key)))))
      _ <- write(intent, s"ledgered:${transition.digest.hex}:${block.digest.hex}")
      _ <- Either.cond(crash != FederationTransactionPhase.AfterLedgered, (), "simulated crash after federation-state ledgered")
      _ <- write(visible, transition.digest.hex)
      _ = Files.deleteIfExists(intent)
    yield (cert, block)

  /** Production path: mints the certificate over the real network —
    * `activeManifest`'s OTHER replicas are genuinely independent processes,
    * never private keys this one holds (see [[FederationFinality.agreeNetworkRemote]]).
    */
  def publish(
      transactions: List[FederationCommit],
      priorState: FederationState,
      newState: FederationState,
      epoch: Long,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
      crash: FederationTransactionPhase = FederationTransactionPhase.None,
  ): Either[String, (FederationFinality.FederationFinalityCertificate, Block)] =
    publishWithCert(transactions, priorState, newState, epoch, authority, authorities, crash,
      proposal => FederationFinality.agreeNetworkRemote(replicaUrls, proposal, authority, activeManifest.authorities))

  /** TEST-ONLY: identical crash/journal/ledger machinery as [[publish]],
    * differing only in how the certificate is minted — every replica's
    * state machine runs synchronously in this one call, given every
    * replica's PRIVATE key directly in `replicas` (see
    * [[FederationFinality.agreeForFederationStateLocalTestOnly]]'s own doc
    * comment for why this must never be production's own path). Exists so
    * tests whose actual subject is GC/history/ceremony/crash-recovery
    * plumbing — not the network transport itself, already covered by
    * `FederationNetworkSuite` — don't need to stand up real HTTP replicas
    * for every fixture; the crash-phase/journal logic they exercise is
    * identical either way, since `publishWithCert`'s shared body doesn't
    * care how its `mintCert` argument obtains a certificate.
    */
  def publishLocalTestOnly(
      replicas: List[Keypair],
      transactions: List[FederationCommit],
      priorState: FederationState,
      newState: FederationState,
      epoch: Long,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
      crash: FederationTransactionPhase = FederationTransactionPhase.None,
  ): Either[String, (FederationFinality.FederationFinalityCertificate, Block)] =
    publishWithCert(transactions, priorState, newState, epoch, authority, authorities, crash,
      proposal => FederationFinality.agreeForFederationStateLocalTestOnly(
        replicas, activeManifest, view = 0, stateDigest = proposal.after,
        epoch = proposal.epoch, previousState = proposal.before, federationId = proposal.federationId))

  /** Anything staged/proposed/certified but never ledgered is safely
    * abandoned (old state stands). Only a ledgered generation is completed
    * — and only after re-decoding and fully re-verifying the
    * [[FederationTransition]] the journal names (never trusting the
    * journal string, or even the bare state digest, alone), and
    * cross-checking that BOTH the transition's and the resulting state's
    * ledger keys are actually published.
    */
  def recover(authorities: Map[String, Vector[Byte]]): Either[String, Option[Digest]] =
    if !Files.exists(intent) then current
    else read(intent).flatMap { text => text.split(':').toList match
      case "ledgered" :: digest :: _ :: Nil =>
        val d = Digest(digest)
        for
          transitionArtifact <- node.cas.getByDigest(d)
          transition <- FederationTransition.fromArtifact(transitionArtifact)
          beforeArtifact <- node.cas.getByDigest(transition.before)
          before <- FederationState.fromArtifact(beforeArtifact)
          afterArtifact <- node.cas.getByDigest(transition.after)
          after <- FederationState.fromArtifact(afterArtifact)
          _ <- verifyFederationState(after, node.cas)
          commits <- transition.transactions.foldLeft[Either[String, List[FederationCommit]]](Right(Nil)) { (acc, cd) =>
            for xs <- acc; a <- node.cas.getByDigest(cd); c <- FederationCommit.fromArtifact(a) yield xs :+ c
          }
          finalityDigest <- transition.finality.toRight("federation recovery: transition missing finality")
          finalityArtifact <- node.cas.getByDigest(finalityDigest)
          finality <- FederationFinality.FederationFinalityCertificate.fromCanon(finalityArtifact.body)
          _ <- VerifiedFederationTransition.verify(transition, before, after, commits, finality, federationId, node.cas)
          state <- node.state(authorities)
          _ <- Either.cond(state.published.contains(transitionArtifact.key.render) &&
            state.published.contains(afterArtifact.key.render), (),
            "federation recovery cannot prove the ledger generation")
          _ <- write(visible, d.hex)
          _ = Files.deleteIfExists(intent)
        yield Some(after.digest)
      case ("staged" | "proposed" | "certified") :: _ =>
        Files.deleteIfExists(intent); current
      case _ => Left("invalid federation-state recovery journal") }
