package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{CasAdmin, CasAdminEffects, EffectContext, FederationFinality, Node}
import java.nio.file.Path

/** PR31's "replicated GC safety" invariant: deletion requires a finalized
  * retention epoch proving that no accepted repository head, pending
  * causal change, release, evidence artifact, or migration route still
  * reaches the object.
  *
  * [[BranchRefStore.liveCasRoots]] already correctly computes the live-root
  * set for ONE namespace (branch heads, causal graph + pending changes via
  * `NativeRepository.gcRoots`, and resident `CertifiedCausalChange`
  * evidence via the `repository.certifications` ledger) — this module
  * aggregates that ACROSS every namespace and adds the two pieces
  * `liveCasRoots` cannot see on its own: ecosystem releases and the
  * application/machine closures they and each namespace's current runtime
  * select. `EcosystemBundle`'s own [[ArtifactDependencies.direct]] entry
  * already walks `migrations`/`previous` transitively (confirmed: a
  * release's dependency graph already includes its migration artifacts and
  * every prior release it supersedes) — auditing each LIVE release's root
  * is therefore sufficient; no separate "migration route" artifact type is
  * needed to keep migrations reachable.
  */
object FederationGc:
  private def casErr(e: Cas.Error): String = e match
    case Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
    case Cas.Error.Io(m)      => m

  private def foldRoots[A](items: Iterable[A])(f: A => Either[String, Set[Digest]]): Either[String, Set[Digest]] =
    items.foldLeft[Either[String, Set[Digest]]](Right(Set.empty)) { (acc, item) =>
      for xs <- acc; ys <- f(item) yield xs ++ ys
    }

  /** Full required-roots set for a federation-wide GC epoch: every
    * namespace's live causal-graph roots (already including pending
    * changes and certification evidence), plus every namespace's currently
    * selected application/machine closure, plus every currently-live
    * ecosystem release's closure (which transitively keeps its migrations
    * and prior releases reachable).
    */
  def computeFederationGcRoots(
      namespaces: Map[String, Branches],
      applications: Map[String, Digest],
      liveReleases: Map[String, SignedEcosystemBundle],
      resolver: ArtifactApplicationResolver,
  ): Either[String, Set[Digest]] =
    for
      repoRoots <- foldRoots(namespaces.values)(_.liveCasRoots())
      appRoots <- foldRoots(applications.values)(resolver.audit)
      releaseRoots <- foldRoots(liveReleases.values)(r => resolver.audit(r.digest))
    yield repoRoots ++ appRoots ++ releaseRoots

  /** Every [[FederationTransition]] ever ledger-anchored, in the order
    * `node.blocks` (itself hash-linked, height-ordered) recorded them —
    * this IS the federation's hash-linked transition history; no separate
    * index or pointer structure is needed to enumerate it.
    */
  def orderedTransitionDigests(node: Node): Either[String, List[Digest]] =
    node.blocks.map(_.flatMap(_.txs.collect {
      case SignedTx(Tx.PublishArtifact(key), _, _) if key.kind == ArtifactKind.FederationTransition => key.valueHash
    }))

  private val emptyNamespaceIndexDigest: Digest = NamespaceIndex(Map.empty).digest

  /** [[VerifiedFederationTransition]] independently re-decodes each
    * `NamespaceTrustManifest` a `NamespaceIndex` entry names (to check
    * amendment policy against its predecessor), so — unlike
    * `RepositoryIndex`/`ApplicationIndex` entries, whose bodies the checker
    * never dereferences, only compares digests for — these ARE artifacts
    * replay needs resolvable. Still small, still flat (one level past the
    * index itself, never the repository/application content a namespace's
    * OTHER index entries point to).
    */
  private def namespaceTrustDigests(state: FederationState, cas: Cas): Either[String, Set[Digest]] =
    if state.namespaces == emptyNamespaceIndexDigest then Right(Set.empty)
    else cas.getByDigest(state.namespaces).flatMap(NamespaceIndex.fromArtifact).map(_.manifests.values.toSet)

  /** The federation's transition/state history is small, metadata-only
    * content (digests and short field lists — never a repository/release
    * body), so retaining it forever is cheap and doesn't require retaining
    * the heavy content it references (which stays exactly as reclaimable as
    * [[reclaimAgainstFinalizedEpoch]]'s existing current-closure protection
    * already makes it). This is a FLAT, one-to-two-level union over every
    * transition ever published — deliberately not a recursive
    * [[ArtifactApplicationResolver.audit]] walk, which would pull in the
    * full historic `NativeRepository`/application/release closures these
    * digests reference and defeat GC entirely.
    */
  def permanentHistoryRoots(node: Node, cas: Cas): Either[String, Set[Digest]] =
    for
      digests <- orderedTransitionDigests(node)
      roots <- digests.foldLeft[Either[String, Set[Digest]]](Right(Set.empty)) { (acc, td) =>
        for
          xs <- acc
          transitionArtifact <- cas.getByDigest(td)
          transition <- FederationTransition.fromArtifact(transitionArtifact)
          beforeArtifact <- cas.getByDigest(transition.before)
          before <- FederationState.fromArtifact(beforeArtifact)
          afterArtifact <- cas.getByDigest(transition.after)
          after <- FederationState.fromArtifact(afterArtifact)
          beforeNsDigests <- namespaceTrustDigests(before, cas)
          afterNsDigests <- namespaceTrustDigests(after, cas)
          // PR33.1: history replay (`VerifiedFederationTransition.verify`)
          // independently fetches the certified PROPOSAL by digest to check
          // the certificate's projections against it — the proposal is
          // referenced only from inside the certificate's own body (never
          // from `transition.dependencies`), so it must be retained
          // explicitly or replay-after-GC would lose its verification input.
          proposalDigests <- transition.finality match
            case None => Right(Set.empty[Digest])
            case Some(fd) =>
              for
                certArtifact <- cas.getByDigest(fd)
                cert <- FederationFinality.FederationFinalityCertificate.fromCanon(certArtifact.body)
              yield Set(cert.proposal)
        yield xs + transition.digest ++ transition.dependencies ++ before.dependencies ++ after.dependencies ++
          beforeNsDigests ++ afterNsDigests ++ proposalDigests
      }
    yield roots

  /** Reclaim is only ever driven by the last epoch that survived BFT
    * finality, never by a node's transient local view: `certificate` must
    * independently verify against `activeManifest`, name exactly
    * `latestFinalized`'s digest as its finalized subject, and only then is
    * `latestFinalized.gcEpoch`'s recorded `roots` handed to the existing
    * local mark/sweep. A locally-computed-but-not-yet-finalized epoch (e.g.
    * one built via [[computeFederationGcRoots]] mid-transaction, before
    * quorum agreement) must never reach [[CasAdminEffects.gc]] directly.
    *
    * The swept root set is `epoch.roots` UNIONED with the full transitive
    * closure of `latestFinalized` itself (`ArtifactApplicationResolver.audit`).
    * `epoch.roots` is deliberately unable to name its own generation's
    * bookkeeping artifacts (the state, its indices, and the epoch artifact
    * itself) — a generation's epoch is computed before it or its state
    * exist, the same way a git commit can't embed its own hash — so without
    * this union, the very act of finalizing a new generation would make
    * that generation's own wrapper artifacts immediately unreachable to the
    * next reclaim. `epoch.roots` remains the authority for what OLDER,
    * otherwise-unreachable content survives; this audit is what guarantees
    * the CURRENT, just-finalized state is always live regardless.
    */
  def reclaimAgainstFinalizedEpoch(
      casRoot: Path,
      latestFinalized: FederationState,
      cas: Cas,
      certificate: FederationFinality.FederationFinalityCertificate,
      activeManifest: ReplicaSetManifest,
      federationId: Digest,
      ctx: EffectContext,
      node: Node,
  ): Either[String, CasAdmin.GcReport] =
    for
      _ <- Either.cond(certificate.federationId == federationId, (), "federation gc: certificate federation id mismatch")
      _ <- Either.cond(certificate.stateDigest == latestFinalized.digest, (),
        "federation gc: certificate does not finalize the candidate epoch's state")
      // Shared cert↔proposal verifier (PR33.1 slice 4): `certificate.stateDigest`
      // above decides what content SURVIVES reclamation, so it must be a
      // verified projection of the signed proposal — not a raw certificate
      // field checked only for quorum seals. The proposal artifact is always
      // CAS-resident for a published generation (publishWithCert persists it).
      proposalArtifact <- cas.getByDigest(certificate.proposal)
      proposal <- FederationFinality.FederationProposal.fromArtifact(proposalArtifact)
      _ <- FederationFinality.verifyCertificateForProposal(certificate, proposal, activeManifest)
      epochArtifact <- cas.getByDigest(latestFinalized.gcEpoch)
      epoch <- ReplicatedGcEpoch.fromArtifact(epochArtifact)
      currentClosure <- ArtifactApplicationResolver(cas).audit(latestFinalized.digest)
      historyRoots <- permanentHistoryRoots(node, cas)
      report <- CasAdminEffects.gc(casRoot, epoch.roots ++ currentClosure ++ historyRoots, ctx).left.map(casErr)
    yield report
