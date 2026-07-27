package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{CasAdmin, CasAdminEffects, EffectContext, FederationFinality}
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
  ): Either[String, CasAdmin.GcReport] =
    for
      _ <- FederationFinality.FederationFinalityCertificate.verify(certificate, activeManifest)
      _ <- Either.cond(certificate.federationId == federationId, (), "federation gc: certificate federation id mismatch")
      _ <- Either.cond(certificate.stateDigest == latestFinalized.digest, (),
        "federation gc: certificate does not finalize the candidate epoch's state")
      epochArtifact <- cas.getByDigest(latestFinalized.gcEpoch)
      epoch <- ReplicatedGcEpoch.fromArtifact(epochArtifact)
      currentClosure <- ArtifactApplicationResolver(cas).audit(latestFinalized.digest)
      report <- CasAdminEffects.gc(casRoot, epoch.roots ++ currentClosure, ctx).left.map(casErr)
    yield report
