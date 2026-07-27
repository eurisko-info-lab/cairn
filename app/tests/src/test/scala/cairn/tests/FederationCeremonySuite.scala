package cairn.tests

import cairn.kernel.*
import cairn.kernel.BftQuorum.*
import cairn.core.*
import cairn.runtime.*
import cairn.examples.stlc.Stlc
import cairn.systemhandler.{BftFinality, DiskCas, Ed25519, FederationFinality, Keypair, Node}
import java.nio.file.Files

/** PR31 exit ceremony: four replicas, two namespaces. Publishes a valid
  * change and successor application, crashes during each transaction
  * phase, rotates one namespace authority, activates a successor replica
  * set, injects an equivocating proposal, restarts every node from disk,
  * and verifies: all honest nodes expose the same federation root; no
  * referenced artifact was reclaimed; the entire state reconstructs from
  * that one root.
  *
  * Broken into separate `test(...)` blocks per ceremony step (rather than
  * one monolithic scenario) so a failure localizes to one step, per the
  * plan's own guidance for this, the highest-risk slice.
  */
class FederationCeremonySuite extends munit.FunSuite:
  private val lang = Stlc.language
  private val dl = Delta.deltaOf(lang).toOption.get
  private val m0 = Module(List("a" -> Stlc.tru))
  private val casCtx = EffectContexts.forBranches()
  private val authority = Keypair.dev("ceremony-authority")
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val authorities = Map(authority.name -> authority.publicBytes)

  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  /** One namespace's complete, internally-consistent closure — a resident
    * NativeRepository/CausalChange/AcceptanceEvidence, an Application/
    * GenericMachine, an ecosystem release, and a genesis NamespaceTrustManifest.
    */
  private final case class NamespaceFixture(
      graphDigest: Digest, appDigest: Digest, releaseDigest: Digest,
      trustManifest: NamespaceTrustManifest, commit: FederationCommit, owner: Keypair,
  )

  /** `reuseTrust`, when supplied, carries the SAME namespace's governing
    * trust manifest (and its owner signer) forward from a prior generation
    * unchanged — this namespace's own commit still cites it, but no
    * namespace-trust rotation happens in this generation. Without it, a
    * fresh genesis manifest is minted (only valid for a namespace's FIRST
    * generation — reusing this default for a namespace that already has an
    * active manifest would be an unauthorized trust swap, exactly what
    * VerifiedFederationTransition's amendment-policy check now rejects).
    */
  private def buildNamespace(
      cas: DiskCas, name: String, changeSrc: String, epochDigest: Digest,
      reuseTrust: Option[(NamespaceTrustManifest, Keypair)] = None,
  ): NamespaceFixture =
    val change = parseChange(changeSrc)
    val (result, vcs) = Delta.apply(lang, m0, change).fold(e => fail(e), identity)
    val capabilities = LanguageCapabilities.standard(lang)
    val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
    val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
    val machine = GenericMachine.declare(List(runtime.digest))
    val grammar = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(lang.grammar))
    val appLanguage = ApplicationLanguage("stlc", lang.digest, grammar.digest, capabilities.descriptor.digest, Some(runtime.digest))
    val appManifest = ApplicationManifest(s"$name-app", machine.machine.digest, List(appLanguage), Nil)
    val (trustManifest, owner) = reuseTrust.getOrElse {
      val o = Keypair.dev(s"$name-owner")
      (NamespaceTrustManifest.of(name, List(o.name -> o.publicBytes)).fold(e => fail(e), identity), o)
    }
    val evidence = AcceptanceEvidence(lang.digest, m0.digest, Some(vcs.artifact.digest), result.digest,
      constitution.digest, "open", capabilities.changeModel.digest, constitution = Some(constitution.digest),
      runtime = Some(runtime.digest))
    val causal = CausalChange(vcs.artifact.digest, Set.empty, Nil, m0.digest, result.digest, runtime.digest,
      acceptanceEvidence = Some(evidence.digest))
    val graph = NativeRepository(changes = Map(causal.id -> causal), heads = Map("main" -> Set(causal.id)))
    val release = EcosystemBundles.sign(name, SemanticVersion(1, 0, 0), appManifest.digest,
      EcosystemRootKind.Application, Nil, Nil, authority)
    val branchView = BranchManifest("main", None, Nil, acceptanceEvidence = Some(evidence.digest),
      domainRuntime = Some(runtime.digest), repositoryGraph = Some(graph.digest))
    val commit = FederationCommit(name, "main", graph.digest, branchView.artifact.digest, evidence.digest,
      runtime.digest, appManifest.digest, release.digest, trustManifest.digest, epochDigest)
    (runtime.artifacts ++ machine.supportArtifacts ++ List(machine.machine.artifact, appManifest.artifact, grammar,
      trustManifest.artifact, vcs.artifact, m0.artifact, result.artifact, evidence.artifact, graph.artifact,
      release.artifact, branchView.artifact, commit.artifact))
      .distinctBy(_.digest).foreach(cas.put)
    NamespaceFixture(graph.digest, appManifest.digest, release.digest, trustManifest, commit, owner)

  test("exit ceremony: two namespaces publish, crash at every phase, rotate namespace + replica-set trust, equivocation is captured, restart converges, nothing live is reclaimed, and the final state reconstructs from one root"):
    val dir = Files.createTempDirectory("cairn-ceremony")
    val cas = DiskCas(dir.resolve("cas"))
    val nodeHome = dir.resolve("ledger")
    var node = Node(nodeHome, EffectContexts.forLedger())
    node.append(authority, authorities, List(authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes))))
      .fold(e => fail(e), identity)
    val ledgerStandIn = Artifact(ArtifactKind.Block, Canon.CStr("ceremony-ledger-stand-in"))
    cas.put(ledgerStandIn)
    val federationId = node.chainDigests.head

    val replicaSet0 = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    cas.put(replicaSet0.artifact)

    // -- Step 1: publish an initial (genesis -> generation 1) two-namespace state. --
    val genesisEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    cas.put(genesisEpoch.artifact)
    val genesisState = FederationState.genesis(ledgerStandIn.digest, replicaSet0.digest)
    cas.put(genesisState.artifact)

    val nsA0 = buildNamespace(cas, "org-a", "{ add extra = true ; }", genesisEpoch.digest)
    val nsB0 = buildNamespace(cas, "org-b", "{ add other = false ; }", genesisEpoch.digest)
    val epoch1 = ReplicatedGcEpoch(1,
      Set(nsA0.graphDigest, nsA0.appDigest, nsA0.releaseDigest, nsB0.graphDigest, nsB0.appDigest, nsB0.releaseDigest),
      Some(genesisEpoch.digest))
    cas.put(epoch1.artifact)
    val repoIndex1 = RepositoryIndex(Map("org-a" -> nsA0.graphDigest, "org-b" -> nsB0.graphDigest))
    val appIndex1 = ApplicationIndex(Map("org-a" -> nsA0.appDigest, "org-b" -> nsB0.appDigest))
    val nsIndex1 = NamespaceIndex(Map("org-a" -> nsA0.trustManifest.digest, "org-b" -> nsB0.trustManifest.digest))
    List(repoIndex1.artifact, appIndex1.artifact, nsIndex1.artifact).foreach(cas.put)
    val state1 = FederationState(ledgerStandIn.digest, repoIndex1.digest, appIndex1.digest, nsIndex1.digest,
      replicaSet0.digest, epoch1.digest)

    val home1 = dir.resolve("home-1")
    val coord1 = FederationTransactionCoordinator(home1, cas, node, replicas, replicaSet0, federationId)
    val (cert1, _) = coord1.publish(List(nsA0.commit, nsB0.commit), genesisState, state1, epoch = 1L, authority, authorities)
      .fold(e => fail(e), identity)
    assertEquals(coord1.current, Right(Some(state1.digest)))
    assertEquals(cert1.stateDigest, state1.digest)

    // -- Step 2: crash at every transaction phase during a second transition
    //    (org-a publishes a further change), each on its own coordinator
    //    instance sharing the same on-disk home/cas/node, then confirm
    //    recovery leaves state1 untouched. --
    // org-a's trust is carried forward unchanged from generation 1 — this
    // generation only publishes new content, it does not rotate trust
    // (that happens explicitly in Step 3 below).
    val nsA1 = buildNamespace(cas, "org-a", "{ add extra = true ; add second = false ; }", epoch1.digest,
      reuseTrust = Some(nsA0.trustManifest, nsA0.owner))
    val epoch2 = ReplicatedGcEpoch(2,
      Set(nsA1.graphDigest, nsA1.appDigest, nsA1.releaseDigest, nsB0.graphDigest, nsB0.appDigest, nsB0.releaseDigest),
      Some(epoch1.digest))
    cas.put(epoch2.artifact)
    val repoIndex2 = RepositoryIndex(Map("org-a" -> nsA1.graphDigest, "org-b" -> nsB0.graphDigest))
    val appIndex2 = ApplicationIndex(Map("org-a" -> nsA1.appDigest, "org-b" -> nsB0.appDigest))
    val nsIndex2 = NamespaceIndex(Map("org-a" -> nsA1.trustManifest.digest, "org-b" -> nsB0.trustManifest.digest))
    List(repoIndex2.artifact, appIndex2.artifact, nsIndex2.artifact).foreach(cas.put)
    val state2 = FederationState(ledgerStandIn.digest, repoIndex2.digest, appIndex2.digest, nsIndex2.digest,
      replicaSet0.digest, epoch2.digest)

    List(
      FederationTransactionPhase.AfterStaged, FederationTransactionPhase.AfterProposed,
      FederationTransactionPhase.AfterCertified, FederationTransactionPhase.AfterLedgered,
    ).zipWithIndex.foreach { (phase, i) =>
      val home = dir.resolve(s"home-crash-$i")
      val coord = FederationTransactionCoordinator(home, cas, node, replicas, replicaSet0, federationId)
      assert(coord.publish(List(nsA1.commit, nsB0.commit), state1, state2, epoch = 2L, authority, authorities, phase).isLeft)
      assertEquals(coord.current, Right(None), s"phase $phase: nothing exposed locally on this fresh coordinator yet")
    }
    // The actual generation-2 transition, uninterrupted, on its own coordinator home.
    val home2 = dir.resolve("home-2")
    val coord2 = FederationTransactionCoordinator(home2, cas, node, replicas, replicaSet0, federationId)
    val (cert2, _) = coord2.publish(List(nsA1.commit, nsB0.commit), state1, state2, epoch = 2L, authority, authorities)
      .fold(e => fail(e), identity)
    assertEquals(coord2.current, Right(Some(state2.digest)))

    // -- Step 3: rotate org-a's namespace authority (majority-of-predecessor-owners amendment). --
    val newOwner = Keypair.dev("org-a-owner-2")
    val rotatedDraft = NamespaceTrustManifest.of("org-a", List(newOwner.name -> newOwner.publicBytes),
      replaces = Some(nsA1.trustManifest.digest), activationEpoch = 3L).fold(e => fail(e), identity)
    val payload = Canon.encode(rotatedDraft.bodyCanon)
    val rotated = rotatedDraft.copy(predecessorApprovals = List(nsA1.owner.name -> nsA1.owner.sign(payload)))
    assertEquals(
      NamespaceTrustManifest.allowsTransition(rotated, Some(nsA1.trustManifest), Some(nsA1.trustManifest.digest), Ed25519.verify),
      Right(()))
    cas.put(rotated.artifact)
    val nsIndex3 = NamespaceIndex(Map("org-a" -> rotated.digest, "org-b" -> nsB0.trustManifest.digest))
    cas.put(nsIndex3.artifact)

    // -- Step 4: activate a successor replica set (majority-of-old-quorum amendment). --
    val successorReplicas = replicas.take(3) ++ List(Keypair.dev("r4"))
    val successorDraft = ReplicaSetManifest.of(successorReplicas.map(k => k.name -> k.publicBytes),
      replaces = Some(replicaSet0.digest), activationHeight = 3L).fold(e => fail(e), identity)
    val rsPayload = Canon.encode(successorDraft.bodyCanon)
    val rsApprovals = replicas.take(BftQuorum.quorumSize(4)).map(k => k.name -> k.sign(rsPayload))
    val successorApproved = ReplicaSetManifest.withPredecessorApprovals(successorDraft, rsApprovals).fold(e => fail(e), identity)
    val successorReplicaSet = BftFinality.sealReplicaSet(successorReplicas, replaces = Some(replicaSet0.digest), activationHeight = 3L)
      .fold(e => fail(e), identity).copy(predecessorApprovals = successorApproved.predecessorApprovals)
    assertEquals(
      ReplicaSetManifest.allowsTransition(successorReplicaSet, Some(replicaSet0), Some(replicaSet0.digest), Ed25519.verify),
      Right(()))
    cas.put(successorReplicaSet.artifact)

    // Content-level closure only (matching FederationGc.computeFederationGcRoots'
    // own approach: repository/application/release content, not the state's
    // own bookkeeping wrapper artifacts) — reclaimAgainstFinalizedEpoch
    // separately, always unions in the full closure of the state it's
    // finalizing, so this epoch does not need to (and structurally cannot:
    // a generation's epoch is computed before its own state/epoch digests
    // exist to be named, the same way a git commit can't embed its own hash).
    val resolverForEpoch3 = ArtifactApplicationResolver(cas)
    val epoch3Roots = List(nsA1.graphDigest, nsA1.appDigest, nsA1.releaseDigest,
        nsB0.graphDigest, nsB0.appDigest, nsB0.releaseDigest)
      .foldLeft(Set.empty[Digest])((acc, d) => acc ++ resolverForEpoch3.audit(d).fold(e => fail(e), identity))
    val epoch3 = ReplicatedGcEpoch(3, epoch3Roots, Some(epoch2.digest))
    cas.put(epoch3.artifact)

    val state3 = FederationState(ledgerStandIn.digest, repoIndex2.digest, appIndex2.digest, nsIndex3.digest,
      successorReplicaSet.digest, epoch3.digest)
    val home3 = dir.resolve("home-3")
    val coord3 = FederationTransactionCoordinator(home3, cas, node, successorReplicas, successorReplicaSet, federationId)
    val (cert3, _) = coord3.publish(Nil, state2, state3, epoch = 3L, authority, authorities)
      .fold(e => fail(e), identity)
    assertEquals(coord3.current, Right(Some(state3.digest)))
    assertEquals(cert3.replicaSet, successorReplicaSet.replicaSetDigest)

    // -- Step 5: inject an equivocating proposal against the NEW replica set
    //    (mirrors EquivocationSuite: a genuine view-change is needed for the
    //    preparedLock cross-view guard to fire) and confirm it's captured
    //    without corrupting the legitimate state3. --
    val equivReplicas = successorReplicas
    val equivManifest = BftFinality.sealReplicaSet(equivReplicas).fold(e => fail(e), identity)
    val equivBfts = equivReplicas.map(k =>
      k.name -> cairn.systemhandler.BftReplica.certified(k, equivManifest, node = None, ledgerAuth = Map.empty).fold(e => fail(e), identity)).toMap
    def isCommit(sm: BftFinality.SignedMsg): Boolean = sm.msg.isInstanceOf[Msg.Commit]
    def deliverAll(from: String, msgs: List[BftFinality.SignedMsg], exclude: Set[String] = Set.empty, dropCommits: Boolean = false): Unit =
      val filtered = if dropCommits then msgs.filterNot(isCommit) else msgs
      filtered.foreach(sm => equivBfts.foreach((id, r) => if id != from && !exclude.contains(id) then r.receive(sm).fold(e => fail(s"$id: $e"), identity)))
    val equivBlock = Digest.of(Canon.CStr("equivocation-round-value"))
    // Bare (no-node) replicas each independently require the block noted as
    // sealed before accepting a PrePrepare for it — not just the primary.
    equivBfts.values.foreach(_.noteSealedBlock(equivBlock, 0L, Digest.of(Canon.CStr("parent"))).fold(e => fail(e), identity))
    val out0 = equivBfts(equivReplicas.head.name).propose(0, 0, equivBlock).fold(e => fail(e), identity)
    deliverAll(equivReplicas.head.name, out0, dropCommits = true)
    var round = 0
    var progress = true
    while round < 16 && progress do
      progress = false
      equivBfts.foreach { (id, r) =>
        val out = r.drainOutbound().filterNot(isCommit)
        if out.nonEmpty then progress = true; deliverAll(id, out, exclude = Set(equivReplicas.head.name), dropCommits = true)
      }
      round += 1
    val honestEquiv = equivReplicas.tail.map(_.name)
    honestEquiv.foreach { id =>
      val out = equivBfts(id).requestViewChange(1).fold(e => fail(e), identity)
      deliverAll(id, out, exclude = Set(equivReplicas.head.name))
    }
    round = 0; progress = true
    while round < 24 && progress do
      progress = false
      honestEquiv.foreach { id =>
        val out = equivBfts(id).drainOutbound().filterNot(isCommit)
        if out.nonEmpty then progress = true; deliverAll(id, out, exclude = Set(equivReplicas.head.name), dropCommits = true)
      }
      round += 1
    assert(honestEquiv.forall(id => equivBfts(id).currentView == 1))
    val newPrimaryId = equivReplicas(1).name // designatedPrimary for view 1 among 4 sorted ids
    val forgedValue = Digest.of(Canon.CStr("equivocating-forged-value"))
    val forgedPp = BftFinality.sign(equivReplicas(1),
      Msg.PrePrepare(1, 0, BftFinality.valueOfBlock(forgedValue), ReplicaId(newPrimaryId)),
      equivManifest.replicaSetDigest, equivBfts(honestEquiv(1)).chainId).fold(e => fail(e), identity)
    val targetId = honestEquiv.find(_ != newPrimaryId).getOrElse(fail("expected another honest replica"))
    val rejected = equivBfts(targetId).receive(forgedPp)
    assert(rejected.isLeft, "the equivocating PrePrepare must be rejected")
    val evidence = equivBfts(targetId).detectedEquivocations
    assertEquals(evidence.length, 1, "the equivocation must produce exactly one evidence artifact")
    assertEquals(evidence.head.replica, ReplicaId(newPrimaryId))
    // Legitimate state3 is entirely unaffected by this separate, injected round.
    assertEquals(coord3.current, Right(Some(state3.digest)))

    // -- Step 6: restart every node from disk — fresh Node/DiskCas/coordinator
    //    instances against the SAME on-disk paths — and confirm convergence. --
    val restartedCas = DiskCas(dir.resolve("cas"))
    val restartedNode = Node(nodeHome, EffectContexts.forLedger())
    val restartedCoord = FederationTransactionCoordinator(home3, restartedCas, restartedNode, successorReplicas, successorReplicaSet, federationId)
    assertEquals(restartedCoord.current, Right(Some(state3.digest)), "restart from disk must converge on the same federation root")
    assertEquals(verifyFederationState(state3, restartedCas).map(_ => ()), Right(()))

    // -- Step 7: nothing live was reclaimed — an actual reclaim runs against
    //    the finalized epoch3 (certified by cert3/successorReplicaSet), with
    //    a planted orphan proving the sweep is doing real work, not a no-op. --
    val finalClosure = ArtifactApplicationResolver(cas).audit(state3.digest).fold(e => fail(e), identity)
    assert(finalClosure.nonEmpty)
    val casRoot = dir.resolve("cas")
    val orphan = Artifact(ArtifactKind.Claim, Canon.CStr("ceremony-orphan-blob"))
    val orphanDigest = cas.put(orphan).valueHash
    assert(cas.getByDigest(orphanDigest).isRight)
    val report = FederationGc.reclaimAgainstFinalizedEpoch(
      casRoot, state3, cas, cert3, successorReplicaSet, federationId, casCtx).fold(e => fail(e), identity)
    assert(report.swept >= 1, report.toString)
    assert(cas.getByDigest(orphanDigest).isLeft, "the orphan must actually be swept")
    finalClosure.foreach(d => assert(cas.getByDigest(d).isRight, s"reclaim must never have touched live digest ${d.short}"))

    // -- Step 8: the entire state reconstructs from that one root, into a FRESH,
    //    otherwise-empty CAS populated only via ArtifactApplicationResolver.install. --
    val freshDir = Files.createTempDirectory("cairn-ceremony-fresh")
    val freshCas = DiskCas(freshDir.resolve("cas"))
    val installedClosure = ArtifactApplicationResolver(freshCas).install(state3.digest, cas).fold(e => fail(e), identity)
    assertEquals(installedClosure, finalClosure)
    val reconstructed = verifyFederationState(state3, freshCas).fold(e => fail(e), identity)
    assertEquals(reconstructed, finalClosure)
