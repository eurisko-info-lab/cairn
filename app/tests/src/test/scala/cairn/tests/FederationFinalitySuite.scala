package cairn.tests

import cairn.kernel.*
import cairn.systemhandler.{BftFinality, FederationFinality, Keypair}

/** PR31 slice 4: BFT finality over a federation-state digest. Mirrors
  * `DistributionDaemonSuite`'s existing `BftFinality.agreeForSealedBlock`
  * certificate tests, retargeted at `FederationFinality.agreeForFederationStateLocalTestOnly`
  * — a parallel, digest-generic certificate path that never touches the
  * ledger/`Block`-coupled block-finality machinery.
  */
class FederationFinalitySuite extends munit.FunSuite:
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val stateDigest = Digest.of(Canon.CStr("federation-state-generation-1"))
  private val previousState = Digest.of(Canon.CStr("federation-state-genesis"))
  private val federationId = Digest.of(Canon.CStr("federation-chain-id"))
  private val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
  private val transitionDigest = Digest.of(Canon.CStr("federation-transition-generation-1"))

  private def sampleProposal(epoch: Long): FederationFinality.FederationProposal =
    FederationFinality.FederationProposal(
      federationId, transitionDigest, previousState, stateDigest, epoch, manifest.replicaSetDigest)

  test("agreeForFederationStateLocalTestOnly mints a verifiable 2f+1 certificate"):
    val proposal = sampleProposal(1L)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, proposal)
      .fold(e => fail(e), identity)
    assertEquals(cert.proposal, proposal.digest)
    assertEquals(cert.transition, transitionDigest)
    assertEquals(cert.stateDigest, stateDigest)
    assertEquals(cert.epoch, 1L)
    assertEquals(cert.seq, 1)
    assertEquals(cert.previousState, previousState)
    assertEquals(cert.federationId, federationId)
    assert(cert.commits.size >= BftQuorum.quorumSize(4))
    assertEquals(cert.commits.map(_._1.id).distinct.length, cert.commits.length)
    assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert, manifest), Right(()))

  test("canon/fromCanon round-trip"):
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, sampleProposal(2L))
      .fold(e => fail(e), identity)
    val back = FederationFinality.FederationFinalityCertificate.fromCanon(cert.canon).fold(e => fail(e), identity)
    assertEquals(back, cert)

  test("certificate rejects duplicate replica commits"):
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, sampleProposal(1L))
      .fold(e => fail(e), identity)
    val (id0, seal0) = cert.commits.head
    val duped = cert.copy(commits = List.fill(3)((id0, seal0)))
    assert(FederationFinality.FederationFinalityCertificate.verify(duped, manifest).isLeft)

  test("certificate rejects under-quorum commits"):
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, sampleProposal(1L))
      .fold(e => fail(e), identity)
    val thin = cert.copy(commits = cert.commits.take(1))
    assert(FederationFinality.FederationFinalityCertificate.verify(thin, manifest).isLeft)

  test("certificate rejects a forged epoch (seq must equal epoch)"):
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, sampleProposal(1L))
      .fold(e => fail(e), identity)
    val forged = cert.copy(epoch = 99L)
    assert(FederationFinality.FederationFinalityCertificate.verify(forged, manifest).isLeft)

  test("certificate rejects a replica set mismatch"):
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, sampleProposal(1L))
      .fold(e => fail(e), identity)
    val otherReplicas = List("s0", "s1", "s2", "s3").map(Keypair.dev)
    val otherManifest = BftFinality.sealReplicaSet(otherReplicas).fold(e => fail(e), identity)
    assert(FederationFinality.FederationFinalityCertificate.verify(cert, otherManifest).isLeft)

  test("certificate rejects a forged proposal digest not matching what quorum actually signed"):
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, sampleProposal(1L))
      .fold(e => fail(e), identity)
    val forged = cert.copy(proposal = sampleProposal(2L).digest)
    assert(FederationFinality.FederationFinalityCertificate.verify(forged, manifest).isLeft)

  test("verifyAgainstFederationHistory checks federationId/previousState/stateDigest bindings"):
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, sampleProposal(1L))
      .fold(e => fail(e), identity)
    assertEquals(FederationFinality.FederationFinalityCertificate.verifyAgainstFederationHistory(
      cert, manifest, federationId, previousState, stateDigest), Right(()))
    val wrongFederation = Digest.of(Canon.CStr("some-other-federation"))
    assert(FederationFinality.FederationFinalityCertificate.verifyAgainstFederationHistory(
      cert, manifest, wrongFederation, previousState, stateDigest).isLeft)
    val wrongPrevious = Digest.of(Canon.CStr("wrong-previous-state"))
    assert(FederationFinality.FederationFinalityCertificate.verifyAgainstFederationHistory(
      cert, manifest, federationId, wrongPrevious, stateDigest).isLeft)
    val wrongClaimed = Digest.of(Canon.CStr("wrong-claimed-state"))
    assert(FederationFinality.FederationFinalityCertificate.verifyAgainstFederationHistory(
      cert, manifest, federationId, previousState, wrongClaimed).isLeft)

  test("valueOfState is a pure digest-generic encoding, not coupled to any ledger/block concept"):
    val v1 = FederationFinality.valueOfState(stateDigest)
    val v2 = FederationFinality.valueOfState(stateDigest)
    assertEquals(v1.digest, v2.digest)
    assert(v1.digest != FederationFinality.valueOfState(previousState).digest)

  test("valueOfProposal is a pure digest-generic encoding distinct from valueOfState"):
    val proposal = sampleProposal(1L)
    val v1 = FederationFinality.valueOfProposal(proposal.digest)
    val v2 = FederationFinality.valueOfProposal(proposal.digest)
    assertEquals(v1.digest, v2.digest)
    assert(v1.digest != FederationFinality.valueOfProposal(sampleProposal(2L).digest).digest)
    assert(v1.digest != FederationFinality.valueOfState(stateDigest).digest)

  test("two proposals sharing the same after/epoch but different transitions cannot share a certificate's votes"):
    // The exact PR33.1 ambiguity this closes: same `after`/`epoch`, but a
    // DIFFERENT `transition` — under the old valueOfState(after)-only
    // scheme these would have produced INDISTINGUISHABLE Commit signatures.
    val proposalA = FederationFinality.FederationProposal(
      federationId, Digest.of(Canon.CStr("transition-a")), previousState, stateDigest, 1L, manifest.replicaSetDigest)
    val proposalB = FederationFinality.FederationProposal(
      federationId, Digest.of(Canon.CStr("transition-b")), previousState, stateDigest, 1L, manifest.replicaSetDigest)
    assert(proposalA.digest != proposalB.digest)
    assertEquals(proposalA.after, proposalB.after)
    assertEquals(proposalA.epoch, proposalB.epoch)
    assert(FederationFinality.valueOfProposal(proposalA.digest).digest != FederationFinality.valueOfProposal(proposalB.digest).digest)
    val certA = FederationFinality.agreeForFederationStateLocalTestOnly(replicas, manifest, view = 0, proposalA)
      .fold(e => fail(e), identity)
    // A certificate genuinely minted over proposalA can never be reinterpreted
    // as one for proposalB: substituting the OTHER proposal's digest in
    // directly makes the certificate self-inconsistent (its own commits no
    // longer verify against the substituted value), and substituting
    // proposalB's `after`-adjacent fields while keeping proposalA's real
    // `proposal` digest is caught by `VerifiedFederationTransition`'s own
    // proposal-content cross-check (PR33.1) at history-replay/audit time.
    val mixed = certA.copy(proposal = proposalB.digest)
    assert(FederationFinality.FederationFinalityCertificate.verify(mixed, manifest).isLeft,
      "substituting a different proposal's digest must invalidate every existing commit seal")
