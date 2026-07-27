package cairn.tests

import cairn.kernel.*
import cairn.systemhandler.{BftFinality, FederationFinality, Keypair}

/** PR31 slice 4: BFT finality over a federation-state digest. Mirrors
  * `DistributionDaemonSuite`'s existing `BftFinality.agreeForSealedBlock`
  * certificate tests, retargeted at `FederationFinality.agreeForFederationState`
  * — a parallel, digest-generic certificate path that never touches the
  * ledger/`Block`-coupled block-finality machinery.
  */
class FederationFinalitySuite extends munit.FunSuite:
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val stateDigest = Digest.of(Canon.CStr("federation-state-generation-1"))
  private val previousState = Digest.of(Canon.CStr("federation-state-genesis"))
  private val federationId = Digest.of(Canon.CStr("federation-chain-id"))

  test("agreeForFederationState mints a verifiable 2f+1 certificate"):
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = stateDigest, epoch = 1L,
      previousState = previousState, federationId = federationId).fold(e => fail(e), identity)
    assertEquals(cert.stateDigest, stateDigest)
    assertEquals(cert.epoch, 1L)
    assertEquals(cert.seq, 1)
    assertEquals(cert.previousState, previousState)
    assertEquals(cert.federationId, federationId)
    assert(cert.commits.size >= BftQuorum.quorumSize(4))
    assertEquals(cert.commits.map(_._1.id).distinct.length, cert.commits.length)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert, manifest), Right(()))

  test("canon/fromCanon round-trip"):
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = stateDigest, epoch = 2L,
      previousState = previousState, federationId = federationId).fold(e => fail(e), identity)
    val back = FederationFinality.FederationFinalityCertificate.fromCanon(cert.canon).fold(e => fail(e), identity)
    assertEquals(back, cert)

  test("certificate rejects duplicate replica commits"):
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = stateDigest, epoch = 1L,
      previousState = previousState, federationId = federationId).fold(e => fail(e), identity)
    val (id0, seal0) = cert.commits.head
    val duped = cert.copy(commits = List.fill(3)((id0, seal0)))
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assert(FederationFinality.FederationFinalityCertificate.verify(duped, manifest).isLeft)

  test("certificate rejects under-quorum commits"):
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = stateDigest, epoch = 1L,
      previousState = previousState, federationId = federationId).fold(e => fail(e), identity)
    val thin = cert.copy(commits = cert.commits.take(1))
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assert(FederationFinality.FederationFinalityCertificate.verify(thin, manifest).isLeft)

  test("certificate rejects a forged epoch (seq must equal epoch)"):
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = stateDigest, epoch = 1L,
      previousState = previousState, federationId = federationId).fold(e => fail(e), identity)
    val forged = cert.copy(epoch = 99L)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assert(FederationFinality.FederationFinalityCertificate.verify(forged, manifest).isLeft)

  test("certificate rejects a replica set mismatch"):
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = stateDigest, epoch = 1L,
      previousState = previousState, federationId = federationId).fold(e => fail(e), identity)
    val otherReplicas = List("s0", "s1", "s2", "s3").map(Keypair.dev)
    val otherManifest = BftFinality.sealReplicaSet(otherReplicas).fold(e => fail(e), identity)
    assert(FederationFinality.FederationFinalityCertificate.verify(cert, otherManifest).isLeft)

  test("verifyAgainstFederationHistory checks federationId/previousState/stateDigest bindings"):
    val cert = FederationFinality.agreeForFederationState(
      replicas, view = 0, stateDigest = stateDigest, epoch = 1L,
      previousState = previousState, federationId = federationId).fold(e => fail(e), identity)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
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
