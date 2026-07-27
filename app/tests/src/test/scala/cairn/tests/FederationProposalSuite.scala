package cairn.tests

import cairn.kernel.*
import cairn.systemhandler.FederationFinality

/** PR33 slice 1: `FederationProposal` — the network-agreement analogue of a
  * proposed block. Content-addressed round-trip only; no replica/HTTP
  * behavior yet.
  */
class FederationProposalSuite extends munit.FunSuite:
  private val proposal = FederationFinality.FederationProposal(
    federationId = Digest.of(Canon.CStr("federation-chain-id")),
    transition = Digest.of(Canon.CStr("transition-digest")),
    before = Digest.of(Canon.CStr("before-state")),
    after = Digest.of(Canon.CStr("after-state")),
    epoch = 3L,
    replicaSet = Digest.of(Canon.CStr("replica-set-digest")))

  test("artifact/fromArtifact round-trip"):
    val back = FederationFinality.FederationProposal.fromArtifact(proposal.artifact).fold(e => fail(e), identity)
    assertEquals(back, proposal)

  test("artifact kind is FederationProposal"):
    assertEquals(proposal.artifact.kind, ArtifactKind.FederationProposal)

  test("different fields produce different digests"):
    val other = proposal.copy(epoch = 4L)
    assert(proposal.digest != other.digest)
    val sameAgain = proposal.copy()
    assertEquals(proposal.digest, sameAgain.digest)

  test("fromArtifact rejects an artifact of the wrong kind"):
    val wrong = Artifact(ArtifactKind.FederationState, proposal.canon)
    assert(FederationFinality.FederationProposal.fromArtifact(wrong).isLeft)

  test("fromArtifact rejects a malformed body"):
    val malformed = Artifact(ArtifactKind.FederationProposal, Canon.CStr("not-a-proposal"))
    assert(FederationFinality.FederationProposal.fromArtifact(malformed).isLeft)

  test("valueOfProposal commits to the proposal's own digest, not just 'after'"):
    val v1 = FederationFinality.valueOfProposal(proposal)
    val v2 = FederationFinality.valueOfProposal(proposal)
    assertEquals(v1.digest, v2.digest)
    val sameAfterDifferentEpoch = proposal.copy(epoch = 99L)
    assert(FederationFinality.valueOfProposal(sameAfterDifferentEpoch).digest != v1.digest)
