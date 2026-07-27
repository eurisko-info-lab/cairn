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

  test("fromCanon round-trips the bare canon body (no artifact envelope)"):
    val back = FederationFinality.FederationProposal.fromCanon(proposal.canon).fold(e => fail(e), identity)
    assertEquals(back, proposal)

  test("fromCanon rejects a canon value that isn't a federation-proposal-v1 body"):
    assert(FederationFinality.FederationProposal.fromCanon(Canon.CStr("not-a-proposal")).isLeft)
