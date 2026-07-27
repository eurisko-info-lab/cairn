package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.systemhandler.{BftFinality, DiskCas, FederationFinality, Keypair}
import java.nio.file.Files

/** PR32 slice 1: [[VerifiedFederationTransition]]'s core structural checks —
  * before/after digest bindings, no dangling/duplicate transactions or
  * approvals, and the finality certificate binding this exact before/after
  * pair (sourced from `after.trustRoots`, not `before`). Per-namespace index
  * diffing, amendment policy, and GC-epoch checks land in later slices.
  */
class FederationTransitionCheckSuite extends munit.FunSuite:
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
  private val federationId = Digest.of(Canon.CStr("federation-transition-check-chain"))

  private def indices(tag: String): (RepositoryIndex, ApplicationIndex, NamespaceIndex) =
    (RepositoryIndex(Map("org-a" -> Digest.of(Canon.CStr(s"$tag-repo")))),
      ApplicationIndex(Map("org-a" -> Digest.of(Canon.CStr(s"$tag-app")))),
      NamespaceIndex(Map("org-a" -> Digest.of(Canon.CStr(s"$tag-ns")))))

  /** A single, non-rotating generation: `before`/`after` differ only in
    * their index digests (opaque stand-ins — slice 1 doesn't decode them
    * yet), with a real, verifiable finality certificate binding them.
    */
  private def fixture(): (DiskCas, FederationState, FederationState, FederationTransition, FederationFinality.FederationFinalityCertificate) =
    val dir = Files.createTempDirectory("cairn-fedtx-check")
    val cas = DiskCas(dir.resolve("cas"))
    val ledger = Digest.of(Canon.CStr("ledger-stand-in"))
    val gcEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val (repoBefore, appBefore, nsBefore) = indices("before")
    val (repoAfter, appAfter, nsAfter) = indices("after")
    val before = FederationState(ledger, repoBefore.digest, appBefore.digest, nsBefore.digest, replicaSet.digest, gcEpoch.digest)
    val after = FederationState(ledger, repoAfter.digest, appAfter.digest, nsAfter.digest, replicaSet.digest, gcEpoch.digest)
    List(replicaSet.artifact, gcEpoch.artifact, repoBefore.artifact, appBefore.artifact, nsBefore.artifact,
      repoAfter.artifact, appAfter.artifact, nsAfter.artifact, before.artifact, after.artifact).foreach(cas.put)
    val cert = FederationFinality.agreeForFederationState(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, Nil, Some(cert.digest))
    (cas, before, after, transition, cert)

  test("verify accepts a well-formed transition with a matching finality certificate"):
    val (cas, before, after, transition, cert) = fixture()
    val verified = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
      .fold(e => fail(e), identity)
    assertEquals(verified.transition, transition)
    assertEquals(verified.before, before)
    assertEquals(verified.after, after)

  test("verify rejects a transition whose 'before' does not match the supplied before-state"):
    val (cas, before, after, transition, cert) = fixture()
    val wrong = transition.copy(before = Digest.of(Canon.CStr("wrong-before")))
    val result = VerifiedFederationTransition.verify(wrong, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("transition.before")), result.toString)

  test("verify rejects a transition whose 'after' does not match the supplied after-state"):
    val (cas, before, after, transition, cert) = fixture()
    val wrong = transition.copy(after = Digest.of(Canon.CStr("wrong-after")))
    val result = VerifiedFederationTransition.verify(wrong, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("transition.after")), result.toString)

  test("verify rejects duplicate transaction digests"):
    val (cas, before, after, transition, cert) = fixture()
    val dupeDigest = Digest.of(Canon.CStr("dupe-commit"))
    val wrong = transition.copy(transactions = List(dupeDigest, dupeDigest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("duplicate transactions")), result.toString)

  test("verify rejects duplicate approval digests"):
    val (cas, before, after, transition, cert) = fixture()
    val dupeDigest = Digest.of(Canon.CStr("dupe-approval"))
    val wrong = transition.copy(approvals = List(dupeDigest, dupeDigest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("duplicate approvals")), result.toString)

  test("verify rejects a transition whose finality field does not cite the supplied certificate"):
    val (cas, before, after, transition, cert) = fixture()
    val wrong = transition.copy(finality = Some(Digest.of(Canon.CStr("some-other-certificate"))))
    val result = VerifiedFederationTransition.verify(wrong, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("transition.finality")), result.toString)

  test("verify rejects a certificate that does not bind this before/after pair"):
    val (cas, before, after, transition, cert) = fixture()
    val forged = cert.copy(previousState = Digest.of(Canon.CStr("some-other-prior-state")))
    val wrong = transition.copy(finality = Some(forged.digest))
    cas.put(forged.artifact)
    val result = VerifiedFederationTransition.verify(wrong, before, after, Nil, forged, federationId, cas)
    assert(result.isLeft, result.toString)
