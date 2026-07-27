package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.*
import cairn.systemhandler.{BftFinality, DiskCas, FederationFinality, Keypair}
import java.nio.file.Files

/** PR32 slices 1-3: [[VerifiedFederationTransition]]'s core structural
  * checks (before/after digest bindings, no dangling/duplicate transactions
  * or approvals, finality certificate binding sourced from `after.trustRoots`)
  * plus per-namespace `RepositoryIndex`/`ApplicationIndex` diffing (every
  * changed entry traces to exactly one authorizing commit, every untouched
  * entry is byte-identical, and each commit cites its own governing trust
  * manifest), plus namespace-trust/replica-set amendment-policy checks for
  * pure rotation transitions (no backing commit). GC-epoch
  * checks land in later slices.
  */
class FederationTransitionCheckSuite extends munit.FunSuite:
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val replicaSet = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
  private val federationId = Digest.of(Canon.CStr("federation-transition-check-chain"))
  private val owner = Keypair.dev("org-a-owner")
  private val trustManifest = NamespaceTrustManifest.of("org-a", List(owner.name -> owner.publicBytes))
    .fold(e => fail(e), identity)

  private def standIn(tag: String): Digest = Digest.of(Canon.CStr(tag))

  /** One namespace ("org-a"), one commit changing its repository/application
    * entries, an unrotated namespace-trust manifest, a real verifiable
    * finality certificate binding `before`/`after`, and a transition
    * artifact referencing exactly that one commit.
    */
  private def fixture(): (DiskCas, FederationState, FederationState, FederationCommit,
      FederationTransition, FederationFinality.FederationFinalityCertificate) =
    val dir = Files.createTempDirectory("cairn-fedtx-check")
    val cas = DiskCas(dir.resolve("cas"))
    val ledger = standIn("ledger-stand-in")
    val gcEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val repoBefore = RepositoryIndex(Map("org-a" -> standIn("repo-before")))
    val appBefore = ApplicationIndex(Map("org-a" -> standIn("app-before")))
    val nsIndex = NamespaceIndex(Map("org-a" -> trustManifest.digest))
    val commit = FederationCommit("org-a", "main", standIn("repo-after"), standIn("branch-view"),
      standIn("acceptance-evidence"), standIn("runtime"), standIn("app-after"), standIn("ecosystem-release"),
      trustManifest.digest, gcEpoch.digest)
    val repoAfter = RepositoryIndex(Map("org-a" -> commit.repositoryGraph))
    val appAfter = ApplicationIndex(Map("org-a" -> commit.application))
    val before = FederationState(ledger, repoBefore.digest, appBefore.digest, nsIndex.digest, replicaSet.digest, gcEpoch.digest)
    val after = FederationState(ledger, repoAfter.digest, appAfter.digest, nsIndex.digest, replicaSet.digest, gcEpoch.digest)
    List(replicaSet.artifact, gcEpoch.artifact, trustManifest.artifact, commit.artifact,
      repoBefore.artifact, appBefore.artifact, repoAfter.artifact, appAfter.artifact, nsIndex.artifact,
      before.artifact, after.artifact).foreach(cas.put)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, List(commit.digest), after.digest, Nil, Some(cert.digest))
    (cas, before, after, commit, transition, cert)

  test("verify accepts a well-formed transition whose commit matches the resulting indices"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val verified = VerifiedFederationTransition.verify(transition, before, after, List(commit), cert, federationId, cas)
      .fold(e => fail(e), identity)
    assertEquals(verified.commits, List(commit))

  test("verify rejects a transition whose 'before' does not match the supplied before-state"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val wrong = transition.copy(before = Digest.of(Canon.CStr("wrong-before")))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(commit), cert, federationId, cas)
    assert(result.left.exists(_.contains("transition.before")), result.toString)

  test("verify rejects a transition whose 'after' does not match the supplied after-state"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val wrong = transition.copy(after = Digest.of(Canon.CStr("wrong-after")))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(commit), cert, federationId, cas)
    assert(result.left.exists(_.contains("transition.after")), result.toString)

  test("verify rejects duplicate transaction digests"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val wrong = transition.copy(transactions = List(commit.digest, commit.digest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(commit), cert, federationId, cas)
    assert(result.left.exists(_.contains("duplicate transactions")), result.toString)

  test("verify rejects duplicate approval digests"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val dupeDigest = Digest.of(Canon.CStr("dupe-approval"))
    val wrong = transition.copy(approvals = List(dupeDigest, dupeDigest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(commit), cert, federationId, cas)
    assert(result.left.exists(_.contains("duplicate approvals")), result.toString)

  test("verify rejects a transition whose finality field does not cite the supplied certificate"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val wrong = transition.copy(finality = Some(Digest.of(Canon.CStr("some-other-certificate"))))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(commit), cert, federationId, cas)
    assert(result.left.exists(_.contains("transition.finality")), result.toString)

  test("verify rejects a certificate that does not bind this before/after pair"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val forged = cert.copy(previousState = Digest.of(Canon.CStr("some-other-prior-state")))
    cas.put(forged.artifact)
    val wrong = transition.copy(finality = Some(forged.digest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(commit), forged, federationId, cas)
    assert(result.isLeft, result.toString)

  test("verify rejects a repository-index change with no backing commit"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val wrong = transition.copy(transactions = Nil)
    val result = VerifiedFederationTransition.verify(wrong, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("changed without an authorizing commit")), result.toString)

  test("verify rejects a commit whose repositoryGraph does not match the resulting index entry"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val mismatched = commit.copy(repositoryGraph = Digest.of(Canon.CStr("some-other-repo-graph")))
    cas.put(mismatched.artifact)
    val wrong = transition.copy(transactions = List(mismatched.digest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(mismatched), cert, federationId, cas)
    assert(result.left.exists(_.contains("does not match its authorizing commit")), result.toString)

  test("verify rejects a commit that does not cite its own governing trust manifest"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val wrongTrust = commit.copy(namespaceTrust = Digest.of(Canon.CStr("some-other-trust-manifest")))
    cas.put(wrongTrust.artifact)
    val wrong = transition.copy(transactions = List(wrongTrust.digest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(wrongTrust), cert, federationId, cas)
    assert(result.left.exists(_.contains("does not cite its own governing trust manifest")), result.toString)

  test("verify rejects two commits targeting the same namespace in one transition"):
    val (cas, before, after, commit, transition, cert) = fixture()
    val dupe = commit.copy(previous = Some(commit.digest))
    cas.put(dupe.artifact)
    val wrong = transition.copy(transactions = List(commit.digest, dupe.digest))
    val result = VerifiedFederationTransition.verify(wrong, before, after, List(commit, dupe), cert, federationId, cas)
    assert(result.left.exists(_.contains("target the same namespace")), result.toString)

  test("verify accepts an untouched namespace whose repository/application entries are unchanged and uncommitted"):
    val dir = Files.createTempDirectory("cairn-fedtx-check-untouched")
    val cas = DiskCas(dir.resolve("cas"))
    val ledger = standIn("ledger-stand-in-2")
    val gcEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val repoIndex = RepositoryIndex(Map("org-b" -> standIn("org-b-repo")))
    val appIndex = ApplicationIndex(Map("org-b" -> standIn("org-b-app")))
    val nsIndex = NamespaceIndex(Map("org-b" -> trustManifest.digest))
    val before = FederationState(ledger, repoIndex.digest, appIndex.digest, nsIndex.digest, replicaSet.digest, gcEpoch.digest)
    val after = before
    List(replicaSet.artifact, gcEpoch.artifact, trustManifest.artifact, repoIndex.artifact, appIndex.artifact,
      nsIndex.artifact, before.artifact).foreach(cas.put)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, Nil, Some(cert.digest))
    val verified = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
      .fold(e => fail(e), identity)
    assertEquals(verified.after, after)

  /** A pure rotation transition: repository/application indices untouched,
    * no commits, only `nsDigest`/`replicaSet` differ between before/after.
    */
  private def rotationFixture(nsDigestBefore: Digest, nsDigestAfter: Digest,
      replicaSetBefore: ReplicaSetManifest, replicaSetAfter: ReplicaSetManifest,
  ): (DiskCas, FederationState, FederationState) =
    val dir = Files.createTempDirectory("cairn-fedtx-check-rotation")
    val cas = DiskCas(dir.resolve("cas"))
    val ledger = standIn("ledger-stand-in-rotation")
    val gcEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val repoIndex = RepositoryIndex(Map("org-a" -> standIn("rotation-repo")))
    val appIndex = ApplicationIndex(Map("org-a" -> standIn("rotation-app")))
    val nsIndexBefore = NamespaceIndex(Map("org-a" -> nsDigestBefore))
    val nsIndexAfter = NamespaceIndex(Map("org-a" -> nsDigestAfter))
    val before = FederationState(ledger, repoIndex.digest, appIndex.digest, nsIndexBefore.digest,
      replicaSetBefore.digest, gcEpoch.digest)
    val after = FederationState(ledger, repoIndex.digest, appIndex.digest, nsIndexAfter.digest,
      replicaSetAfter.digest, gcEpoch.digest)
    List(gcEpoch.artifact, repoIndex.artifact, appIndex.artifact, nsIndexBefore.artifact, nsIndexAfter.artifact,
      replicaSetBefore.artifact, replicaSetAfter.artifact, before.artifact, after.artifact).foreach(cas.put)
    (cas, before, after)

  test("verify accepts a namespace-trust rotation with predecessor majority and a listed approval"):
    val newOwner = Keypair.dev("org-a-owner-2")
    val draft = NamespaceTrustManifest.of("org-a", List(newOwner.name -> newOwner.publicBytes),
      replaces = Some(trustManifest.digest), activationEpoch = 1L).fold(e => fail(e), identity)
    val payload = Canon.encode(draft.bodyCanon)
    val rotated = draft.copy(predecessorApprovals = List(owner.name -> owner.sign(payload)))
    val (cas, before, after) = rotationFixture(trustManifest.digest, rotated.digest, replicaSet, replicaSet)
    cas.put(trustManifest.artifact); cas.put(rotated.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, List(rotated.digest), Some(cert.digest))
    val verified = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
      .fold(e => fail(e), identity)
    assertEquals(verified.after, after)

  test("verify rejects an otherwise-correct rotation whose approvals also list an unrelated extra digest"):
    val newOwner = Keypair.dev("org-a-owner-2b")
    val draft = NamespaceTrustManifest.of("org-a", List(newOwner.name -> newOwner.publicBytes),
      replaces = Some(trustManifest.digest), activationEpoch = 1L).fold(e => fail(e), identity)
    val payload = Canon.encode(draft.bodyCanon)
    val rotated = draft.copy(predecessorApprovals = List(owner.name -> owner.sign(payload)))
    val (cas, before, after) = rotationFixture(trustManifest.digest, rotated.digest, replicaSet, replicaSet)
    cas.put(trustManifest.artifact); cas.put(rotated.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    // The rotation itself is correctly approved and listed — but an extra,
    // unrelated digest (not backing any actual rotation) is also present.
    // A "contains" check would wrongly accept this; approvals must equal
    // the expected set exactly.
    val extraneous = Digest.of(Canon.CStr("unrelated-extra-approval"))
    val transition = FederationTransition(before.digest, Nil, after.digest, List(rotated.digest, extraneous), Some(cert.digest))
    val result = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("approvals does not equal exactly")), result.toString)

  test("verify rejects a namespace-trust rotation missing predecessor majority approval"):
    val newOwner = Keypair.dev("org-a-owner-3")
    val rotated = NamespaceTrustManifest.of("org-a", List(newOwner.name -> newOwner.publicBytes),
      replaces = Some(trustManifest.digest), activationEpoch = 1L).fold(e => fail(e), identity)
    val (cas, before, after) = rotationFixture(trustManifest.digest, rotated.digest, replicaSet, replicaSet)
    cas.put(trustManifest.artifact); cas.put(rotated.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, List(rotated.digest), Some(cert.digest))
    val result = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
    assert(result.isLeft, result.toString)

  test("verify rejects a properly-approved namespace-trust rotation absent from transition.approvals"):
    val newOwner = Keypair.dev("org-a-owner-4")
    val draft = NamespaceTrustManifest.of("org-a", List(newOwner.name -> newOwner.publicBytes),
      replaces = Some(trustManifest.digest), activationEpoch = 1L).fold(e => fail(e), identity)
    val payload = Canon.encode(draft.bodyCanon)
    val rotated = draft.copy(predecessorApprovals = List(owner.name -> owner.sign(payload)))
    val (cas, before, after) = rotationFixture(trustManifest.digest, rotated.digest, replicaSet, replicaSet)
    cas.put(trustManifest.artifact); cas.put(rotated.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, Nil, Some(cert.digest))
    val result = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("approvals does not equal exactly")), result.toString)

  test("verify accepts a replica-set rotation with predecessor quorum and a listed approval"):
    val successorReplicas = replicas.take(3) ++ List(Keypair.dev("r4"))
    val draft = ReplicaSetManifest.of(successorReplicas.map(k => k.name -> k.publicBytes),
      replaces = Some(replicaSet.digest), activationHeight = 1L).fold(e => fail(e), identity)
    val payload = Canon.encode(draft.bodyCanon)
    val approvals = replicas.take(BftQuorum.quorumSize(4)).map(k => k.name -> k.sign(payload))
    val approvedDraft = ReplicaSetManifest.withPredecessorApprovals(draft, approvals).fold(e => fail(e), identity)
    val successor = BftFinality.sealReplicaSet(successorReplicas, replaces = Some(replicaSet.digest), activationHeight = 1L)
      .fold(e => fail(e), identity).copy(predecessorApprovals = approvedDraft.predecessorApprovals)
    val (cas, before, after) = rotationFixture(trustManifest.digest, trustManifest.digest, replicaSet, successor)
    cas.put(trustManifest.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      successorReplicas, successor, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, List(successor.digest), Some(cert.digest))
    val verified = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
      .fold(e => fail(e), identity)
    assertEquals(verified.after, after)

  test("verify rejects a replica-set rotation missing predecessor quorum"):
    val successorReplicas = replicas.take(3) ++ List(Keypair.dev("r5"))
    val successor = BftFinality.sealReplicaSet(successorReplicas, replaces = Some(replicaSet.digest), activationHeight = 1L)
      .fold(e => fail(e), identity)
    val (cas, before, after) = rotationFixture(trustManifest.digest, trustManifest.digest, replicaSet, successor)
    cas.put(trustManifest.artifact)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      successorReplicas, successor, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, List(successor.digest), Some(cert.digest))
    val result = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
    assert(result.isLeft, result.toString)

  /** A transition that advances the GC epoch only — repository/application/
    * namespace/trust-roots all unchanged, no commits.
    */
  private def gcEpochFixture(afterEpoch: ReplicatedGcEpoch): (DiskCas, FederationState, FederationState) =
    val dir = Files.createTempDirectory("cairn-fedtx-check-gcepoch")
    val cas = DiskCas(dir.resolve("cas"))
    val ledger = standIn("ledger-stand-in-gcepoch")
    val beforeEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val repoIndex = RepositoryIndex(Map("org-a" -> standIn("gcepoch-repo")))
    val appIndex = ApplicationIndex(Map("org-a" -> standIn("gcepoch-app")))
    val nsIndex = NamespaceIndex(Map("org-a" -> trustManifest.digest))
    val before = FederationState(ledger, repoIndex.digest, appIndex.digest, nsIndex.digest, replicaSet.digest, beforeEpoch.digest)
    val after = FederationState(ledger, repoIndex.digest, appIndex.digest, nsIndex.digest, replicaSet.digest, afterEpoch.digest)
    List(replicaSet.artifact, trustManifest.artifact, repoIndex.artifact, appIndex.artifact, nsIndex.artifact,
      beforeEpoch.artifact, afterEpoch.artifact, before.artifact, after.artifact).foreach(cas.put)
    (cas, before, after)

  test("verify accepts a gcEpoch that strictly increases and hash-links to its predecessor"):
    val beforeEpoch = ReplicatedGcEpoch(0, Set.empty, None)
    val afterEpoch = ReplicatedGcEpoch(1, Set.empty, Some(beforeEpoch.digest))
    val (cas, before, after) = gcEpochFixture(afterEpoch)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, Nil, Some(cert.digest))
    val verified = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
      .fold(e => fail(e), identity)
    assertEquals(verified.after, after)

  test("verify rejects a gcEpoch that does not hash-link to its predecessor"):
    val afterEpoch = ReplicatedGcEpoch(1, Set.empty, Some(Digest.of(Canon.CStr("some-other-epoch"))))
    val (cas, before, after) = gcEpochFixture(afterEpoch)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, Nil, Some(cert.digest))
    val result = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("does not hash-link")), result.toString)

  test("verify rejects a gcEpoch number that does not strictly increase"):
    val afterEpoch = ReplicatedGcEpoch(0, Set.empty, Some(Digest.of(Canon.CStr("dummy-predecessor"))))
    val (cas, before, after) = gcEpochFixture(afterEpoch)
    val cert = FederationFinality.agreeForFederationStateLocalTestOnly(
      replicas, replicaSet, view = 0, stateDigest = after.digest, epoch = 1L,
      previousState = before.digest, federationId = federationId).fold(e => fail(e), identity)
    val transition = FederationTransition(before.digest, Nil, after.digest, Nil, Some(cert.digest))
    val result = VerifiedFederationTransition.verify(transition, before, after, Nil, cert, federationId, cas)
    assert(result.left.exists(_.contains("does not exceed predecessor")), result.toString)
