package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{Ed25519, Keypair}

/** PR31 slice 3: namespace-trust rotation. Mirrors
  * [[cairn.kernel.ReplicaSetManifest]]'s own amendment-policy test style
  * (`DistributionDaemonSuite`'s "replica-set digest binds public keys and
  * transition metadata"), with a plain owner-majority in place of BFT
  * quorum math.
  */
class NamespaceTrustSuite extends munit.FunSuite:
  private def owners(names: String*): List[Keypair] = names.toList.map(Keypair.dev)

  test("genesis manifest: well-formed, no replaces/predecessorApprovals allowed"):
    val a = owners("o0", "o1", "o2")
    val m = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    assertEquals(m.namespace, "acme")
    assertEquals(m.n, 3)
    assertEquals(NamespaceTrustManifest.allowsTransition(m, None), Right(()))
    val withReplaces = m.copy(replaces = Some(Digest.of(Canon.CStr("x"))))
    assert(NamespaceTrustManifest.allowsTransition(withReplaces, None).isLeft)
    val withApprovals = m.copy(predecessorApprovals = List("o0" -> Vector[Byte](1)))
    assert(NamespaceTrustManifest.allowsTransition(withApprovals, None).isLeft)

  test("digest binds namespace, owners, and transition metadata — different owners/namespace ⇒ different digest"):
    val a = owners("o0", "o1", "o2")
    val b = List("o0", "o1", "o2").map(id => Keypair.dev(s"$id-alt").copy(name = id))
    val mA = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val mB = NamespaceTrustManifest.of("acme", b.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    assert(mA.bodyDigest != mB.bodyDigest, "different keys under the same ids must change the digest")
    val mOtherNamespace = NamespaceTrustManifest.of("other", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    assert(mA.bodyDigest != mOtherNamespace.bodyDigest)

  test("amendment: majority of predecessor owners required, disjoint/insufficient approvals rejected"):
    val a = owners("o0", "o1", "o2")
    val mA = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val draft = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes),
      replaces = Some(mA.digest), activationEpoch = 10L).fold(e => fail(e), identity)
    assert(draft.bodyDigest != mA.bodyDigest)
    // No predecessor approvals at all: refused.
    assert(NamespaceTrustManifest.allowsTransition(draft, Some(mA), Some(mA.digest), Ed25519.verify).isLeft)
    val payload = Canon.encode(draft.bodyCanon)
    // majority(3) == 2
    val approvals = a.take(2).map(k => k.name -> k.sign(payload))
    val amended = draft.copy(predecessorApprovals = approvals.sortBy(_._1))
    assertEquals(
      NamespaceTrustManifest.allowsTransition(amended, Some(mA), Some(mA.digest), Ed25519.verify), Right(()))
    // Only one approval: below majority.
    val underQuorum = draft.copy(predecessorApprovals = a.take(1).map(k => k.name -> k.sign(payload)))
    assert(NamespaceTrustManifest.allowsTransition(underQuorum, Some(mA), Some(mA.digest), Ed25519.verify).isLeft)
    // New-set (disjoint) seals must not count as predecessor approvals.
    val b = owners("p0", "p1", "p2")
    val disjointDraft = NamespaceTrustManifest.of("acme", b.map(k => k.name -> k.publicBytes),
      replaces = Some(mA.digest), activationEpoch = 10L).fold(e => fail(e), identity)
    val badApprovals = b.take(2).map(k => k.name -> k.sign(Canon.encode(disjointDraft.bodyCanon)))
    val disjoint = disjointDraft.copy(predecessorApprovals = badApprovals.sortBy(_._1))
    assert(NamespaceTrustManifest.allowsTransition(disjoint, Some(mA), Some(mA.digest), Ed25519.verify).isLeft,
      "new-set seals must not count as predecessorApprovals")

  test("amendment: must cite the exact predecessor digest, and strictly increase activationEpoch"):
    val a = owners("o0", "o1", "o2")
    val mA = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val wrongReplaces = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes),
      replaces = Some(Digest.of(Canon.CStr("not-mA"))), activationEpoch = 10L).fold(e => fail(e), identity)
    val payload1 = Canon.encode(wrongReplaces.bodyCanon)
    val wrongReplacesApproved = wrongReplaces.copy(predecessorApprovals = a.take(2).map(k => k.name -> k.sign(payload1)))
    assert(NamespaceTrustManifest.allowsTransition(wrongReplacesApproved, Some(mA), Some(mA.digest), Ed25519.verify).isLeft)

    val nonIncreasing = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes),
      replaces = Some(mA.digest), activationEpoch = 0L).fold(e => fail(e), identity)
    val payload2 = Canon.encode(nonIncreasing.bodyCanon)
    val nonIncreasingApproved = nonIncreasing.copy(predecessorApprovals = a.take(2).map(k => k.name -> k.sign(payload2)))
    assert(NamespaceTrustManifest.allowsTransition(nonIncreasingApproved, Some(mA), Some(mA.digest), Ed25519.verify).isLeft)

  test("amendment: mismatched namespace between predecessor and proposed is rejected"):
    val a = owners("o0", "o1", "o2")
    val mA = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val wrongNamespace = NamespaceTrustManifest.of("other", a.map(k => k.name -> k.publicBytes),
      replaces = Some(mA.digest), activationEpoch = 10L).fold(e => fail(e), identity)
    val payload = Canon.encode(wrongNamespace.bodyCanon)
    val approved = wrongNamespace.copy(predecessorApprovals = a.take(2).map(k => k.name -> k.sign(payload)))
    assert(NamespaceTrustManifest.allowsTransition(approved, Some(mA), Some(mA.digest), Ed25519.verify).isLeft)

  test("seal/verifySeals round-trip; canon round-trip preserves seals; activeAt selects by epoch"):
    val a = owners("o0", "o1", "o2")
    val draft = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val sealedManifest = NamespaceTrustManifest.seal(draft, a.map(k => k.name -> ((msg: Array[Byte]) => k.sign(msg))))
      .fold(e => fail(e), identity)
    NamespaceTrustManifest.verifySeals(sealedManifest, Ed25519.verify).fold(e => fail(e), identity)
    val reloaded = NamespaceTrustManifest.fromArtifact(sealedManifest.artifact).fold(e => fail(e), identity)
    assertEquals(reloaded, sealedManifest)
    assertEquals(NamespaceTrustManifest.activeAt(List(sealedManifest), 0), Right(sealedManifest))

  test("seal rejects a signer set that doesn't exactly cover the owner ids"):
    val a = owners("o0", "o1", "o2")
    val draft = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val partial = NamespaceTrustManifest.seal(draft, a.take(2).map(k => k.name -> ((msg: Array[Byte]) => k.sign(msg))))
    assert(partial.isLeft)

  test("wellFormed rejects empty namespace, empty owners, duplicate ids, negative activationEpoch"):
    val a = owners("o0", "o1")
    assert(NamespaceTrustManifest.of("", a.map(k => k.name -> k.publicBytes)).isLeft)
    assert(NamespaceTrustManifest.of("acme", Nil).isLeft)
    assert(NamespaceTrustManifest.of("acme", List("o0" -> a.head.publicBytes, "o0" -> a.head.publicBytes)).isLeft)
    val m = NamespaceTrustManifest.of("acme", a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    assert(NamespaceTrustManifest.wellFormed(m.copy(activationEpoch = -1)).isLeft)

  test("fromArtifact rejects a non-certificate artifact and a wrongly-tagged certificate body"):
    assert(NamespaceTrustManifest.fromArtifact(Artifact(ArtifactKind.ChangeSet, Canon.CStr("nope"))).isLeft)
    assert(NamespaceTrustManifest.fromArtifact(Artifact(ArtifactKind.Certificate, Canon.CTag("replica-set-manifest", Canon.CStr("nope")))).isLeft)
