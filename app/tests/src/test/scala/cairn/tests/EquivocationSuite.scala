package cairn.tests

import cairn.kernel.*
import cairn.kernel.BftQuorum.*
import cairn.runtime.EffectContexts
import cairn.systemhandler.{BftFinality, BftReplica, EquivocationEvidence, Keypair, Node}

/** PR31 slice 5: equivocation as first-class evidence.
  *
  * `EquivocationEvidence.detect` is tested standalone (its own signature/
  * shape preconditions), and separately end-to-end through `BftReplica`'s
  * real conflict-detection path: a genuine prepared lock is built up via
  * directly-signed `Prepare` messages from the other replicas (no HTTP
  * networking needed — `receive` doesn't care about transport), then a
  * second, conflicting `PrePrepare` from the same primary is fed in and
  * `detectedEquivocations` is asserted to have captured it.
  */
class EquivocationSuite extends munit.FunSuite:
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
  private val setDig = manifest.replicaSetDigest
  private val federationId = Digest.of(Canon.CStr("federation-chain"))

  private def signedPrePrepare(kp: Keypair, view: Int, seq: Int, block: Digest): BftFinality.SignedMsg =
    BftFinality.sign(kp, Msg.PrePrepare(view, seq, BftFinality.valueOfBlock(block), ReplicaId(kp.name)), setDig, federationId)
      .fold(e => fail(e), identity)

  test("EquivocationEvidence.detect: two conflicting PrePrepares from the same replica produce verifiable evidence"):
    val blockA = Digest.of(Canon.CStr("block-a"))
    val blockB = Digest.of(Canon.CStr("block-b"))
    val a = signedPrePrepare(replicas.head, 0, 0, blockA)
    val b = signedPrePrepare(replicas.head, 0, 0, blockB)
    val evidence = EquivocationEvidence.detect(a, b, manifest.authorities, setDig, federationId)
      .fold(e => fail(e), identity)
    assertEquals(evidence.replica, ReplicaId("r0"))
    assertEquals(evidence.view, 0)
    assertEquals(evidence.seq, 0)
    val back = EquivocationEvidence.fromArtifact(evidence.artifact).fold(e => fail(e), identity)
    assertEquals(back, evidence)

  test("EquivocationEvidence.detect rejects identical values (not a conflict)"):
    val blockA = Digest.of(Canon.CStr("block-a"))
    val a = signedPrePrepare(replicas.head, 0, 0, blockA)
    val aAgain = signedPrePrepare(replicas.head, 0, 0, blockA)
    assert(EquivocationEvidence.detect(a, aAgain, manifest.authorities, setDig, federationId).isLeft)

  test("EquivocationEvidence.detect rejects proposals from different replicas"):
    val blockA = Digest.of(Canon.CStr("block-a"))
    val blockB = Digest.of(Canon.CStr("block-b"))
    val a = signedPrePrepare(replicas.head, 0, 0, blockA)
    val b = signedPrePrepare(replicas(1), 0, 0, blockB)
    assert(EquivocationEvidence.detect(a, b, manifest.authorities, setDig, federationId).isLeft)

  test("EquivocationEvidence.detect rejects proposals for different (view, seq)"):
    val blockA = Digest.of(Canon.CStr("block-a"))
    val blockB = Digest.of(Canon.CStr("block-b"))
    val a = signedPrePrepare(replicas.head, 0, 0, blockA)
    val b = signedPrePrepare(replicas.head, 0, 1, blockB)
    assert(EquivocationEvidence.detect(a, b, manifest.authorities, setDig, federationId).isLeft)

  test("EquivocationEvidence.detect rejects a forged seal"):
    val blockA = Digest.of(Canon.CStr("block-a"))
    val blockB = Digest.of(Canon.CStr("block-b"))
    val a = signedPrePrepare(replicas.head, 0, 0, blockA)
    val b = signedPrePrepare(replicas.head, 0, 0, blockB).copy(seal = Vector.fill(64)(0: Byte))
    assert(EquivocationEvidence.detect(a, b, manifest.authorities, setDig, federationId).isLeft)

  test("EquivocationEvidence.detect requires both replicaSet and federation/chain-id bindings to match"):
    val blockA = Digest.of(Canon.CStr("block-a"))
    val blockB = Digest.of(Canon.CStr("block-b"))
    val a = signedPrePrepare(replicas.head, 0, 0, blockA)
    val b = signedPrePrepare(replicas.head, 0, 0, blockB)
    val otherReplicaSet = Digest.of(Canon.CStr("other-replica-set"))
    assert(EquivocationEvidence.detect(a, b, manifest.authorities, otherReplicaSet, federationId).isLeft)
    val otherFederation = Digest.of(Canon.CStr("other-federation"))
    assert(EquivocationEvidence.detect(a, b, manifest.authorities, setDig, otherFederation).isLeft)

  test("BftReplica.receive: a NEW-view primary re-proposing something OTHER than the carried-forward prepared value mints evidence, and is still rejected"):
    // The `preparedLock`-based conflict guard in `bind` is a cross-view
    // safety check (locked.preparedView < beforeView) — it defends against
    // a new-view primary proposing something other than what a quorum
    // already prepared in an OLDER view, not a same-view double-PrePrepare
    // (that narrower case is silently no-op'd inside BftQuorum.deliver
    // itself: `if slot.prePrepare.isDefined then (state, Nil)`). So a real
    // trigger of this evidence path needs a genuine view-change first.
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val homes = replicas.map(k => k.name -> java.nio.file.Files.createTempDirectory(s"cairn-equiv-${k.name}")).toMap
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), EffectContexts.forLedger())
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      id -> n
    }
    val block1 = nodes("r0").chainDigests.head
    val bfts = replicas.map { k =>
      k.name -> BftReplica.certified(k, manifest, node = Some(nodes(k.name)), ledgerAuth = ledgerAuth)
        .fold(e => fail(e), identity)
    }.toMap
    def isCommit(sm: BftFinality.SignedMsg): Boolean = sm.msg.isInstanceOf[Msg.Commit]
    def deliverAll(from: String, msgs: List[BftFinality.SignedMsg], exclude: Set[String] = Set.empty, dropCommits: Boolean = false): Unit =
      val filtered = if dropCommits then msgs.filterNot(isCommit) else msgs
      filtered.foreach { sm => bfts.foreach { (id, r) => if id != from && !exclude.contains(id) then r.receive(sm).fold(e => fail(s"$id: $e"), identity) } }
    // r0 proposes block1 in view 0; circulate Prepares only (drop Commits) so
    // no certificate finalizes — the goal is a genuine PREPARED (not yet
    // committed) state, forcing view-change to be the only way forward.
    val out0 = bfts("r0").propose(0, 0, block1).fold(e => fail(e), identity)
    deliverAll("r0", out0, dropCommits = true)
    var round = 0
    var progress = true
    while round < 16 && progress do
      progress = false
      bfts.foreach { (id, r) =>
        val out = r.drainOutbound().filterNot(isCommit)
        if out.nonEmpty then
          progress = true
          deliverAll(id, out, exclude = Set("r0"), dropCommits = true)
      }
      round += 1
    val honest = List("r1", "r2", "r3")
    assert(honest.forall(id => bfts(id).finalityCerts.isEmpty))
    // Fail r0; view-change to view 1 carries the view-0 prepared value forward.
    honest.foreach { id =>
      val out = bfts(id).requestViewChange(1).fold(e => fail(e), identity)
      deliverAll(id, out, exclude = Set("r0"))
    }
    // Drop Commits here too: once a certificate mints for seq 0, BftReplica
    // prunes slot/lock evidence for every seq <= the new finalized high-water
    // (it's no longer needed once durably certified) — which would erase the
    // very view-0 prepared evidence this test needs `preparedLock` to still
    // find. Stopping at PREPARE-quorum in view 1 (mirroring how view 0 itself
    // was deliberately left uncommitted above) keeps that evidence resident.
    round = 0
    progress = true
    while round < 24 && progress do
      progress = false
      honest.foreach { id =>
        val out = bfts(id).drainOutbound().filterNot(isCommit)
        if out.nonEmpty then
          progress = true
          deliverAll(id, out, exclude = Set("r0"), dropCommits = true)
      }
      round += 1
    assert(honest.forall(id => bfts(id).currentView == 1), clues(honest.map(id => id -> bfts(id).currentView)))
    assert(honest.forall(id => bfts(id).finalityCerts.isEmpty), "no certificate should have minted yet")
    // r1 is the new primary for view 1. A genuinely equivocating r1 signs a
    // DIFFERENT value for the same seq instead of the carried-forward block1.
    val forged = Digest.of(Canon.CStr("equivocating-alternate-value"))
    val ppForged = BftFinality.sign(
      replicas(1), Msg.PrePrepare(1, 0, BftFinality.valueOfBlock(forged), ReplicaId("r1")),
      setDig, bfts("r2").chainId).fold(e => fail(e), identity)
    val rejected = bfts("r2").receive(ppForged)
    assert(rejected.isLeft, "the equivocating PrePrepare must still be rejected")
    assert(rejected.left.exists(_.contains("conflicts with prepared lock")), rejected.toString)
    val evidence = bfts("r2").detectedEquivocations
    assertEquals(evidence.length, 1)
    assertEquals(evidence.head.replica, ReplicaId("r1"))
    assertEquals(evidence.head.seq, 0)
    val valueDigests = Set(evidence.head.proposalA, evidence.head.proposalB).flatMap {
      case sm if sm.msg.isInstanceOf[Msg.PrePrepare] => Some(sm.msg.asInstanceOf[Msg.PrePrepare].value.digest)
      case _ => None
    }
    assertEquals(valueDigests, Set(BftFinality.valueOfBlock(block1).digest, BftFinality.valueOfBlock(forged).digest))
    // The evidence artifact is independently verifiable/round-trippable.
    val back = EquivocationEvidence.fromArtifact(evidence.head.artifact).fold(e => fail(e), identity)
    assertEquals(back, evidence.head)
