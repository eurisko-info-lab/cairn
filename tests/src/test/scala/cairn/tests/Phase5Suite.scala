package cairn.tests

import cairn.systemhandler.{CasEffects, EffectContext}
import cairn.kernel.*
import cairn.ledger.*
import cairn.examples.stlc.Stlc

/** Phase 5 acceptance (S35–S40): local PoA chain publishes the STLC pack; a
  * second local node verifies blocks and materializes artifacts by digest.
  * Phase 6 acceptance (S41–S42): sync + divergence surfacing.
  */
class Phase5Suite extends munit.FunSuite:
  val alice = Keypair.dev("alice")
  def authorities = Map("alice" -> alice.publicBytes)

  private def casPut(node: Node, art: Artifact): Unit =
    CasEffects.put(node.cas, art, node.ctx).fold(e => fail(e.toString), identity)

  private def casGet(node: Node, key: TypedKey): Artifact =
    CasEffects.get(node.cas, key.valueHash, node.ctx).fold(e => throw AssertionError(e.toString), identity)

  private def casGetDigest(node: Node, d: Digest): Either[String, Artifact] =
    CasEffects.get(node.cas, d, node.ctx).left.map(_.toString)

  test("tx digests are stable canonical values (S35)"):
    val tx = Tx.PublishArtifact(Stlc.base.artifact.key)
    assertEquals(Tx.digest(tx), Tx.digest(Tx.fromCanon(Tx.toCanon(tx))))

  test("bad signature rejected (S36)"):
    val mallory = Keypair.dev("mallory")
    val tx = Tx.RegisterIdentity("alice", alice.publicBytes)
    val forged = SignedTx(tx, "alice", mallory.signTx(tx).signature)
    assert(LedgerKernel.applyTx(LedgerState.genesis, forged, Ed25519.verify)
      .swap.exists(_.contains("bad signature")))

  test("same txs => same state root (S37)"):
    val stx = alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))
    val s1 = LedgerKernel.applyTx(LedgerState.genesis, stx, Ed25519.verify).toOption.get
    val s2 = LedgerKernel.applyTx(LedgerState.genesis, stx, Ed25519.verify).toOption.get
    assertEquals(s1.root, s2.root)

  test("head cannot point at unpublished artifact (S37/§4.9)"):
    val stx = alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))
    val st = LedgerKernel.applyTx(LedgerState.genesis, stx, Ed25519.verify).toOption.get
    val head = alice.signTx(Tx.SetBranchHead("main", Stlc.base.artifact.key))
    assert(LedgerKernel.applyTx(st, head, Ed25519.verify).swap.exists(_.contains("not published")))

  def publishStlc(node: Node): Block =
    val lang = Stlc.language
    // ledger records digests + heads; bodies go to CAS (§4.9)
    Stlc.fragments.foreach(f => casPut(node, f.artifact))
    casPut(node, lang.artifact)
    val txs = List(
      alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))) ++
      Stlc.fragments.map(f => alice.signTx(Tx.PublishArtifact(f.artifact.key))) ++
      List(
        alice.signTx(Tx.PublishArtifact(lang.artifact.key)),
        alice.signTx(Tx.SetBranchHead("main", lang.artifact.key)))
    node.append(alice, authorities, txs).fold(e => throw AssertionError(e), identity)

  test("PoA block seals, tampered block rejected (S38)"):
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-node"), EffectContext.forLedger())
    val block = publishStlc(node)
    assertEquals(node.state(authorities).map(_.heads.keySet), Right(Set("main")))
    // tamper: swap authority name
    val tampered = block.copy(authority = "mallory")
    assert(LedgerKernel.applyBlock(LedgerState.genesis, LedgerKernel.genesisParent, 0,
      authorities + ("mallory" -> Keypair.dev("mallory").publicBytes), tampered, Ed25519.verify)
      .swap.exists(_.contains("seal")))
    // tamper: drop a tx but keep the seal
    val tampered2 = block.copy(txs = block.txs.drop(1))
    assert(LedgerKernel.applyBlock(LedgerState.genesis, LedgerKernel.genesisParent, 0,
      authorities, tampered2, Ed25519.verify).isLeft)

  test("publication records digests only, second node materializes by hash (S39/S40 acceptance)"):
    val nodeA = Node(java.nio.file.Files.createTempDirectory("cairn-nodeA"), EffectContext.forLedger())
    publishStlc(nodeA)
    // "second local node process": fresh object over a different directory
    val nodeB = Node(java.nio.file.Files.createTempDirectory("cairn-nodeB"), EffectContext.forLedger())
    val fetched = Sync.pull(nodeA, nodeB, authorities).fold(e => throw AssertionError(e), identity)
    assert(fetched.nonEmpty)
    // node B independently verifies the chain and reads the branch head
    val st = nodeB.state(authorities).fold(e => throw AssertionError(e), identity)
    val head = st.heads("main")
    assertEquals(head, Stlc.language.artifact.key)
    // and materializes the artifact body from its own CAS by digest
    val art = casGet(nodeB, head)
    assertEquals(art.digest, Stlc.language.digest)
    // fragments are retrievable by digest too
    for f <- Stlc.fragments do
      assertEquals(casGetDigest(nodeB, f.artifact.digest).map(_.digest), Right(f.artifact.digest))

  test("two nodes converge on a published head (S41 acceptance)"):
    val a = Node(java.nio.file.Files.createTempDirectory("cairn-a"), EffectContext.forLedger())
    val b = Node(java.nio.file.Files.createTempDirectory("cairn-b"), EffectContext.forLedger())
    publishStlc(a)
    Sync.pull(a, b, authorities).toOption.get
    assertEquals(a.chainDigests, b.chainDigests)
    assertEquals(a.state(authorities).map(_.root), b.state(authorities).map(_.root))

  test("divergence surfaces as competing heads, no silent corruption (S42 acceptance)"):
    val a = Node(java.nio.file.Files.createTempDirectory("cairn-a2"), EffectContext.forLedger())
    val b = Node(java.nio.file.Files.createTempDirectory("cairn-b2"), EffectContext.forLedger())
    // both start from the same genesis block
    val reg = List(alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes)))
    a.append(alice, authorities, reg).toOption.get
    Sync.pull(a, b, authorities).toOption.get
    // then fork: different second blocks
    val claimA = Digest.of(Canon.CStr("claim-a"))
    val claimB = Digest.of(Canon.CStr("claim-b"))
    a.append(alice, authorities, List(alice.signTx(Tx.RecordCertificate(claimA)))).toOption.get
    b.append(alice, authorities, List(alice.signTx(Tx.RecordCertificate(claimB)))).toOption.get
    Sync.compare(a.chainDigests, b.chainDigests) match
      case Sync.Comparison.Diverged(at, mine, other) =>
        assertEquals(at, 1)
        assert(mine != other)
      case other => fail(s"expected divergence, got $other")
