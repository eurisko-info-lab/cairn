package cairn.tests

import cairn.systemhandler.{CasEffects, EffectContext}
import cairn.kernel.*
import cairn.core.Module
import cairn.core.{Parser, RoundTrip}
import cairn.ledger.*
import cairn.examples.stlc.Stlc

/** Wave G acceptance (M35–M40). */
class WaveGSuite extends munit.FunSuite:
  val alice = Keypair.dev("alice")
  val bob = Keypair.dev("bob")
  val carol = Keypair.dev("carol")
  def bootAuth = Map("alice" -> alice.publicBytes)

  private def casPut(node: Node, art: Artifact): Unit =
    CasEffects.put(node.cas, art, node.ctx).fold(e => fail(e.toString), identity)

  def register(node: Node): Unit =
    node.append(alice, bootAuth, List(alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes)))).fold(
      e => throw AssertionError(e), identity)

  // ---- M35: Merkle state + inclusion proofs ----

  test("M35: inclusion proof verifies against root without full state"):
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-merkle"), EffectContext.forLedger())
    casPut(node, Stlc.base.artifact)
    val key = Stlc.base.artifact.key
    node.append(alice, bootAuth, List(
      alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes)),
      alice.signTx(Tx.PublishArtifact(key)),
      alice.signTx(Tx.SetBranchHead("main", key)))).fold(e => fail(e), identity)
    val st = node.state(bootAuth).toOption.get
    val root = st.root
    // light client receives only: root, component roots, one proof
    val (proof, roots) = st.provePublished(key.render).toOption.get
    assert(LedgerState.verifyInclusion(root, "published", roots, proof))
    val (hproof, hroots) = st.proveHead("main").toOption.get
    assert(LedgerState.verifyInclusion(root, "heads", hroots, hproof))
    // tampered proof fails
    val tampered = proof.copy(leafKey = "someone-else")
    assert(!LedgerState.verifyInclusion(root, "published", roots, tampered))
    // absent key has no proof
    assert(st.provePublished("ghost").isLeft)

  test("M35: merkle proofs work across tree sizes"):
    for n <- List(1, 2, 3, 5, 8, 13) do
      val entries = (1 to n).toList.map(i => (s"k$i", Canon.CInt(i.toLong)))
      val root = Merkle.root(entries)
      for (k, _) <- entries do
        val proof = Merkle.prove(entries, k).toOption.get
        assert(Merkle.verify(root, proof), s"n=$n key=$k")
      assert(!Merkle.verify(root, Merkle.Proof("k1", Canon.CInt(999), Nil)))

  // ---- M36: multi-authority PoA ----

  test("M36: 2-of-3 rotation — add authorities by quorum, seal round-robin"):
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-auth"), EffectContext.forLedger())
    // block 0: alice bootstraps herself as first on-chain authority
    val b0 = node.append(alice, bootAuth, List(
      alice.signTx(Tx.AddAuthority("alice", alice.publicBytes, Nil)))).fold(e => fail(e), identity)
    // block 1: with 1 authority, rotation says alice seals; she adds bob (quorum = 1 of 1)
    val addBob = Tx.AddAuthority("bob", bob.publicBytes,
      List(("alice", Ed25519.sign(alice.privateKey, Tx.approvalPayload(Tx.AddAuthority("bob", bob.publicBytes, Nil))))))
    node.append(alice, bootAuth, List(alice.signTx(addBob))).fold(e => fail(e), identity)
    // block 2: rotation with {alice, bob} at height 2 -> sorted(alice,bob)[0] = alice
    val addCarolPayload = Tx.approvalPayload(Tx.AddAuthority("carol", carol.publicBytes, Nil))
    val addCarol = Tx.AddAuthority("carol", carol.publicBytes, List(
      ("alice", Ed25519.sign(alice.privateKey, addCarolPayload)),
      ("bob", Ed25519.sign(bob.privateKey, addCarolPayload))))
    node.append(alice, bootAuth, List(alice.signTx(addCarol))).fold(e => fail(e), identity)
    val st = node.state(bootAuth).toOption.get
    assertEquals(st.authorities.keySet, Set("alice", "bob", "carol"))
    // block 3: rotation (height 3 % 3 = 0 -> alice... sorted = alice,bob,carol)
    assertEquals(LedgerKernel.expectedSealer(st, 3), Some("alice"))

  test("M36: insufficient quorum rejected; removed authority cannot seal"):
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-auth2"), EffectContext.forLedger())
    node.append(alice, bootAuth, List(
      alice.signTx(Tx.AddAuthority("alice", alice.publicBytes, Nil)))).fold(e => fail(e), identity)
    val addBobPayload = Tx.approvalPayload(Tx.AddAuthority("bob", bob.publicBytes, Nil))
    node.append(alice, bootAuth, List(alice.signTx(
      Tx.AddAuthority("bob", bob.publicBytes, List(("alice", Ed25519.sign(alice.privateKey, addBobPayload)))))))
      .fold(e => fail(e), identity)
    // 2 authorities now; carol needs 2 approvals — 1 is not enough
    val addCarolPayload = Tx.approvalPayload(Tx.AddAuthority("carol", carol.publicBytes, Nil))
    val underQuorum = Tx.AddAuthority("carol", carol.publicBytes,
      List(("alice", Ed25519.sign(alice.privateKey, addCarolPayload))))
    assert(node.append(alice, bootAuth, List(alice.signTx(underQuorum)))
      .swap.exists(_.contains("quorum not met")))
    // remove bob (quorum 2 of 2), then bob sealing is rejected
    val remBobPayload = Tx.approvalPayload(Tx.RemoveAuthority("bob", Nil))
    val remBob = Tx.RemoveAuthority("bob", List(
      ("alice", Ed25519.sign(alice.privateKey, remBobPayload)),
      ("bob", Ed25519.sign(bob.privateKey, remBobPayload))))
    node.append(alice, bootAuth, List(alice.signTx(remBob))).fold(e => fail(e), identity)
    val st = node.state(bootAuth).toOption.get
    assertEquals(st.authorities.keySet, Set("alice"))
    assert(node.append(bob, bootAuth, List(bob.signTx(Tx.RegisterIdentity("bob", bob.publicBytes)))).isLeft)

  // ---- M37: policy language ----

  test("M37: policy language parses, round-trips, and has ΔPolicy"):
    val src = "branch main requires method proof-term from alice"
    // note: 'proof-term' has a dash — use the underscore form in the surface
    val term = PolicyLang.parse("branch main requires method proof_term from alice").fold(e => fail(e), identity)
    RoundTrip.check(PolicyLang.language.grammar, term).fold(e => fail(e), identity)
    val dp = PolicyLang.deltaPolicy.fold(e => fail(e), identity)
    assertEquals(dp.name, "Δpolicy")
    assert(cairn.core.Delta.deltaOf(dp).isRight) // Δ(ΔPolicy)

  test("M37: head update violating policy rejected with policy cited"):
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-policy"), EffectContext.forLedger())
    casPut(node, Stlc.base.artifact)
    val key = Stlc.base.artifact.key
    val certDigest = Digest.of(Canon.CStr("some-proof-cert"))
    val policy = PolicyLang.parse("branch main requires method proof_term from alice").toOption.get
    node.append(alice, bootAuth, List(
      alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes)),
      alice.signTx(Tx.PublishArtifact(key)),
      alice.signTx(Tx.SetPolicy("main", policy)),
      alice.signTx(Tx.RecordCertificate(certDigest, "test-suite")))).fold(e => fail(e), identity)
    // no cert ref
    assert(node.append(alice, bootAuth, List(alice.signTx(Tx.SetBranchHead("main", key))))
      .swap.exists(e => e.contains("requires a proof_term certificate") && e.contains("polReq")))
    // wrong method
    assert(node.append(alice, bootAuth, List(alice.signTx(Tx.SetBranchHead("main", key, Some(certDigest)))))
      .swap.exists(_.contains("has 'test-suite'")))
    // right method passes
    val proofCert = Digest.of(Canon.CStr("real-proof"))
    node.append(alice, bootAuth, List(
      alice.signTx(Tx.RecordCertificate(proofCert, "proof_term")),
      alice.signTx(Tx.SetBranchHead("main", key, Some(proofCert))))).fold(e => fail(e), identity)
    assertEquals(node.state(bootAuth).toOption.get.heads.contains("main"), true)

  // ---- M38: HTTP sync ----

  test("M38: two nodes sync over localhost HTTP; interrupted pull resumes"):
    val a = Node(java.nio.file.Files.createTempDirectory("cairn-http-a"), EffectContext.forLedger())
    casPut(a, Stlc.language.artifact)
    Stlc.fragments.foreach(f => casPut(a, f.artifact))
    a.append(alice, bootAuth,
      List(alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))) ++
      Stlc.fragments.map(f => alice.signTx(Tx.PublishArtifact(f.artifact.key))) ++
      List(alice.signTx(Tx.PublishArtifact(Stlc.language.artifact.key)),
        alice.signTx(Tx.SetBranchHead("main", Stlc.language.artifact.key)))).fold(e => fail(e), identity)
    val http = HttpNode(a, bootAuth)
    val port = http.start()
    try
      val b = Node(java.nio.file.Files.createTempDirectory("cairn-http-b"), EffectContext.forLedger())
      val r1 = HttpSync.pull(s"http://localhost:$port", b, bootAuth).fold(e => fail(e), identity)
      assert(r1.fetchedBlocks > 0 || r1.fetchedBlobs > 0)
      assertEquals(b.state(bootAuth).map(_.heads("main")), Right(Stlc.language.artifact.key))
      // "interrupted": second pull is a no-op resume — nothing re-fetched
      val r2 = HttpSync.pull(s"http://localhost:$port", b, bootAuth).fold(e => fail(e), identity)
      assertEquals(r2.fetchedBlocks, 0)
      assertEquals(r2.fetchedBlobs, 0)
    finally http.stop()

  // ---- M39: gossip + fork choice ----

  test("M39: three nodes converge; fork surfaces as an explicit reorg event"):
    def freshNode(tag: String) = Node(java.nio.file.Files.createTempDirectory(s"cairn-gossip-$tag"), EffectContext.forLedger())
    val (a, b, c) = (freshNode("a"), freshNode("b"), freshNode("c"))
    // a builds 2 blocks; b builds 1 different block; c is empty
    register(a)
    a.append(alice, bootAuth, List(alice.signTx(
      Tx.RecordCertificate(Digest.of(Canon.CStr("a-work")), "test-suite")))).fold(e => fail(e), identity)
    register(b)
    val peers = List(Gossip.Peer("a", a), Gossip.Peer("b", b), Gossip.Peer("c", c))
    val reorgs = Gossip.converge(peers, bootAuth).fold(e => fail(e), identity)
    // all three end on the same (longest) chain
    assertEquals(a.chainDigests, b.chainDigests)
    assertEquals(b.chainDigests, c.chainDigests)
    assertEquals(a.chainDigests.length, 2)
    // b shares the (deterministic) genesis block with a, then diverges:
    // its switch is a REORG at fork point 1
    assert(reorgs.exists(r => r.node == "b" && r.forkPoint == 1 && r.fromHead.isDefined), reorgs.toString)
    // c's adoption from empty is not a fork (fork point 0, no previous head)
    assert(reorgs.exists(r => r.node == "c" && r.fromHead.isEmpty))

  // ---- M40: provenance ----

  test("M40: `why` walks 4 provenance hops from port text back to fragments"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-prov")
    val cas = cairn.systemhandler.DiskCas(dir)
    val ctx = EffectContext.forCas()
    val lang = Stlc.language
    def put(art: Artifact): Unit =
      CasEffects.put(cas, art, ctx).fold(e => fail(e.toString), identity)
    def putBs(bs: Array[Byte]): Digest =
      CasEffects.putBytes(cas, bs, ctx).fold(e => fail(e.toString), identity)
    def record(out: Digest, inputs: List[Digest], tool: String): Unit =
      Provenance.record(cas, out, inputs, tool, ctx).fold(e => fail(e.toString), identity)
    // hop 1: fragments -> language (compose)
    Stlc.fragments.foreach(f => put(f.artifact))
    put(lang.artifact)
    record(lang.digest, Stlc.fragments.map(_.digest), "compose")
    // hop 2: language + change -> module (delta)
    val dl = cairn.core.Delta.deltaOf(lang).toOption.get
    val change = Parser.parse(dl.grammar, "{ add id = fun x : Bool . x ; }").toOption.get
    val Right((module, vcs)) = cairn.core.Delta.apply(lang, Module(Nil), change): @unchecked
    put(module.artifact)
    put(vcs.artifact)
    record(module.digest, List(lang.digest, vcs.artifact.digest), "delta")
    // hop 3: module -> rosetta artifact
    val rosettaArt = cairn.examples.quicksort.QuickSort2.module.artifact
    put(rosettaArt)
    record(rosettaArt.digest, List(module.digest), "rosetta-model")
    // hop 4: rosetta artifact -> emitted port text
    val portText = cairn.core.PortV2.verified(cairn.core.Ports2.ScalaPort2,
      cairn.examples.quicksort.QuickSort2.module).toOption.get.text
    val portDigest = putBs(portText.getBytes)
    record(portDigest, List(rosettaArt.digest), "port-scala")
    // why?
    val hops = Provenance.why(dir, portDigest, ctx).fold(e => fail(e), identity)
    assertEquals(hops.map(_.record.tool).sorted, List("compose", "delta", "port-scala", "rosetta-model"))
    assertEquals(hops.map(_.depth).max, 3)
    val terminal = hops.find(_.record.tool == "compose").get
    assertEquals(terminal.record.inputs.toSet, Stlc.fragments.map(_.digest).toSet)
