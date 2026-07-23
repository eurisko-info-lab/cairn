package cairn.tests

import cairn.kernel.*
import cairn.systemhandler.*

/** Peer discovery, HTTP gossip daemon, and BFT finality certificates. */
class DistributionDaemonSuite extends munit.FunSuite:
  private val ledgerCtx = EffectContext.forLedger()
  private val ksSecret = Some("cairn-test-secret".getBytes(java.nio.charset.StandardCharsets.UTF_8))

  test("PeerRegistry round-trips and merges by lastSeen"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-peers")
    PeerRegistry.add(dir, "a", "http://127.0.0.1:1").fold(e => fail(e), identity)
    PeerRegistry.add(dir, "b", "http://127.0.0.1:2", PeerRegistry.Role.Replica)
      .fold(e => fail(e), identity)
    val loaded = PeerRegistry.load(dir).fold(e => fail(e), identity)
    assertEquals(loaded.peers.size, 2)
    assertEquals(loaded.replicas.map(_.name), List("b"))
    val remote = PeerRegistry.Directory(List(
      PeerRegistry.Peer("a", "http://127.0.0.1:99", lastSeenEpochMs = System.currentTimeMillis() + 1000)))
    val merged = PeerRegistry.merge(loaded, remote)
    assertEquals(merged.byName("a").map(_.baseUrl), Some("http://127.0.0.1:99"))

  test("HttpGossip: longer remote chain is adopted over HTTP"):
    val auth = Keypair.dev("auth")
    val authorities = Map(auth.name -> auth.publicBytes)
    val aRoot = java.nio.file.Files.createTempDirectory("cairn-ga")
    val bRoot = java.nio.file.Files.createTempDirectory("cairn-gb")
    val a = Node(aRoot, ledgerCtx)
    val b = Node(bRoot, ledgerCtx)
    a.append(auth, authorities, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    assertEquals(a.chainDigests.length, 1)
    assertEquals(b.chainDigests.length, 0)
    val peersRoot = java.nio.file.Files.createTempDirectory("cairn-gp")
    val http = HttpNode(a, authorities, peersRoot = Some(peersRoot))
    val port = http.start()
    try
      PeerRegistry.add(peersRoot, "a", s"http://127.0.0.1:$port")
        .fold(e => fail(e), identity)
      val report = HttpGossip.round("b", b, PeerRegistry.load(peersRoot).fold(e => fail(e), _.gossipPeers), authorities)
      assert(report.errors.isEmpty, report.errors.toString)
      assertEquals(report.pulled, List("a"))
      assertEquals(b.chainDigests.length, 1)
      assertEquals(b.chainDigests, a.chainDigests)
    finally http.stop()

  test("GossipDaemon tick pulls from registered peers"):
    val auth = Keypair.dev("auth")
    val authorities = Map(auth.name -> auth.publicBytes)
    val aRoot = java.nio.file.Files.createTempDirectory("cairn-da")
    val bRoot = java.nio.file.Files.createTempDirectory("cairn-db")
    val peersRoot = java.nio.file.Files.createTempDirectory("cairn-dp")
    val a = Node(aRoot, ledgerCtx)
    val b = Node(bRoot, ledgerCtx)
    a.append(auth, authorities, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val http = HttpNode(a, authorities, peersRoot = Some(peersRoot))
    val port = http.start()
    try
      PeerRegistry.add(peersRoot, "a", s"http://127.0.0.1:$port").fold(e => fail(e), identity)
      val daemon = GossipDaemon("b", b, peersRoot, authorities, intervalMs = 10)
      val r = daemon.tick()
      daemon.stop()
      assert(r.errors.isEmpty, r.errors.toString)
      assertEquals(b.chainDigests, a.chainDigests)
    finally http.stop()

  test("HttpNode GET /peers serves the registry"):
    val auth = Keypair.dev("auth")
    val authorities = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-disc")
    val node = Node(root.resolve("n"), ledgerCtx)
    PeerRegistry.add(root, "self", "http://example").fold(e => fail(e), identity)
    val http = HttpNode(node, authorities, peersRoot = Some(root))
    val port = http.start()
    try
      val client = java.net.http.HttpClient.newHttpClient()
      val resp = client.send(
        java.net.http.HttpRequest.newBuilder(java.net.URI.create(s"http://127.0.0.1:$port/peers")).GET().build(),
        java.net.http.HttpResponse.BodyHandlers.ofByteArray())
      assertEquals(resp.statusCode(), 200)
      val dir = Canon.decode(resp.body()).flatMap(PeerRegistry.Directory.fromCanon).fold(e => fail(e), identity)
      assert(dir.byName("self").isDefined)
    finally http.stop()

  test("BftFinality.agreeForSealedBlock mints a verifiable 2f+1 certificate"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-bft-local")
    val node = Node(root, ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val block = node.chainDigests.head
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val cert = BftFinality.agreeForSealedBlock(node, ledgerAuth, replicas, 0, 1, block)
      .fold(e => fail(e), identity)
    assertEquals(cert.blockDigest, block)
    assert(cert.commits.size >= BftQuorum.quorumSize(4))
    assertEquals(cert.commits.map(_._1.id).distinct.length, cert.commits.length)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assertEquals(BftFinality.FinalityCertificate.verifyAgainstChain(cert, manifest, node, ledgerAuth), Right(()))

  test("BftFinality certificate rejects duplicate replica commits"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-bft-dup")
    val node = Node(root, ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val block = node.chainDigests.head
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val cert = BftFinality.agreeForSealedBlock(node, ledgerAuth, replicas, 0, 0, block)
      .fold(e => fail(e), identity)
    val (id0, seal0) = cert.commits.head
    val duped = cert.copy(commits = List.fill(3)((id0, seal0)))
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assert(BftFinality.FinalityCertificate.verify(duped, manifest).isLeft)

  test("BftFinality certificate rejects under-quorum commits"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-bft-thin")
    val node = Node(root, ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val block = node.chainDigests.head
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val cert = BftFinality.agreeForSealedBlock(node, ledgerAuth, replicas, 0, 0, block)
      .fold(e => fail(e), identity)
    val thin = cert.copy(commits = cert.commits.take(1))
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assert(BftFinality.FinalityCertificate.verify(thin, manifest).isLeft)

  test("BftFinality rejects agreeing over a non-chain digest"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-bft-fake")
    val node = Node(root, ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val fake = Digest.of(Canon.CStr("not-a-block"))
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    assert(BftFinality.agreeForSealedBlock(node, ledgerAuth, replicas, 0, 0, fake).isLeft)

  test("BftFinality certificate rejects forged height/parent vs chain"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-bft-height")
    val node = Node(root, ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val block = node.chainDigests.head
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val cert = BftFinality.agreeForSealedBlock(node, ledgerAuth, replicas, 0, 0, block)
      .fold(e => fail(e), identity)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    assert(BftFinality.FinalityCertificate.verify(cert, manifest).isRight)
    val forgedH = cert.copy(height = cert.height + 99)
    assert(BftFinality.FinalityCertificate.verifyAgainstChain(forgedH, manifest, node, ledgerAuth).isLeft)
    val forgedP = cert.copy(parent = Digest.of(Canon.CStr("fake-parent")))
    assert(BftFinality.FinalityCertificate.verifyAgainstChain(forgedP, manifest, node, ledgerAuth).isLeft)

  test("four HttpNode BftReplicas exchange messages and mint a network certificate"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val bftAuth = manifest.authorities
    val ids = manifest.ids
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-bft-$id")).toMap
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), ledgerCtx)
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      id -> n
    }
    val block = nodes("r0").chainDigests.head
    assert(nodes.values.forall(_.chainDigests == List(block)))
    val https = scala.collection.mutable.ListBuffer.empty[HttpNode]
    try
      val ports = ids.map { id =>
        val peersRoot = homes(id)
        val bft = BftReplica.certified(
          replicas.find(_.name == id).get, manifest,
          node = Some(nodes(id)), ledgerAuth = ledgerAuth,
          certStore = Some(peersRoot.resolve("bft-certs.canon")),
          stateStore = Some(peersRoot.resolve("bft-state.canon")))
          .fold(e => fail(e), identity)
        val http = HttpNode(nodes(id), ledgerAuth, peersRoot = Some(peersRoot), bft = Some(bft))
        https += http
        id -> http.start()
      }.toMap
      ids.foreach { id =>
        ids.foreach { peer =>
          PeerRegistry.add(
            homes(id), peer, s"http://127.0.0.1:${ports(peer)}",
            PeerRegistry.Role.Replica,
            publicKey = Some(bftAuth(peer))).fold(e => fail(e), identity)
        }
      }
      val primaryId = BftFinality.designatedPrimary(ids, view = 0).fold(e => fail(e), identity)
      val primary = replicas.find(_.name == primaryId.id).get
      val urls = ids.map(id => id -> s"http://127.0.0.1:${ports(id)}").toMap
      val cert = BftFinality.agreeNetwork(primary, urls, view = 0, seq = 0, block, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert.blockDigest, block)
      assertEquals(cert.seq, 0)
      assert(cert.commits.map(_._1.id).distinct.length >= BftQuorum.quorumSize(4))
      assertEquals(
        BftFinality.FinalityCertificate.verifyAgainstChain(cert, manifest, nodes("r0"), ledgerAuth),
        Right(()))
      // Continuous finality: second block on the same running durable replicas.
      ids.foreach { id =>
        nodes(id).append(
          auth, ledgerAuth,
          List(auth.signTx(Tx.RegisterIdentity("extra", auth.publicBytes))))
          .fold(e => fail(e), identity)
      }
      val block1 = nodes("r0").chainDigests(1)
      assert(nodes.values.forall(_.chainDigests == nodes("r0").chainDigests))
      val cert1 = BftFinality.agreeNetworkRemote(urls, block1, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert1.blockDigest, block1)
      assertEquals(cert1.seq, 1)
      assertEquals(cert1.height, 1L)
      assertEquals(
        BftFinality.FinalityCertificate.verifyAgainstChain(cert1, manifest, nodes("r0"), ledgerAuth),
        Right(()))
    finally https.foreach(_.stop())

  test("multi-home provisioning: distinct keys + replica-set.canon + remote propose"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val ids = List("r0", "r1", "r2", "r3")
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-mp-$id")).toMap
    val kps = ids.map { id =>
      Keystore.loadOrCreate(homes(id), id, ksSecret).fold(e => fail(e), identity)
    }
    val manifest = BftFinality.sealReplicaSet(kps).fold(e => fail(e), identity)
    ids.foreach { id =>
      BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(homes(id)), manifest)
        .fold(e => fail(e), identity)
      val foreign = ids.find(_ != id).get
      assert(Keystore.load(homes(id), foreign, ksSecret).isLeft, s"$id should not hold $foreign's key")
    }
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), ledgerCtx)
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      id -> n
    }
    val block = nodes("r0").chainDigests.head
    val https = scala.collection.mutable.ListBuffer.empty[HttpNode]
    try
      val ports = ids.map { id =>
        val home = homes(id)
        val kp = Keystore.load(home, id, ksSecret).fold(e => fail(e), identity)
        val bft = BftReplica.certified(
          kp, manifest,
          node = Some(nodes(id)), ledgerAuth = ledgerAuth,
          certStore = Some(home.resolve("bft-certs.canon")),
          stateStore = Some(home.resolve("bft-state.canon")))
          .fold(e => fail(e), identity)
        val http = HttpNode(nodes(id), ledgerAuth, peersRoot = Some(home), bft = Some(bft))
        https += http
        id -> http.start()
      }.toMap
      ids.foreach { id =>
        ids.foreach { peer =>
          PeerRegistry.add(
            homes(id), peer, s"http://127.0.0.1:${ports(peer)}",
            PeerRegistry.Role.Replica,
            publicKey = Some(manifest.authorities(peer))).fold(e => fail(e), identity)
        }
      }
      val urls = ids.map(id => id -> s"http://127.0.0.1:${ports(id)}").toMap
      val cert = BftFinality.agreeNetworkRemote(urls, block, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert.blockDigest, block)
      assertEquals(cert.seq, 0)
      assertEquals(
        BftFinality.FinalityCertificate.verifyAgainstChain(cert, manifest, nodes("r0"), ledgerAuth),
        Right(()))
      assert(java.nio.file.Files.exists(homes("r0").resolve("bft-state.canon")))
    finally https.foreach(_.stop())

  test("replica-set digest binds public keys and transition metadata"):
    val a = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val b = List("r0", "r1", "r2", "r3").map(id => Keypair.dev(s"$id-alt").copy(name = id))
    val mA = ReplicaSetManifest.of(a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val mB = ReplicaSetManifest.of(b.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    assert(mA.replicaSetDigest != mB.replicaSetDigest)
    val mapOnly = BftFinality.replicaSetDigest(a.map(k => k.name -> k.publicBytes).toMap)
    assert(mA.replicaSetDigest != mapOnly)
    val draft = ReplicaSetManifest.of(
      a.map(k => k.name -> k.publicBytes),
      replaces = Some(mA.digest),
      activationHeight = 10L).fold(e => fail(e), identity)
    assert(draft.replicaSetDigest != mA.replicaSetDigest)
    // Without predecessor quorum approvals, amendment is refused.
    assert(ReplicaSetManifest.allowsTransition(draft, Some(mA), Some(mA.digest), Ed25519.verify).isLeft)
    val payload = Canon.encode(draft.bodyCanon)
    val approvals = a.take(3).map(k => k.name -> k.sign(payload)) // quorum of 4 is 3
    val amended = ReplicaSetManifest.withPredecessorApprovals(draft, approvals)
      .fold(e => fail(e), identity)
    assertEquals(
      ReplicaSetManifest.allowsTransition(amended, Some(mA), Some(mA.digest), Ed25519.verify),
      Right(()))
    assert(ReplicaSetManifest.allowsTransition(
      amended.copy(activationHeight = 0), Some(mA), Some(mA.digest), Ed25519.verify).isLeft)
    // Disjoint membership still needs old-set quorum (not new-set only).
    val disjointDraft = ReplicaSetManifest.of(
      b.map(k => k.name -> k.publicBytes),
      replaces = Some(mA.digest),
      activationHeight = 10L).fold(e => fail(e), identity)
    val badApprovals = b.take(3).map(k => k.name -> k.sign(Canon.encode(disjointDraft.bodyCanon)))
    val disjoint = ReplicaSetManifest.withPredecessorApprovals(disjointDraft, badApprovals)
      .fold(e => fail(e), identity)
    assert(
      ReplicaSetManifest.allowsTransition(disjoint, Some(mA), Some(mA.digest), Ed25519.verify).isLeft,
      "new-set seals must not count as predecessorApprovals")
    val sealedM = BftFinality.sealReplicaSet(a).fold(e => fail(e), identity)
    val reloaded = ReplicaSetManifest.fromCanon(sealedM.canon).fold(e => fail(e), identity)
    assertEquals(reloaded.seals, sealedM.seals)
    ReplicaSetManifest.verifySeals(reloaded, Ed25519.verify).fold(e => fail(e), identity)
    assertEquals(ReplicaSetManifest.activeAt(List(sealedM), 0), Right(sealedM))
    val validated = ValidatedReplicaSetHistory.verify(List(sealedM), Ed25519.verify)
      .fold(e => fail(e), identity)
    assertEquals(validated.activeAt(0), Right(sealedM))

  test("corrupt bft-state.canon refuses further operate (fail closed)"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val home = java.nio.file.Files.createTempDirectory("cairn-bft-corrupt")
    val node = Node(home.resolve("node"), ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val statePath = home.resolve("bft-state.canon")
    java.nio.file.Files.write(statePath, "not-valid-canon".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val bft = BftReplica.certified(
      replicas.head, manifest,
      node = Some(node), ledgerAuth = ledgerAuth,
      stateStore = Some(statePath)).fold(e => fail(e), identity)
    val block = node.chainDigests.head
    val err = bft.propose(0, 0, block)
    assert(err.isLeft, err.toString)
    assert(err.swap.toOption.exists(_.contains("durable I/O failure")), err.toString)

  test("Keystore encrypts private keys at rest when a secret is supplied"):
    val kp = Keypair.dev("enc-r0")
    val secret = Some("test-keystore-secret".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val sealedC = Keystore.toCanon(kp, secret)
    assert(sealedC match
      case Canon.CTag("keypair-sealed", _) => true
      case _ => false)
    val loaded = Keystore.fromCanon(sealedC, secret).fold(e => fail(e), identity)
    assertEquals(loaded.publicBytes, kp.publicBytes)
    assertEquals(loaded.sign("ping".getBytes), kp.sign("ping".getBytes))
    assert(Keystore.fromCanon(sealedC, None).isLeft)
    val home = java.nio.file.Files.createTempDirectory("cairn-ks")
    Keystore.saveCreate(home, kp, ksSecret).fold(e => fail(e), identity)
    val round = Keystore.load(home, "enc-r0", ksSecret).fold(e => fail(e), identity)
    assertEquals(round.sign("pong".getBytes), kp.sign("pong".getBytes))

  test("Keystore never overwrites on wrong/missing secret and refuses plaintext by default"):
    val home = java.nio.file.Files.createTempDirectory("cairn-ks-nooverwrite")
    val kp = Keypair.dev("r0")
    Keystore.saveCreate(home, kp, ksSecret).fold(e => fail(e), identity)
    val before = java.nio.file.Files.readAllBytes(Keystore.path(home, "r0")).toVector
    // Wrong secret: load fails and loadOrCreate must not replace the file.
    val wrong = Some("wrong-secret".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    assert(Keystore.load(home, "r0", wrong).isLeft)
    assert(Keystore.loadOrCreate(home, "r0", wrong).isLeft)
    val after = java.nio.file.Files.readAllBytes(Keystore.path(home, "r0")).toVector
    assertEquals(after, before)
    // Create-only: second save refused.
    assert(Keystore.saveCreate(home, kp, ksSecret).isLeft)
    // Plaintext refused without CAIRN_KEYSTORE_PLAINTEXT.
    assert(Keystore.toCanonE(Keypair.dev("x"), None).isLeft)

  test("DurableIo permission failure is surfaced (no silent success)"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-durable-perm")
    val target = dir.resolve("nested").resolve("state.canon")
    // Make parent read-only after creating it so createDirectories of nested fails... 
    // Instead: write then make directory non-writable and try consensus write.
    java.nio.file.Files.createDirectories(dir.resolve("nested"))
    DurableIo.writeConsensus(target, Array[Byte](1, 2, 3)).fold(e => fail(e), identity)
    dir.resolve("nested").toFile.setWritable(false)
    dir.toFile.setWritable(false)
    val err = DurableIo.writeConsensus(target, Array[Byte](4, 5, 6))
    // Restore perms for cleanup
    dir.toFile.setWritable(true)
    dir.resolve("nested").toFile.setWritable(true)
    assert(err.isLeft, err.toString)

  test("BftReplica fails closed when state write fails before exposing votes"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val home = java.nio.file.Files.createTempDirectory("cairn-bft-writefail")
    val node = Node(home.resolve("node"), ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val stateDir = home.resolve("statedir")
    java.nio.file.Files.createDirectories(stateDir)
    val statePath = stateDir.resolve("bft-state.canon")
    val bft = BftReplica.certified(
      replicas.head, manifest,
      node = Some(node), ledgerAuth = ledgerAuth,
      stateStore = Some(statePath)).fold(e => fail(e), identity)
    val block = node.chainDigests.head
    bft.propose(0, 0, block).fold(e => fail(e), identity)
    assert(java.nio.file.Files.exists(statePath))
    stateDir.toFile.setWritable(false)
    home.toFile.setWritable(false)
    val peer = replicas(1)
    val prep = BftFinality.sign(
      peer,
      cairn.kernel.BftQuorum.Msg.Prepare(
        0, 0, BftFinality.valueOfBlock(block).digest,
        cairn.kernel.BftQuorum.ReplicaId(peer.name)))
      .fold(e => fail(e), identity)
    val err = bft.receive(prep)
    home.toFile.setWritable(true)
    stateDir.toFile.setWritable(true)
    assert(err.isLeft, err.toString)
    assert(err.swap.toOption.exists(_.contains("durable")), err.toString)
    assert(bft.propose(0, 1, block).isLeft)

  test("pre-activation and post-deactivation refuse operate"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val home = java.nio.file.Files.createTempDirectory("cairn-bft-activation")
    val node = Node(home.resolve("node"), ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val future = BftFinality.sealReplicaSet(replicas, activationHeight = 5L).fold(e => fail(e), identity)
    val pre = BftReplica.certified(
      replicas.head, future,
      node = Some(node), ledgerAuth = ledgerAuth).fold(e => fail(e), identity)
    val block0 = node.chainDigests.head
    val preErr = pre.propose(0, 0, block0)
    assert(preErr.isLeft, preErr.toString)
    assert(preErr.swap.toOption.exists(_.contains("not yet active")), preErr.toString)

    val genesis = BftFinality.sealReplicaSet(replicas, activationHeight = 0L).fold(e => fail(e), identity)
    BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(home), genesis).fold(e => fail(e), identity)
    val draft = ReplicaSetManifest.of(
      replicas.map(k => k.name -> k.publicBytes),
      replaces = Some(genesis.digest),
      activationHeight = 1L).fold(e => fail(e), identity)
    val payload = Canon.encode(draft.bodyCanon)
    val approvals = replicas.take(3).map(k => k.name -> k.sign(payload))
    val amendedBody = ReplicaSetManifest.withPredecessorApprovals(draft, approvals)
      .fold(e => fail(e), identity)
    val successor = ReplicaSetManifest.seal(
      amendedBody,
      replicas.map(k => k.name -> ((msg: Array[Byte]) => k.sign(msg))))
      .fold(e => fail(e), identity)
    BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(home), successor)
      .fold(e => fail(e), identity)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity("extra", auth.publicBytes))))
      .fold(e => fail(e), identity) // tip height becomes 1
    val hist = BftFinality.loadReplicaSetHistory(home).fold(e => fail(e), identity)
    assertEquals(hist.manifests.length, 2)
    assertEquals(hist.activeAt(0).map(_.digest), Right(genesis.digest))
    assertEquals(hist.activeAt(1).map(_.digest), Right(successor.digest))
    val post = BftReplica.certified(
      replicas.head, genesis,
      node = Some(node), ledgerAuth = ledgerAuth,
      home = Some(home)).fold(e => fail(e), identity)
    val block1 = node.chainDigests(1)
    // Old set may still finalize pre-deactivation heights, but not post-activation ones.
    val postErr = post.propose(0, 1, block1)
    assert(postErr.isLeft, postErr.toString)
    assert(postErr.swap.toOption.exists(_.contains("deactivated")), postErr.toString)
    // Receive must not emit Commit seals after deactivation either.
    val primaryId = BftFinality.designatedPrimary(replicas.map(_.name), 0).fold(e => fail(e), identity)
    val primary = replicas.find(_.name == primaryId.id).get
    val pp = BftFinality.sign(
      primary,
      cairn.kernel.BftQuorum.Msg.PrePrepare(
        0, 1, BftFinality.valueOfBlock(block1), primaryId))
      .fold(e => fail(e), identity)
    val recvErr = post.receive(pp)
    assert(recvErr.isLeft, recvErr.toString)
    assert(recvErr.swap.toOption.exists(_.contains("deactivated")), recvErr.toString)
    assertEquals(post.drainOutbound(), Nil)
    // Independent verify rejects old-set certs at the successor height.
    val fakeCert = BftFinality.FinalityCertificate(
      block1, 0, 1, Nil, genesis.replicaSetDigest, 1L, block0)
    assert(BftFinality.FinalityCertificate.verifyAgainstHistory(
      fakeCert, hist, node, ledgerAuth).isLeft)

  test("forged replica-set history without predecessor quorum is rejected"):
    val home = java.nio.file.Files.createTempDirectory("cairn-bft-forged-hist")
    val a = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val genesis = BftFinality.sealReplicaSet(a).fold(e => fail(e), identity)
    BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(home), genesis)
      .fold(e => fail(e), identity)
    val forgedDraft = ReplicaSetManifest.of(
      a.map(k => k.name -> k.publicBytes),
      replaces = Some(genesis.digest),
      activationHeight = 1L).fold(e => fail(e), identity)
    // Member seals only — no predecessorApprovals.
    val forged = ReplicaSetManifest.seal(
      forgedDraft,
      a.map(k => k.name -> ((msg: Array[Byte]) => k.sign(msg)))).fold(e => fail(e), identity)
    val histPath = BftFinality.defaultReplicaSetHistoryPath(home)
    java.nio.file.Files.write(
      histPath,
      Canon.encode(Canon.CList(List(genesis.canon, forged.canon))))
    val loaded = BftFinality.loadReplicaSetHistory(home)
    assert(loaded.isLeft, loaded.toString)
    assert(loaded.swap.toOption.exists(_.contains("predecessor")), loaded.toString)
    assert(ValidatedReplicaSetHistory.verify(List(genesis, forged), Ed25519.verify).isLeft)
