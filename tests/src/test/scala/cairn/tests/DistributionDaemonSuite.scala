package cairn.tests

import cairn.kernel.*
import cairn.systemhandler.*

/** Peer discovery, HTTP gossip daemon, and BFT finality certificates. */
class DistributionDaemonSuite extends munit.FunSuite:
  private val ledgerCtx = EffectContext.forLedger()

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
    val bftAuth = replicas.map(k => k.name -> k.publicBytes).toMap
    val setDig = BftFinality.replicaSetDigest(replicas.map(k => k.name -> k.publicBytes).toMap)
    assertEquals(BftFinality.FinalityCertificate.verify(cert, bftAuth, setDig), Right(()))

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
    val bftAuth = replicas.map(k => k.name -> k.publicBytes).toMap
    val setDig = BftFinality.replicaSetDigest(replicas.map(k => k.name -> k.publicBytes).toMap)
    assert(BftFinality.FinalityCertificate.verify(duped, bftAuth, setDig).isLeft)

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
    val bftAuth = replicas.map(k => k.name -> k.publicBytes).toMap
    val setDig = BftFinality.replicaSetDigest(replicas.map(k => k.name -> k.publicBytes).toMap)
    assert(BftFinality.FinalityCertificate.verify(thin, bftAuth, setDig).isLeft)

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

  test("four HttpNode BftReplicas exchange messages and mint a network certificate"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val bftAuth = replicas.map(k => k.name -> k.publicBytes).toMap
    val ids = replicas.map(_.name)
    val setDig = BftFinality.replicaSetDigest(bftAuth)
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-bft-$id")).toMap
    // Identical sealed tip on every replica node
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
        val bft = BftReplica(
          replicas.find(_.name == id).get,
          bftAuth,
          ids,
          node = Some(nodes(id)),
          ledgerAuth = ledgerAuth,
          certStore = Some(peersRoot.resolve("bft-certs.canon")))
        val http = HttpNode(nodes(id), ledgerAuth, peersRoot = Some(peersRoot), bft = Some(bft))
        https += http
        id -> http.start()
      }.toMap
      // Wire replica peer directories (each sees the other three + self)
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
      assert(cert.commits.map(_._1.id).distinct.length >= BftQuorum.quorumSize(4))
      assertEquals(BftFinality.FinalityCertificate.verify(cert, bftAuth, setDig), Right(()))
    finally https.foreach(_.stop())

  test("multi-home provisioning: distinct keys + replica-set.canon + remote propose"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val ids = List("r0", "r1", "r2", "r3")
    // Provision each replica's private key only under its own home.
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-mp-$id")).toMap
    val kps = ids.map { id =>
      Keypair.loadOrCreate(homes(id), id).fold(e => fail(e), identity)
    }
    val unsigned = ReplicaSetManifest.of(kps.map(k => k.name -> k.publicBytes))
      .fold(e => fail(e), identity)
    val manifest = ReplicaSetManifest.seal(
      unsigned,
      kps.map(k => k.name -> ((msg: Array[Byte]) => Ed25519.sign(k.privateKey, msg))))
      .fold(e => fail(e), identity)
    ReplicaSetManifest.verifySeals(manifest, Ed25519.verify).fold(e => fail(e), identity)
    // Every home gets the same certified public-key set (no remote private keys).
    ids.foreach { id =>
      BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(homes(id)), manifest)
        .fold(e => fail(e), identity)
      // Ensure Keypair.load for a foreign name fails on this home
      val foreign = ids.find(_ != id).get
      assert(Keypair.load(homes(id), foreign).isLeft, s"$id should not hold $foreign's key")
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
        val kp = Keypair.load(home, id).fold(e => fail(e), identity)
        val bft = BftReplica(
          kp, manifest.authorities, manifest.ids,
          node = Some(nodes(id)), ledgerAuth = ledgerAuth,
          certStore = Some(home.resolve("bft-certs.canon")),
          stateStore = Some(home.resolve("bft-state.canon")))
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
      // Initiator has no primary private key — uses /bft/propose
      val cert = BftFinality.agreeNetworkRemote(urls, 0, 0, block, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert.blockDigest, block)
      assertEquals(
        BftFinality.FinalityCertificate.verify(cert, manifest.authorities, manifest.replicaSetDigest),
        Right(()))
      // State files persisted
      assert(java.nio.file.Files.exists(homes("r0").resolve("bft-state.canon")))
    finally https.foreach(_.stop())

  test("replica-set digest binds public keys, not names alone"):
    val a = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val b = List("r0", "r1", "r2", "r3").map(id => Keypair.dev(s"$id-alt").copy(name = id))
    // Same ids, different key material → different digests
    val digA = BftFinality.replicaSetDigest(a.map(k => k.name -> k.publicBytes).toMap)
    val digB = BftFinality.replicaSetDigest(b.map(k => k.name -> k.publicBytes).toMap)
    assert(digA != digB)
    val namesOnly = BftFinality.replicaSetDigest(a.map(_.name))
    assert(digA != namesOnly)
    val mA = ReplicaSetManifest.of(a.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    val mB = ReplicaSetManifest.of(b.map(k => k.name -> k.publicBytes)).fold(e => fail(e), identity)
    assertEquals(mA.replicaSetDigest, digA)
    assert(mA.replicaSetDigest != mB.replicaSetDigest)
    // Sealed artifact round-trips member seals
    val sealedM = ReplicaSetManifest.seal(
      mA,
      a.map(k => k.name -> ((msg: Array[Byte]) => Ed25519.sign(k.privateKey, msg))))
      .fold(e => fail(e), identity)
    assert(sealedM.seals.nonEmpty)
    val reloaded = ReplicaSetManifest.fromCanon(sealedM.canon).fold(e => fail(e), identity)
    assertEquals(reloaded.seals, sealedM.seals)
    ReplicaSetManifest.verifySeals(reloaded, Ed25519.verify).fold(e => fail(e), identity)

  test("corrupt bft-state.canon refuses further operate (fail closed)"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val home = java.nio.file.Files.createTempDirectory("cairn-bft-corrupt")
    val node = Node(home.resolve("node"), ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val bftAuth = replicas.map(k => k.name -> k.publicBytes).toMap
    val ids = replicas.map(_.name)
    val statePath = home.resolve("bft-state.canon")
    java.nio.file.Files.write(statePath, "not-valid-canon".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val bft = BftReplica(
      replicas.head, bftAuth, ids,
      node = Some(node), ledgerAuth = ledgerAuth,
      stateStore = Some(statePath))
    val block = node.chainDigests.head
    val err = bft.propose(0, 0, block)
    assert(err.isLeft, err.toString)
    assert(err.swap.toOption.exists(_.contains("refusing to operate")), err.toString)
