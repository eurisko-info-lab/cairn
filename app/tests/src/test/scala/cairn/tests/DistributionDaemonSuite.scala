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
    val replica = Keypair.dev("b")
    PeerRegistry.addBound(dir, replica, "http://127.0.0.1:2", PeerRegistry.Role.Replica)
      .fold(e => fail(e), identity)
    val loaded = PeerRegistry.load(dir).fold(e => fail(e), identity)
    assertEquals(loaded.peers.size, 2)
    assertEquals(loaded.replicas.map(_.name), List("b"))
    val remote = PeerRegistry.Directory(List(
      PeerRegistry.Peer("a", "http://127.0.0.1:99", lastSeenEpochMs = System.currentTimeMillis() + 1000)))
    val merged = PeerRegistry.merge(loaded, remote)
    assertEquals(merged.byName("a").map(_.baseUrl), Some("http://127.0.0.1:99"))
    // Poisoned replica URL without a valid seal is rejected.
    val poison = PeerRegistry.Peer(
      "b", "http://evil.example:9", PeerRegistry.Role.Replica,
      publicKey = Some(replica.publicBytes),
      lastSeenEpochMs = System.currentTimeMillis() + 5000)
    val merged2 = PeerRegistry.merge(loaded, PeerRegistry.Directory(List(poison)))
    assertEquals(merged2.byName("b").map(_.baseUrl), Some("http://127.0.0.1:2"))

  test("PeerRegistry resolveReplicaUrls rejects a correctly sealed attacker key"):
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val attacker = Keypair.dev("r0")
    val peers = manifest.ids.map { id =>
      if id == "r0" then PeerRegistry.Peer.bound(attacker, "http://evil", PeerRegistry.Role.Replica)
      else
        PeerRegistry.Peer.bound(
          replicas.find(_.name == id).get, s"http://$id", PeerRegistry.Role.Replica)
    }
    val result = PeerRegistry.resolveReplicaUrls(PeerRegistry.Directory(peers), manifest)
    assert(result.isLeft, result.toString)
    assert(result.swap.toOption.exists(_.contains("public key does not match")), result.toString)

  test("Keystore uses PBKDF2 and still loads legacy SHA-256 seals"):
    val kp = Keypair.dev("kdf-r0")
    val secret = Some("test-keystore-secret".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val sealedC = Keystore.toCanon(kp, secret)
    sealedC match
      case Canon.CTag("keypair-sealed", m) =>
        assertEquals(m.field("kdf").asStr, "pbkdf2-hmac-sha256")
        assert(m.field("salt") match
          case Canon.CBytes(bs) => bs.nonEmpty
          case _ => false)
        assert(m.field("iterations").asInt >= 10000)
      case _ => fail(s"expected keypair-sealed, got $sealedC")
    val loaded = Keystore.fromCanon(sealedC, secret).fold(e => fail(e), identity)
    assertEquals(loaded.sign("ping".getBytes), kp.sign("ping".getBytes))
    // Legacy unsalted SHA-256 format (no kdf/salt fields).
    val legacyKey = java.security.MessageDigest.getInstance("SHA-256").digest(secret.get)
    val nonce = new Array[Byte](12)
    java.security.SecureRandom().nextBytes(nonce)
    val cipher = javax.crypto.Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(
      javax.crypto.Cipher.ENCRYPT_MODE,
      new javax.crypto.spec.SecretKeySpec(legacyKey, "AES"),
      new javax.crypto.spec.GCMParameterSpec(128, nonce))
    val ct = cipher.doFinal(kp.privateBytes.toArray)
    val legacy = Canon.CTag("keypair-sealed", Canon.cmap(
      "name" -> Canon.CStr(kp.name),
      "public" -> Canon.CBytes(kp.publicBytes),
      "privateNonce" -> Canon.CBytes(nonce.toVector),
      "privateCiphertext" -> Canon.CBytes(ct.toVector)))
    val legacyLoaded = Keystore.fromCanon(legacy, secret).fold(e => fail(e), identity)
    assertEquals(legacyLoaded.sign("pong".getBytes), kp.sign("pong".getBytes))

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

  test("GossipDaemon reports a corrupt checkpoint before pulling"):
    val auth = Keypair.dev("auth")
    val authorities = Map(auth.name -> auth.publicBytes)
    val remoteRoot = java.nio.file.Files.createTempDirectory("cairn-corrupt-remote")
    val localRoot = java.nio.file.Files.createTempDirectory("cairn-corrupt-local")
    val remote = Node(remoteRoot, ledgerCtx)
    val local = Node(localRoot, ledgerCtx)
    remote.append(auth, authorities, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val http = HttpNode(remote, authorities)
    val port = http.start()
    try
      PeerRegistry.add(localRoot, "remote", s"http://127.0.0.1:$port").fold(e => fail(e), identity)
      java.nio.file.Files.write(
        BftFinality.defaultCheckpointPath(localRoot), "not canonical".getBytes)
      val report = GossipDaemon("local", local, localRoot, authorities).tick()
      assert(report.errors.exists(_.contains("checkpoint corrupt")), report.errors.toString)
      assertEquals(local.chainDigests, Nil)
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
    assertEquals(cert.seq, 0) // derived from height, ignores caller seq=1
    assertEquals(cert.height, 0L)
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
    assertEquals(cert.chainId, block)
    val forgedChain = cert.copy(chainId = Digest.of(Canon.CStr("other-chain")))
    assert(BftFinality.FinalityCertificate.verifyAgainstChain(
      forgedChain, manifest, node, ledgerAuth).isLeft)

  test("ViewChange rejects prepared claims without prepare-quorum evidence"):
    import cairn.kernel.BftQuorum.*
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-bft-bare-pc")
    val node = Node(root, ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val block = node.chainDigests.head
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val bft = BftReplica.certified(
      replicas.head, manifest,
      node = Some(node), ledgerAuth = ledgerAuth).fold(e => fail(e), identity)
    val bare = PreparedCert(
      seq = 0,
      valueDigest = BftFinality.valueOfBlock(block).digest,
      preparedView = 0,
      value = Some(BftFinality.valueOfBlock(block)),
      prepareVotes = Nil)
    val vc = BftFinality.sign(
      replicas(1),
      Msg.ViewChange(1, List(bare), ReplicaId("r1")),
      manifest.replicaSetDigest,
      block).fold(e => fail(e), identity)
    val err = bft.receive(vc)
    assert(err.isLeft, err.toString)
    assert(
      err.swap.toOption.exists(e =>
        e.contains("prepare-quorum") || e.contains("PrePrepare evidence")),
      err.toString)

  test("four HttpNode BftReplicas exchange messages and mint a network certificate"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
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
          PeerRegistry.addBound(
            homes(id), replicas.find(_.name == peer).get,
            s"http://127.0.0.1:${ports(peer)}",
            PeerRegistry.Role.Replica).fold(e => fail(e), identity)
        }
      }
      val urls = ids.map(id => id -> s"http://127.0.0.1:${ports(id)}").toMap
      val cert = BftFinality.agreeNetworkRemote(
        urls, block, replicas.head, chainId = block, replicaSet = manifest.replicaSetDigest,
        polls = 64, pollSleepMs = 30)
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
      val cert1 = BftFinality.agreeNetworkRemote(
        urls, block1, replicas.head, chainId = block, replicaSet = manifest.replicaSetDigest,
        polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert1.blockDigest, block1)
      assertEquals(cert1.seq, 1)
      assertEquals(cert1.height, 1L)
      assertEquals(
        BftFinality.FinalityCertificate.verifyAgainstChain(cert1, manifest, nodes("r0"), ledgerAuth),
        Right(()))
      // Slot compaction: finalized seq 0 is dropped from durable state after mint.
      val stateBytes = java.nio.file.Files.readAllBytes(homes("r0").resolve("bft-state.canon"))
      val (st, seals, _) = Canon.decode(stateBytes).flatMap(BftFinality.decodeReplicaState)
        .fold(e => fail(e), identity)
      assert(!st.slots.keys.exists { case (_, seq) => seq <= 0 }, st.slots.keys.toString)
      assert(!seals.keys.exists { case (_, seq, _, _) => seq <= 0 }, seals.keys.toString)
    finally https.foreach(_.stop())

  test("HTTP view-change failover when primary is unreachable"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val ids = manifest.ids
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-vc-$id")).toMap
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), ledgerCtx)
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      id -> n
    }
    val block = nodes("r0").chainDigests.head
    val https = scala.collection.mutable.ListBuffer.empty[HttpNode]
    try
      // Start everyone except the view-0 primary (r0 is sorted first).
      val primary0 = BftFinality.designatedPrimary(ids, 0).fold(e => fail(e), identity).id
      assertEquals(primary0, "r0")
      val running = ids.filter(_ != primary0)
      val ports = running.map { id =>
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
      // Unreachable primary URL + live backups.
      val urls = ids.map { id =>
        id -> (if id == primary0 then "http://127.0.0.1:1" else s"http://127.0.0.1:${ports(id)}")
      }.toMap
      running.foreach { id =>
        ids.foreach { peer =>
          PeerRegistry.addBound(
            homes(id), replicas.find(_.name == peer).get,
            urls(peer), PeerRegistry.Role.Replica).fold(e => fail(e), identity)
        }
      }
      val initiator = replicas.find(_.name == "r1").get
      val cert = BftFinality.agreeNetworkRemote(
        urls, block, initiator, chainId = block, replicaSet = manifest.replicaSetDigest,
        polls = 80, pollSleepMs = 40, maxViews = 4)
        .fold(e => fail(e), identity)
      assertEquals(cert.blockDigest, block)
      assert(cert.view >= 1, clues(cert.view))
      assertEquals(
        BftFinality.FinalityCertificate.verifyAgainstChain(cert, manifest, nodes("r1"), ledgerAuth),
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
          PeerRegistry.addBound(
            homes(id), kps.find(_.name == peer).get,
            s"http://127.0.0.1:${ports(peer)}",
            PeerRegistry.Role.Replica).fold(e => fail(e), identity)
        }
      }
      val urls = ids.map(id => id -> s"http://127.0.0.1:${ports(id)}").toMap
      val cert = BftFinality.agreeNetworkRemote(
        urls, block, kps.head, chainId = block, replicaSet = manifest.replicaSetDigest,
        polls = 64, pollSleepMs = 30)
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
        cairn.kernel.BftQuorum.ReplicaId(peer.name)),
      manifest.replicaSetDigest,
      block)
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
    val block0 = node.chainDigests.head
    // Staged future set keeps itself before activation and refuses early heights.
    val pre = BftReplica.certified(
      replicas.head, successor,
      node = Some(node), ledgerAuth = ledgerAuth,
      home = Some(home)).fold(e => fail(e), identity)
    val preErr = pre.propose(0, 0, block0)
    assert(preErr.isLeft, preErr.toString)
    assert(preErr.swap.toOption.exists(_.contains("not yet active")), preErr.toString)
    // Height-driven adoption: same history mtime, tip crosses activation → adopt successor.
    val live = BftReplica.certified(
      replicas.head, genesis,
      node = Some(node), ledgerAuth = ledgerAuth,
      home = Some(home),
      stateStore = Some(home.resolve("bft-state.canon"))).fold(e => fail(e), identity)
    live.propose(0, 0, block0).fold(e => fail(e), identity)
    assertEquals(live.setDigest, genesis.replicaSetDigest)
    val histPath = BftFinality.defaultReplicaSetHistoryPath(home)
    val frozenMtime = java.nio.file.Files.getLastModifiedTime(histPath)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity("extra", auth.publicBytes))))
      .fold(e => fail(e), identity) // tip height becomes 1
    java.nio.file.Files.setLastModifiedTime(histPath, frozenMtime)
    live.refreshHistory().fold(e => fail(e), identity)
    assertEquals(live.setDigest, successor.replicaSetDigest)
    val hist = BftFinality.loadReplicaSetHistory(home).fold(e => fail(e), identity)
    assertEquals(hist.manifests.length, 2)
    assertEquals(hist.activeAt(0).map(_.digest), Right(genesis.digest))
    assertEquals(hist.activeAt(1).map(_.digest), Right(successor.digest))
    // Constructing with the predecessor at tip≥activation hot-reloads onto the successor.
    val post = BftReplica.certified(
      replicas.head, genesis,
      node = Some(node), ledgerAuth = ledgerAuth,
      home = Some(home)).fold(e => fail(e), identity)
    val block1 = node.chainDigests(1)
    post.refreshHistory().fold(e => fail(e), identity)
    assertEquals(post.setDigest, successor.replicaSetDigest)
    post.propose(0, 1, block1).fold(e => fail(e), identity)
    // Independent verify rejects old-set certs at the successor height.
    val genesisId = node.chainDigests.head
    val fakeCert = BftFinality.FinalityCertificate(
      block1, 0, 1, Nil, genesis.replicaSetDigest, 1L, block0, genesisId)
    assert(BftFinality.FinalityCertificate.verifyAgainstHistory(
      fakeCert, hist, node, ledgerAuth).isLeft)

  test("multi-home CLI ceremony: keygen → pubkey exchange → seal → commit → install"):
    val secret = ksSecret
    val ids = List("r0", "r1", "r2", "r3")
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-cer-$id")).toMap
    val exchange = java.nio.file.Files.createTempDirectory("cairn-cer-xchg")
    // 1. Each machine keygens only its own identity.
    ids.foreach { id =>
      BftCeremony.keygen(homes(id), id, secret).fold(e => fail(e), identity)
      assert(Keystore.load(homes(id), id, secret).isRight)
      ids.filter(_ != id).foreach { other =>
        assert(Keystore.load(homes(id), other, secret).isLeft)
      }
      val pkFile = exchange.resolve(s"$id.pubkey.canon")
      BftCeremony.exportPubkey(homes(id), id, pkFile, secret).fold(e => fail(e), identity)
    }
    // 2. Coordinator (r0) imports every pubkey and assembles genesis draft.
    val coord = homes("r0")
    ids.foreach { id =>
      BftCeremony.importPubkey(coord, exchange.resolve(s"$id.pubkey.canon"))
        .fold(e => fail(e), identity)
    }
    val draft = BftCeremony.assemble(coord, ids).fold(e => fail(e), identity)
    assertEquals(draft.n, 4)
    assertEquals(draft.replaces, None)
    val draftFile = exchange.resolve("draft.canon")
    BftCeremony.exportDraft(coord, draftFile).fold(e => fail(e), identity)
    // 3. Each member imports draft and seals with its local key only.
    ids.foreach { id =>
      BftCeremony.importDraft(homes(id), draftFile).fold(e => fail(e), identity)
      val sealFile = exchange.resolve(s"$id.seal.canon")
      val sealedPath = BftCeremony.sealMember(homes(id), id, secret).fold(e => fail(e), identity)
      java.nio.file.Files.copy(
        sealedPath, sealFile,
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    }
    // 4. Coordinator gathers seals and commits.
    ids.foreach { id =>
      BftCeremony.importSeal(coord, exchange.resolve(s"$id.seal.canon"))
        .fold(e => fail(e), identity)
    }
    // Draft must be present on coordinator for commit (re-import after member importDraft
    // cleared only local contributions — coordinator still has its draft + imported seals).
    val tip = BftCeremony.commit(coord).fold(e => fail(e), identity)
    ReplicaSetManifest.verifySeals(tip, Ed25519.verify).fold(e => fail(e), identity)
    // 5. Export bundle and install on every other home.
    val bundle = exchange.resolve("replica-set.bundle.canon")
    BftCeremony.exportBundle(coord, bundle).fold(e => fail(e), identity)
    ids.filter(_ != "r0").foreach { id =>
      BftCeremony.installBundle(homes(id), bundle).fold(e => fail(e), identity)
      val loaded = BftFinality.loadReplicaSet(BftFinality.defaultReplicaSetPath(homes(id)))
        .fold(e => fail(e), identity)
      assertEquals(loaded.digest, tip.digest)
      val hist = BftFinality.loadReplicaSetHistory(homes(id)).fold(e => fail(e), identity)
      assertEquals(hist.manifests.map(_.digest), List(tip.digest))
    }

  test("multi-home CLI ceremony amendment: approve + seal + commit"):
    val secret = ksSecret
    val ids = List("r0", "r1", "r2", "r3")
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-am-$id")).toMap
    val exchange = java.nio.file.Files.createTempDirectory("cairn-am-xchg")
    ids.foreach { id =>
      BftCeremony.keygen(homes(id), id, secret).fold(e => fail(e), identity)
      BftCeremony.exportPubkey(homes(id), id, exchange.resolve(s"$id.pubkey.canon"), secret)
        .fold(e => fail(e), identity)
    }
    val coord = homes("r0")
    ids.foreach(id =>
      BftCeremony.importPubkey(coord, exchange.resolve(s"$id.pubkey.canon")).fold(e => fail(e), identity))
    BftCeremony.assemble(coord, ids).fold(e => fail(e), identity)
    val draft0 = exchange.resolve("draft0.canon")
    BftCeremony.exportDraft(coord, draft0).fold(e => fail(e), identity)
    ids.foreach { id =>
      BftCeremony.importDraft(homes(id), draft0).fold(e => fail(e), identity)
      BftCeremony.sealMember(homes(id), id, secret).fold(e => fail(e), identity)
      java.nio.file.Files.copy(
        BftCeremony.sealPath(homes(id), id),
        exchange.resolve(s"$id.seal0.canon"),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    }
    // Re-import draft on coordinator (members' importDraft is local-only) and seals.
    BftCeremony.importDraft(coord, draft0).fold(e => fail(e), identity)
    ids.foreach(id =>
      BftCeremony.importSeal(coord, exchange.resolve(s"$id.seal0.canon")).fold(e => fail(e), identity))
    val genesis = BftCeremony.commit(coord).fold(e => fail(e), identity)
    val bundle0 = exchange.resolve("bundle0.canon")
    BftCeremony.exportBundle(coord, bundle0).fold(e => fail(e), identity)
    ids.foreach { id =>
      if id != "r0" then
        BftCeremony.installBundle(homes(id), bundle0).fold(e => fail(e), identity)
    }
    // Amend at activation height 5 with same membership.
    ids.foreach(id =>
      BftCeremony.importPubkey(coord, exchange.resolve(s"$id.pubkey.canon")).fold(e => fail(e), identity))
    val amendedDraft = BftCeremony.assemble(coord, ids, activationHeight = 5L)
      .fold(e => fail(e), identity)
    assertEquals(amendedDraft.replaces, Some(genesis.digest))
    val draft1 = exchange.resolve("draft1.canon")
    BftCeremony.exportDraft(coord, draft1).fold(e => fail(e), identity)
    // Predecessor quorum approvals (3 of 4) + all member seals.
    ids.take(3).foreach { id =>
      BftCeremony.importDraft(homes(id), draft1).fold(e => fail(e), identity)
      BftCeremony.approve(homes(id), id, secret).fold(e => fail(e), identity)
      java.nio.file.Files.copy(
        BftCeremony.approvalPath(homes(id), id),
        exchange.resolve(s"$id.appr.canon"),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    }
    ids.foreach { id =>
      BftCeremony.importDraft(homes(id), draft1).fold(e => fail(e), identity)
      BftCeremony.sealMember(homes(id), id, secret).fold(e => fail(e), identity)
      java.nio.file.Files.copy(
        BftCeremony.sealPath(homes(id), id),
        exchange.resolve(s"$id.seal1.canon"),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    }
    BftCeremony.importDraft(coord, draft1).fold(e => fail(e), identity)
    ids.take(3).foreach(id =>
      BftCeremony.importApproval(coord, exchange.resolve(s"$id.appr.canon")).fold(e => fail(e), identity))
    ids.foreach(id =>
      BftCeremony.importSeal(coord, exchange.resolve(s"$id.seal1.canon")).fold(e => fail(e), identity))
    val successor = BftCeremony.commit(coord).fold(e => fail(e), identity)
    assertEquals(successor.replaces, Some(genesis.digest))
    assertEquals(successor.activationHeight, 5L)
    val hist = BftFinality.loadReplicaSetHistory(coord).fold(e => fail(e), identity)
    assertEquals(hist.manifests.map(_.digest), List(genesis.digest, successor.digest))

  test("finalized checkpoint blocks gossip/pull from dropping certified blocks"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val homeA = java.nio.file.Files.createTempDirectory("cairn-cp-a")
    val homeB = java.nio.file.Files.createTempDirectory("cairn-cp-b")
    val a = Node(homeA.resolve("node"), ledgerCtx)
    val b = Node(homeB.resolve("node"), ledgerCtx)
    a.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    // Divergent longer chain under a different bootstrap authority.
    val bob = Keypair.dev("bob")
    val bobAuth = Map(bob.name -> bob.publicBytes)
    b.append(bob, bobAuth, List(bob.signTx(Tx.RegisterIdentity(bob.name, bob.publicBytes))))
      .fold(e => fail(e), identity)
    b.append(bob, bobAuth, List(bob.signTx(Tx.RegisterIdentity("carol", bob.publicBytes))))
      .fold(e => fail(e), identity)
    assert(b.chainDigests.length > a.chainDigests.length)
    assert(a.chainDigests.head != b.chainDigests.head)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val cert = BftFinality.agreeForSealedBlock(a, ledgerAuth, replicas, a.chainDigests.head)
      .fold(e => fail(e), identity)
    assertEquals(cert.seq.toLong, cert.height)
    BftFinality.advanceCheckpoint(homeA, cert).fold(e => fail(e), identity)
    val cp = BftFinality.loadCheckpoint(homeA).fold(e => fail(e), identity)
    assert(cp.isDefined)
    assert(BftFinality.requireExtendsCheckpoint(b.chainDigests, cp).isLeft)
    assert(!HttpGossip.shouldAdopt(a.chainDigests, b.chainDigests, cp))
    assert(Sync.pull(b, a, ledgerAuth, cp).isLeft)
    // Same-prefix extension still allowed.
    Sync.pull(a, b, ledgerAuth, None).fold(e => fail(e), identity)
    assertEquals(b.chainDigests, a.chainDigests)
    b.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity("extra", auth.publicBytes))))
      .fold(e => fail(e), identity)
    assert(HttpGossip.shouldAdopt(a.chainDigests, b.chainDigests, cp))
    Sync.pull(b, a, ledgerAuth, cp).fold(e => fail(e), identity)
    assertEquals(a.chainDigests, b.chainDigests)

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

  test("NewView rejects forged and duplicated ViewChange evidence"):
    import cairn.kernel.BftQuorum.*
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val root = java.nio.file.Files.createTempDirectory("cairn-bft-nv-forge")
    val node = Node(root, ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val chainId = node.chainDigests.head
    val bft = BftReplica.certified(
      replicas.head, manifest,
      node = Some(node), ledgerAuth = ledgerAuth).fold(e => fail(e), identity)
    def signedVc(kp: Keypair): BftFinality.SignedMsg =
      BftFinality.sign(
        kp,
        Msg.ViewChange(1, Nil, ReplicaId(kp.name)),
        manifest.replicaSetDigest,
        chainId).fold(e => fail(e), identity)
    val vcs = replicas.take(3).map(signedVc)
    val evidence = vcs.map(sm =>
      ViewChangeEvidence(
        sm.msg.asInstanceOf[Msg.ViewChange], sm.seal, sm.replicaSet, sm.chainId))
    val primary1 = BftFinality.designatedPrimary(replicas.map(_.name), 1)
      .fold(e => fail(e), identity)
    val primaryKp = replicas.find(_.name == primary1.id).get
    // Duplicate sender.
    val dup = Msg.NewView(1, Nil, primary1, evidence :+ evidence.head)
    val dupSm = BftFinality.sign(
      primaryKp, dup, manifest.replicaSetDigest, chainId).fold(e => fail(e), identity)
    val dupErr = bft.receive(dupSm)
    assert(dupErr.isLeft, dupErr.toString)
    assert(dupErr.swap.toOption.exists(_.contains("duplicate")), dupErr.toString)
    // Unsigned / empty seal evidence.
    val forged = Msg.NewView(
      1, Nil, primary1,
      evidence.map(_.copy(seal = Vector.empty)))
    val forgedSm = BftFinality.sign(
      primaryKp, forged, manifest.replicaSetDigest, chainId).fold(e => fail(e), identity)
    val forgedErr = bft.receive(forgedSm)
    assert(forgedErr.isLeft, forgedErr.toString)
    assert(forgedErr.swap.toOption.exists(_.contains("missing seals")), forgedErr.toString)

  test("prepared-then-primary-fails recovers via view-change with durable prepares"):
    import cairn.kernel.BftQuorum.*
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val homes = replicas.map(k => k.name -> java.nio.file.Files.createTempDirectory(s"cairn-ip-${k.name}")).toMap
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), ledgerCtx)
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      id -> n
    }
    val block = nodes("r0").chainDigests.head
    replicas.foreach { k =>
      BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(homes(k.name)), manifest)
        .fold(e => fail(e), identity)
    }
    val bfts = replicas.map { k =>
      k.name -> BftReplica.certified(
        k, manifest,
        node = Some(nodes(k.name)), ledgerAuth = ledgerAuth,
        certStore = Some(homes(k.name).resolve("bft-certs.canon")),
        stateStore = Some(homes(k.name).resolve("bft-state.canon")),
        home = Some(homes(k.name))).fold(e => fail(e), identity)
    }.toMap
    def isCommit(sm: BftFinality.SignedMsg): Boolean = sm.msg match
      case Msg.Commit(_, _, _, _) => true
      case _                      => false
    def deliverAll(
        from: String,
        msgs: List[BftFinality.SignedMsg],
        exclude: Set[String] = Set.empty,
        dropCommits: Boolean = false,
    ): Unit =
      val filtered = if dropCommits then msgs.filterNot(isCommit) else msgs
      filtered.foreach { sm =>
        bfts.foreach { (id, r) =>
          if id != from && !exclude.contains(id) then
            r.receive(sm).fold(e => fail(s"$id: $e"), identity)
        }
      }
    // Primary proposes; circulate Prepares only — intercept all Commits.
    val out0 = bfts("r0").propose(0, 0, block).fold(e => fail(e), identity)
    deliverAll("r0", out0, dropCommits = true)
    var round = 0
    var progress = true
    while round < 16 && progress do
      progress = false
      bfts.foreach { (id, r) =>
        val out = r.drainOutbound()
        val kept = out.filterNot(isCommit)
        // Drop commits from the failed primary and from backups during prepare phase.
        if kept.nonEmpty then
          progress = true
          deliverAll(id, kept, exclude = Set("r0"), dropCommits = true)
        else if out.nonEmpty then
          // Consume commits without delivering them.
          ()
      }
      round += 1
    val honest = List("r1", "r2", "r3")
    assert(honest.forall(id => bfts(id).finalityCerts.isEmpty),
      clues(honest.map(id => id -> bfts(id).finalityCerts.map(_.view))))
    // Fail primary; view-change with prepared evidence; finalize in view 1.
    honest.foreach { id =>
      val out = bfts(id).requestViewChange(1).fold(e => fail(e), identity)
      deliverAll(id, out, exclude = Set("r0"))
    }
    round = 0
    progress = true
    while round < 24 && progress do
      progress = false
      honest.foreach { id =>
        val out = bfts(id).drainOutbound()
        if out.nonEmpty then
          progress = true
          deliverAll(id, out, exclude = Set("r0"))
      }
      round += 1
    val certs = honest.flatMap(id => bfts(id).finalityCerts.filter(_.blockDigest == block))
    assert(certs.nonEmpty, "expected certificate after view-change")
    assert(certs.forall(_.view >= 1), clues(certs.map(_.view)))
    assert(certs.exists(_.view == 1), clues(certs.map(_.view)))
    assertEquals(certs.map(_.blockDigest).toSet, Set(block))
    // New primary for view 1 re-proposed via NewView prepared path.
    assert(honest.exists(id => bfts(id).currentView >= 1))
    val state = Canon.decode(java.nio.file.Files.readAllBytes(homes("r1").resolve("bft-state.canon")))
      .flatMap(BftFinality.decodeReplicaStateWithReplicaSet)
      .fold(e => fail(e), identity)
    assert(
      state.prepareSeals.nonEmpty || state.viewChangeEvidence.nonEmpty ||
        state.prePrepareSeals.nonEmpty,
      state.toString)

  test("restart during failover restores prepare seals and ViewChange votes"):
    import cairn.kernel.BftQuorum.*
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val home = java.nio.file.Files.createTempDirectory("cairn-bft-restart-vc")
    val node = Node(home.resolve("node"), ledgerCtx)
    node.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(home), manifest)
      .fold(e => fail(e), identity)
    val block = node.chainDigests.head
    val statePath = home.resolve("bft-state.canon")
    val bft0 = BftReplica.certified(
      replicas.head, manifest,
      node = Some(node), ledgerAuth = ledgerAuth,
      stateStore = Some(statePath),
      home = Some(home)).fold(e => fail(e), identity)
    bft0.propose(0, 0, block).fold(e => fail(e), identity)
    // Quorum of prepares from peers.
    replicas.tail.foreach { kp =>
      val prep = BftFinality.sign(
        kp,
        Msg.Prepare(0, 0, BftFinality.valueOfBlock(block).digest, ReplicaId(kp.name)),
        manifest.replicaSetDigest,
        block).fold(e => fail(e), identity)
      bft0.receive(prep).fold(e => fail(e), identity)
    }
    val vc = bft0.requestViewChange(1).fold(e => fail(e), identity)
    assert(vc.exists(_.msg.isInstanceOf[Msg.ViewChange]))
    assert(java.nio.file.Files.exists(statePath))
    val decoded = Canon.decode(java.nio.file.Files.readAllBytes(statePath))
      .flatMap(BftFinality.decodeReplicaStateWithReplicaSet)
      .fold(e => fail(e), identity)
    assert(decoded.prepareSeals.nonEmpty, decoded.prepareSeals.toString)
    assert(decoded.viewChangeEvidence.nonEmpty, decoded.viewChangeEvidence.toString)
    assert(decoded.prePrepareSeals.nonEmpty, decoded.prePrepareSeals.toString)
    val viewBefore = bft0.currentView
    val bft1 = BftReplica.certified(
      replicas.head, manifest,
      node = Some(node), ledgerAuth = ledgerAuth,
      stateStore = Some(statePath),
      home = Some(home)).fold(e => fail(e), identity)
    assertEquals(bft1.currentView, viewBefore)
    // Restored prepared evidence must still be usable for a further view-change.
    val again = bft1.requestViewChange(viewBefore + 1)
    assert(again.isRight, again.toString)

  test("follower adopts certificates then recovers after restart"):
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val homeA = java.nio.file.Files.createTempDirectory("cairn-adopt-a")
    val homeB = java.nio.file.Files.createTempDirectory("cairn-adopt-b")
    val a = Node(homeA.resolve("node"), ledgerCtx)
    val b = Node(homeB.resolve("node"), ledgerCtx)
    a.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
      .fold(e => fail(e), identity)
    Sync.pull(a, b, ledgerAuth, None).fold(e => fail(e), identity)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(homeA), manifest)
      .fold(e => fail(e), identity)
    BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(homeB), manifest)
      .fold(e => fail(e), identity)
    val block = a.chainDigests.head
    val cert = BftFinality.agreeForSealedBlock(a, ledgerAuth, replicas, block)
      .fold(e => fail(e), identity)
    BftFinality.saveCerts(homeA.resolve("bft-certs.canon"), List(cert)).fold(e => fail(e), identity)
    BftFinality.advanceCheckpoint(homeA, cert).fold(e => fail(e), identity)
    // Persist certs before checkpoint on follower, then interrupt before chain write.
    val intent = BftFinality.AdoptionIntent(
      a.chainDigests, List(cert), phase = "certs-checkpoint")
    BftFinality.mergeVerifiedCerts(homeB, List(cert)).fold(e => fail(e), identity)
    BftFinality.advanceCheckpoint(homeB, cert).fold(e => fail(e), identity)
    BftFinality.saveAdoptionIntent(homeB, intent).fold(e => fail(e), identity)
    assert(java.nio.file.Files.exists(BftFinality.defaultAdoptionIntentPath(homeB)))
    BftFinality.resumeFollowerAdoption(homeB, b).fold(e => fail(e), identity)
    assertEquals(b.chainDigests, a.chainDigests)
    assert(!java.nio.file.Files.exists(BftFinality.defaultAdoptionIntentPath(homeB)))
    val cp = BftFinality.loadCheckpoint(homeB).fold(e => fail(e), identity)
    assert(cp.exists(_.certificate == cert.digest))
    // Journal with bodies alone (no prior cert store write) can still resume.
    val homeC = java.nio.file.Files.createTempDirectory("cairn-adopt-c")
    val c = Node(homeC.resolve("node"), ledgerCtx)
    Sync.pull(a, c, ledgerAuth, None).fold(e => fail(e), identity)
    BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(homeC), manifest)
      .fold(e => fail(e), identity)
    BftFinality.saveAdoptionIntent(
      homeC,
      BftFinality.AdoptionIntent(a.chainDigests, List(cert), phase = "started"))
      .fold(e => fail(e), identity)
    BftFinality.resumeFollowerAdoption(homeC, c).fold(e => fail(e), identity)
    assertEquals(c.chainDigests, a.chainDigests)
    assert(BftFinality.loadCheckpoint(homeC).fold(e => fail(e), identity).exists(_.certificate == cert.digest))

  test("peer URL generations advance monotonically on rebind"):
    val root = java.nio.file.Files.createTempDirectory("cairn-peer-gen")
    val kp = Keypair.dev("r0")
    PeerRegistry.addBound(root, kp, "http://127.0.0.1:1", PeerRegistry.Role.Replica)
      .fold(e => fail(e), identity)
    val g0 = PeerRegistry.load(root).fold(e => fail(e), identity).byName("r0").get
    assertEquals(g0.generation, 0L)
    PeerRegistry.addBound(root, kp, "http://127.0.0.1:1", PeerRegistry.Role.Replica)
      .fold(e => fail(e), identity)
    assertEquals(
      PeerRegistry.load(root).fold(e => fail(e), identity).byName("r0").get.generation, 0L)
    PeerRegistry.addBound(root, kp, "http://127.0.0.1:2", PeerRegistry.Role.Replica)
      .fold(e => fail(e), identity)
    val g1 = PeerRegistry.load(root).fold(e => fail(e), identity).byName("r0").get
    assertEquals(g1.generation, 1L)
    assertEquals(g1.baseUrl, "http://127.0.0.1:2")
