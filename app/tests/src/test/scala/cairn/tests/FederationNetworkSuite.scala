package cairn.tests

import cairn.runtime.EffectContexts
import cairn.kernel.*
import cairn.systemhandler.*

/** PR33 slice 5: `FederationReplica` + `HttpNode`'s federation endpoints
  * reach real quorum agreement over actual HTTP — the same
  * "four in-JVM `HttpNode`s, real `HttpServer`s on loopback ports" pattern
  * `DistributionDaemonSuite`'s own BFT-block-finality tests already use.
  * The verify callback is stubbed to always accept: this suite's own focus
  * is the network protocol/wiring, not verification depth (already covered
  * by `FederationReplicaVerificationSuite`), so no CAS content needs to
  * exist anywhere — the proposal's digests are just consistent stand-ins.
  */
class FederationNetworkSuite extends munit.FunSuite:
  private val ledgerCtx = EffectContexts.forLedger()
  private val alwaysVerified: FederationReplica.VerifyProposal = (_, _) => FederationReplica.VerifyOutcome.Verified

  private def sampleProposal(
      federationId: Digest, replicaSetDigest: Digest, epoch: Long,
  ): FederationFinality.FederationProposal =
    FederationFinality.FederationProposal(
      federationId = federationId,
      transition = Digest.of(Canon.CStr(s"transition-$epoch")),
      before = Digest.of(Canon.CStr(s"before-$epoch")),
      after = Digest.of(Canon.CStr(s"after-$epoch")),
      epoch = epoch,
      replicaSet = replicaSetDigest)

  test("four HttpNode FederationReplicas exchange messages over real HTTP and mint a network certificate"):
    val auth = Keypair.dev("federation-network-auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val ids = manifest.ids
    val federationId = Digest.of(Canon.CStr("federation-network-suite-federation"))
    val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-federation-net-$id")).toMap
    val nodes = homes.map { (id, home) =>
      val n = Node(home.resolve("node"), ledgerCtx)
      n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
        .fold(e => fail(e), identity)
      id -> n
    }
    val https = scala.collection.mutable.ListBuffer.empty[HttpNode]
    try
      val ports = ids.map { id =>
        val home = homes(id)
        val federation = FederationReplica.certified(
          replicas.find(_.name == id).get, manifest, federationId, alwaysVerified,
          certStore = Some(home.resolve("federation-certs.canon")),
          stateStore = Some(home.resolve("federation-state.canon")))
          .fold(e => fail(e), identity)
        val http = HttpNode(nodes(id), ledgerAuth, peersRoot = Some(home), federation = Some(federation))
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
      val proposal = sampleProposal(federationId, manifest.replicaSetDigest, epoch = 1L)
      val cert = FederationFinality.agreeNetworkRemote(
        urls, proposal, replicas.head, authorities = manifest.authorities, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert.stateDigest, proposal.after)
      assertEquals(cert.previousState, proposal.before)
      assertEquals(cert.epoch, 1L)
      assertEquals(cert.federationId, federationId)
      assert(cert.commits.map(_._1.id).distinct.length >= BftQuorum.quorumSize(4))
      assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert, manifest), Right(()))

      // Continuous finality: second round on the same running durable replicas.
      val proposal2 = sampleProposal(federationId, manifest.replicaSetDigest, epoch = 2L)
        .copy(before = proposal.after)
      val cert2 = FederationFinality.agreeNetworkRemote(
        urls, proposal2, replicas.head, authorities = manifest.authorities, polls = 64, pollSleepMs = 30)
        .fold(e => fail(e), identity)
      assertEquals(cert2.stateDigest, proposal2.after)
      assertEquals(cert2.previousState, proposal.after)
      assertEquals(cert2.epoch, 2L)
      assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert2, manifest), Right(()))
    finally https.foreach(_.stop())

  test("/federation/status serves a verifiable FederationViewStatus"):
    val auth = Keypair.dev("federation-network-status-auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
    val federationId = Digest.of(Canon.CStr("federation-network-status-federation"))
    val home = java.nio.file.Files.createTempDirectory("cairn-federation-net-status")
    val node = Node(home.resolve("node"), ledgerCtx)
    val federation = FederationReplica.certified(replicas.head, manifest, federationId, alwaysVerified)
      .fold(e => fail(e), identity)
    val http = HttpNode(node, ledgerAuth, federation = Some(federation))
    try
      val port = http.start()
      val url = s"http://127.0.0.1:$port"
      val status = FederationFinality.fetchViewStatus(url).fold(e => fail(e), identity)
      assertEquals(status.replica, federation.id)
      assertEquals(status.view, 0)
      assertEquals(status.federationId, federationId)
      assertEquals(status.replicaSet, manifest.replicaSetDigest)
      assertEquals(FederationFinality.verifyViewStatus(manifest.authorities, status, federationId, manifest.replicaSetDigest), Right(()))
    finally http.stop()
