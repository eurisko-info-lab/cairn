package cairn.tests

import cairn.kernel.*
import cairn.systemhandler.{BftFinality, FederationFinality, FederationReplica, Keypair}

/** PR33 slices 2-3: `FederationReplica` in isolation — in-memory quorum
  * agreement (no HTTP yet, messages routed directly between replica
  * objects) and durable persistence/fail-closed behavior. The verify
  * callback is stubbed to always accept (`alwaysVerified`); real,
  * local-CAS-only verification lands in slice 4.
  */
class FederationReplicaSuite extends munit.FunSuite:
  private val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
  private val manifest = BftFinality.sealReplicaSet(replicas).fold(e => fail(e), identity)
  private val federationId = Digest.of(Canon.CStr("federation-replica-suite-chain"))
  private val alwaysVerified: FederationReplica.VerifyProposal = (_, _) => FederationReplica.VerifyOutcome.Verified

  private def sampleProposal(epoch: Long): FederationFinality.FederationProposal =
    FederationFinality.FederationProposal(
      federationId = federationId,
      transition = Digest.of(Canon.CStr(s"transition-$epoch")),
      before = Digest.of(Canon.CStr(s"before-$epoch")),
      after = Digest.of(Canon.CStr(s"after-$epoch")),
      epoch = epoch,
      replicaSet = manifest.replicaSetDigest)

  private def buildReplicas(
      verify: FederationReplica.VerifyProposal = alwaysVerified,
      stateStores: Map[String, java.nio.file.Path] = Map.empty,
      certStores: Map[String, java.nio.file.Path] = Map.empty,
  ): Map[String, FederationReplica] =
    replicas.map { k =>
      k.name -> FederationReplica.certified(
        k, manifest, federationId, verify, stateStore = stateStores.get(k.name), certStore = certStores.get(k.name))
        .fold(e => fail(e), identity)
    }.toMap

  /** Fixed-point delivery: broadcast every message to every replica (harmless
    * on a replica re-seeing its own already-applied message — `deliver` is
    * idempotent on an already-set slot) until nothing new is produced.
    * Registers `proposal` on every non-primary replica first — the stand-in
    * for what the HTTP `/federation/propose`/`/federation/msg` request body
    * will carry in slice 5.
    */
  private def dispatch(
      nodes: Map[String, FederationReplica], proposal: FederationFinality.FederationProposal,
      initial: List[BftFinality.SignedMsg],
  ): Unit =
    nodes.values.foreach(_.learnProposal(proposal))
    var inbox = initial
    var rounds = 0
    while inbox.nonEmpty && rounds < 32 do
      val batch = inbox
      inbox = Nil
      batch.foreach { sm =>
        nodes.values.foreach { r =>
          r.receive(sm) match
            case Right(_) => inbox = inbox ++ r.drainOutbound()
            case Left(_) => ()
        }
      }
      rounds += 1

  test("certified rejects a keypair not in the replica-set manifest"):
    val outsider = Keypair.dev("not-in-set")
    assert(FederationReplica.certified(outsider, manifest, federationId, alwaysVerified).isLeft)

  test("certified rejects a keypair whose public key does not match the manifest entry"):
    val impostor = Keypair.dev(replicas.head.name)
    assert(FederationReplica.certified(impostor, manifest, federationId, alwaysVerified).isLeft)

  test("certified rejects an unsealed manifest"):
    val unsealed = manifest.copy(seals = Nil)
    assert(FederationReplica.certified(replicas.head, unsealed, federationId, alwaysVerified).isLeft)

  test("four replicas reach quorum and every honest replica independently mints a matching certificate"):
    val nodes = buildReplicas()
    val proposal = sampleProposal(epoch = 1L)
    val primary = nodes(replicas.head.name)
    val msgs = primary.propose(view = 0, proposal).fold(e => fail(e), identity)
    dispatch(nodes, proposal, msgs)
    nodes.values.foreach { r =>
      val certs = r.finalityCerts
      assertEquals(certs.length, 1, s"replica ${r.id.id} certs: $certs")
      val cert = certs.head
      assertEquals(cert.stateDigest, proposal.after)
      assertEquals(cert.previousState, proposal.before)
      assertEquals(cert.epoch, 1L)
      assertEquals(cert.federationId, federationId)
      assertEquals(FederationFinality.FederationFinalityCertificate.verify(cert, manifest), Right(()))
    }

  test("only the designated primary for the current view may propose"):
    val nodes = buildReplicas()
    val nonPrimary = nodes(replicas.tail.head.name)
    val result = nonPrimary.propose(view = 0, sampleProposal(epoch = 1L))
    assert(result.isLeft)

  test("propose rejects a view that does not match the replica's current view"):
    val nodes = buildReplicas()
    val primary = nodes(replicas.head.name)
    val result = primary.propose(view = 1, sampleProposal(epoch = 1L))
    assert(result.isLeft)

  test("a PrePrepare naming an unlearned proposal is refused as missing closure, not silently dropped"):
    val nodes = buildReplicas()
    val proposal = sampleProposal(epoch = 1L)
    val primary = nodes(replicas.head.name)
    val pp = primary.propose(view = 0, proposal).fold(e => fail(e), identity).head
    // Deliver directly to a replica that was never told the proposal's content.
    val other = nodes(replicas.tail.head.name)
    val result = other.receive(pp)
    assert(result.isLeft)
    assert(FederationReplica.missingClosureDigests(result.left.getOrElse("")).contains(Set(proposal.after)), result.toString)

  test("restart from disk restores identical protocol state"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-federation-replica")
    val stateStores = replicas.map(k => k.name -> dir.resolve(s"${k.name}-state.canon")).toMap
    val certStores = replicas.map(k => k.name -> dir.resolve(s"${k.name}-certs.canon")).toMap
    val nodes = buildReplicas(stateStores = stateStores, certStores = certStores)
    val proposal = sampleProposal(epoch = 1L)
    val primary = nodes(replicas.head.name)
    val msgs = primary.propose(view = 0, proposal).fold(e => fail(e), identity)
    dispatch(nodes, proposal, msgs)
    // Commit order within a certificate is incidental (Map iteration, not
    // canon-sorted until encoded) — compare the commit SET, not list order.
    // Each replica mints independently from whatever commit subset ITS OWN
    // local view reached quorum with first, so compare each replica's
    // restored cert against its OWN pre-restart cert — not against another
    // replica's, which may validly differ in exact commit subset.
    def normalized(c: FederationFinality.FederationFinalityCertificate) =
      c.copy(commits = c.commits.sortBy(_._1.id))
    val preRestart = nodes.map((name, r) => name -> r.finalityCerts.map(normalized))

    val restarted = buildReplicas(stateStores = stateStores, certStores = certStores)
    restarted.foreach { (name, r) =>
      assertEquals(r.finalityCerts.map(normalized), preRestart(name), s"replica ${r.id.id} did not restore its certificate")
      assertEquals(r.currentView, 0)
    }

  test("identity mismatch on restore (wrong federationId) fails closed"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-federation-replica-mismatch")
    val statePath = dir.resolve("r0-state.canon")
    val certPath = dir.resolve("r0-certs.canon")
    val nodes = buildReplicas(stateStores = Map(replicas.head.name -> statePath), certStores = Map(replicas.head.name -> certPath))
    val proposal = sampleProposal(1L)
    dispatch(nodes, proposal, nodes(replicas.head.name).propose(view = 0, proposal).fold(e => fail(e), identity))

    val otherFederationId = Digest.of(Canon.CStr("some-other-federation"))
    val reloaded = FederationReplica.certified(
      replicas.head, manifest, otherFederationId, alwaysVerified,
      stateStore = Some(statePath), certStore = Some(certPath)).fold(e => fail(e), identity)
    // Fails closed: any subsequent operation refuses, since the restored
    // state's federationId doesn't match this replica's own.
    assert(reloaded.propose(0, sampleProposal(2L)).isLeft)

  test("a durable write failure on persist fails the replica closed for all subsequent operations"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-federation-replica-failclosed")
    // The state path's PARENT is an ordinary file, not a directory: it
    // doesn't exist yet (construction's restore step is skipped, no
    // crash), but DurableIo.writeConsensus's own `Files.createDirectories`
    // fails the moment persistState actually tries to write.
    val blocker = dir.resolve("blocker-file")
    java.nio.file.Files.write(blocker, Array[Byte](1, 2, 3))
    val bogusStatePath = blocker.resolve("state.canon")
    val r = FederationReplica.certified(
      replicas.head, manifest, federationId, alwaysVerified, stateStore = Some(bogusStatePath)
    ).fold(e => fail(e), identity)
    // First call attempts to persist and fails (the path itself is a directory).
    val first = r.propose(0, sampleProposal(1L))
    assert(first.isLeft, first.toString)
    // Second call must also fail — refusing to operate after the durable I/O failure, not just the one that triggered it.
    val second = r.propose(0, sampleProposal(2L))
    assert(second.isLeft, second.toString)
    assert(second.left.exists(_.contains("refusing to operate")), second.toString)
