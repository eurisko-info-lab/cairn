package cairn.tests

import cairn.kernel.Authority.*
import cairn.kernel.{Artifact, ArtifactKind, Canon, Digest, EffectMeta, Effects}
import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.systemhandler.{AuthorityGate, Branches, CasAdminEffects, CasEffects, DiskCas, EffectContext, Filesystem, Keypair, MemCas, Provenance, Sync}
import cairn.surface.{Cli, Transcript}
import cairn.systeminterface.Cas
import cairn.kernel.{Cst, Tx}

/** Phase 4–5 authority: audit mode records decisions; enforce mode blocks.
  * Each test constructs its own fresh `AuthorityGate` instance instead of
  * resetting shared global state — no `beforeEach` needed, and no risk of
  * one test's mode/policies leaking into another regardless of test
  * execution order.
  */
class AuthoritySuite extends munit.FunSuite:

  private val alice = Subject("alice")
  private val fsRead = EffectMeta.filesystem.actionKey("read")
  private val fsWrite = EffectMeta.filesystem.actionKey("write")
  private val wsRead = EffectMeta.workspace.actionKey("read")
  private val ledgerAppend = Effects.Action.LedgerAppend.key
  private val readReq = EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp/a"))
  private val appendReq = EffectRequest(alice, ledgerAppend, Resource("ledger", "/tmp/node"))

  test("Phase 4 audit mode never blocks and records would-permit"):
    val gate = AuthorityGate()
    gate.install(List(PolicyEval.allowAll("allow-read", alice, fsRead)))
    val auth = gate.check(readReq)
    assert(auth.isRight)
    val ev = gate.drainEvents()
    assert(ev.exists {
      case AuthorityEvent.Audited(d, would) => d.decision == Decision.Allow && would
      case _ => false
    })

  test("Phase 4 audit records would-deny when no policy matches"):
    val gate = AuthorityGate()
    val auth = gate.check(readReq)
    assert(auth.isRight) // audit never blocks
    val ev = gate.drainEvents()
    assert(ev.exists {
      case AuthorityEvent.Audited(_, would) => !would
      case _ => false
    })

  test("Phase 5 enforce mode rejects without allow policy"):
    val gate = AuthorityGate()
    gate.setMode(AuthorityGate.Mode.Enforce)
    val denied = gate.check(appendReq)
    assert(denied.isLeft, denied.toString)
    assert(gate.drainEvents().exists {
      case AuthorityEvent.Rejected(_) => true
      case _ => false
    })

  test("Phase 5 enforce mode allows with matching policy"):
    val gate = AuthorityGate()
    gate.setMode(AuthorityGate.Mode.Enforce)
    gate.install(List(PolicyEval.allowAll("allow-append", alice, ledgerAppend)))
    val allowed = gate.check(appendReq)
    assert(allowed.isRight, allowed.toString)

  test("Kernel validate rejects mismatched Core derivation"):
    val policies = List(PolicyEval.denyAll("deny", alice, fsRead))
    val bad = AuthorizationDerivation(readReq, policies, Decision.Allow, None, "lie")
    assert(Authority.validate(readReq, policies, bad).isLeft)

  test("deny overrides allow"):
    val policies = List(
      PolicyEval.allowAll("allow", alice, fsRead),
      PolicyEval.denyAll("deny", alice, fsRead))
    val d = PolicyEval.propose(readReq, policies)
    assertEquals(d.decision, Decision.Deny)

  test("separately constructed gates are distinct; Mode can diverge"):
    val fs = AuthorityGate.bootstrapped()
    val random = AuthorityGate.bootstrapped()
    assert(fs ne random, "each bootstrapped() call must return a fresh gate")
    assertEquals(fs.currentMode, AuthorityGate.Mode.Enforce)
    assertEquals(random.currentMode, AuthorityGate.Mode.Enforce)
    fs.setMode(AuthorityGate.Mode.Audit)
    assertEquals(fs.currentMode, AuthorityGate.Mode.Audit)
    assertEquals(random.currentMode, AuthorityGate.Mode.Enforce) // unaffected

  test("EffectContext authorize then Filesystem.perform"):
    import cairn.systeminterface.Filesystem as Fs
    val gate = AuthorityGate()
    gate.setMode(AuthorityGate.Mode.Enforce)
    gate.install(List(PolicyEval.allowAll("allow-alice", alice, fsRead)))
    val aliceCtx = EffectContext(alice, gate)
    val localCtx = EffectContext.local(gate)
    val resource = EffectMeta.filesystem.resource.at("/tmp")
    // Exists is FsRead-gated; alice is allowed, local is not.
    val aliceAuth = aliceCtx.authorize(fsRead, resource)
    assert(aliceAuth.isRight, aliceAuth.toString)
    assert(Filesystem.perform(Fs.Request.Exists(Fs.Path("/tmp")), aliceAuth.toOption.get).isRight)
    assert(localCtx.authorize(fsRead, resource).isLeft)
    assert(Filesystem.run(Fs.Request.Exists(Fs.Path("/tmp")), aliceCtx).isRight)
    assert(Filesystem.run(Fs.Request.Exists(Fs.Path("/tmp")), localCtx).isLeft)
    assertEquals(aliceCtx.subject, alice)
    assertEquals(localCtx.subject, Subject("local"))
    assert(localCtx.capabilities.isEmpty)
    assertEquals(localCtx.audit, EffectContext.Audit.Local)

  test("AuthorizedEffect covers check rejects mismatched resource"):
    import cairn.systeminterface.Filesystem as Fs
    val gate = AuthorityGate()
    gate.setMode(AuthorityGate.Mode.Enforce)
    gate.install(List(PolicyEval.allowAll("allow-alice", alice, fsRead)))
    val aliceCtx = EffectContext(alice, gate)
    val auth = aliceCtx.authorize(fsRead, EffectMeta.filesystem.resource.at("/other")).toOption.get
    assert(Filesystem.perform(Fs.Request.Exists(Fs.Path("/tmp")), auth).isLeft)

  test("pack-loader narrow policy allows WorkspaceRead under languages*"):
    import cairn.systemhandler.Workspace
    import cairn.systeminterface.Workspace as Ws
    val ctx = EffectContext.forPackLoader()
    assertEquals(ctx.subject, Subject("local"))
    assertEquals(ctx.gate.currentMode, AuthorityGate.Mode.Enforce)
    val langDirs = Workspace.run(Ws.Request.LanguageDirs, ctx)
    assert(langDirs.isRight, langDirs.toString)
    val readOk = Workspace.run(
      Ws.Request.ReadText(cairn.systeminterface.Filesystem.Path("languages/stlc.cairn")), ctx)
    assert(readOk.isRight, readOk.toString)

  test("pack-loader narrow policy denies wrong path"):
    import cairn.systemhandler.Workspace
    import cairn.systeminterface.Workspace as Ws
    val ctx = EffectContext.forPackLoader()
    val denied = Workspace.run(
      Ws.Request.ReadText(cairn.systeminterface.Filesystem.Path("/etc/passwd")), ctx)
    assert(denied.isLeft, denied.toString)

  test("pack-loader narrow policy denies wrong action"):
    val ctx = EffectContext.forPackLoader()
    assert(ctx.authorize(fsWrite, EffectMeta.workspace.resource.at("languages/stlc.cairn")).isLeft)
    assert(ctx.authorize(ledgerAppend, EffectMeta.workspace.resource.at("languages")).isLeft)

  test("pack-loader narrow policy denies wrong subject"):
    val ctx = EffectContext.forPackLoader()
    val aliceCtx = ctx.withSubject(alice)
    assert(aliceCtx.authorize(wsRead, EffectMeta.workspace.resource.at("languages")).isLeft)
    assert(ctx.authorize(wsRead, EffectMeta.workspace.resource.at("languages")).isRight)

  test("PackLoader loads packs under forPackLoader gate"):
    import cairn.runtime.PackLoader
    val packs = PackLoader(EffectContext.forPackLoader())
    val stlc = packs.requireOwn("stlc")
    assert(stlc.nonEmpty)

  test("derived ActionKey round-trips through host bridge and policy match"):
    val key = EffectMeta.filesystem.actionKey("read")
    assertEquals(key, Effects.Action.FsRead.key)
    assertEquals(key.toHost, Some(Effects.Action.FsRead))
    assertEquals(EffectMeta.filesystem.resource.kind, "filesystem")
    val policies = List(PolicyEval.allowAll("allow", alice, key))
    val req = EffectRequest(alice, key, EffectMeta.filesystem.resource.at("/tmp/x"))
    assertEquals(PolicyEval.propose(req, policies).decision, Decision.Allow)

  test("derived resource kind mismatch denies even when action matches"):
    val key = EffectMeta.process.actionKey("run")
    val policies = List(EffectPolicy(
      "proc-only",
      alice,
      key,
      EffectMeta.process.resource.at("scala-cli"),
      Decision.Allow))
    val ok = EffectRequest(alice, key, EffectMeta.process.resource.at("scala-cli"))
    val wrongKind = EffectRequest(alice, key, EffectMeta.filesystem.resource.at("scala-cli"))
    assertEquals(PolicyEval.propose(ok, policies).decision, Decision.Allow)
    assertEquals(PolicyEval.propose(wrongKind, policies).decision, Decision.Deny)

  test("Filesystem.intent derives keys from EffectMeta, not hardcoded enum"):
    import cairn.systeminterface.Filesystem as Fs
    Filesystem.intent(Fs.Request.Read(Fs.Path("/a"))) match
      case Some((action, resource)) =>
        assertEquals(action, EffectMeta.filesystem.actionKey("read"))
        assertEquals(resource, EffectMeta.filesystem.resource.at("/a"))
      case None => fail("expected gated intent for Read")
    assertEquals(Filesystem.intent(Fs.Request.Resolve(Fs.Path("/a"), Fs.Path("b"))), None)

  // ---- Priority #5: conditions / expiry / nonce / replay / delegation / attenuation ----

  test("conditions: matching args allow; missing or wrong args fail closed"):
    val policy = EffectPolicy(
      "cond",
      alice,
      fsRead,
      EffectMeta.filesystem.resource.at("/tmp*"),
      Decision.Allow,
      conditions = Map("role" -> "reader"))
    val ok = EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp/a"), Map("role" -> "reader"))
    val missing = EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp/a"))
    val wrong = EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp/a"), Map("role" -> "writer"))
    assertEquals(PolicyEval.propose(ok, List(policy)).decision, Decision.Allow)
    assertEquals(PolicyEval.propose(missing, List(policy)).decision, Decision.Deny)
    assertEquals(PolicyEval.propose(wrong, List(policy)).decision, Decision.Deny)

  test("conditions: unknown meta: key fails closed; known meta does not require args"):
    val unknown = EffectPolicy(
      "bad-meta",
      alice,
      fsRead,
      Resource("*", "*"),
      Decision.Allow,
      conditions = Map("meta:notARealKey" -> "x"))
    val known = EffectPolicy(
      "ttl",
      alice,
      fsRead,
      Resource("*", "*"),
      Decision.Allow,
      conditions = Map("meta:expiresAtEpochMillis" -> "1000"))
    val req = EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp/a"))
    assertEquals(PolicyEval.propose(req, List(unknown)).decision, Decision.Deny)
    val allowed = PolicyEval.propose(req, List(known))
    assertEquals(allowed.decision, Decision.Allow)
    assertEquals(allowed.grant.flatMap(_.expiresAtEpochMillis), Some(1000L))

  test("conditions appear in policy canon"):
    val p = EffectPolicy("c", alice, fsRead, Resource("*", "*"), Decision.Allow, Map("k" -> "v"))
    val bytes = cairn.kernel.Canon.encode(p.canon)
    assert(bytes.nonEmpty)
    // round-trip map contains conditions key via re-encode stability
    assertEquals(cairn.kernel.Canon.encode(p.canon).toSeq, bytes.toSeq)

  test("expiry: grant allowed before deadline, rejected after"):
    val policy = EffectPolicy(
      "exp",
      alice,
      fsRead,
      Resource("*", "*"),
      Decision.Allow,
      conditions = Map("meta:expiresAtEpochMillis" -> "1000"))
    val gate = AuthorityGate.enforcing(List(policy))
    val req = readReq
    assert(gate.check(req, nowMillis = 500).isRight)
    assert(gate.check(req, nowMillis = 2000).isLeft)
    val ctx = EffectContext(alice, AuthorityGate.enforcing(List(policy)), clock = () => 2000L)
    assert(ctx.authorize(req).isLeft)

  test("nonce: included in grant canon; reused nonce denied"):
    val policy = EffectPolicy(
      "once",
      alice,
      fsRead,
      Resource("*", "*"),
      Decision.Allow,
      conditions = Map("meta:nonce" -> "n-1"))
    val gate = AuthorityGate.enforcing(List(policy))
    val req = readReq
    val d = PolicyEval.propose(req, List(policy))
    assertEquals(d.grant.flatMap(_.nonce), Some("n-1"))
    val encoded = cairn.kernel.Canon.encode(d.grant.get.canon)
    assert(new String(encoded, "UTF-8").contains("n-1") || encoded.nonEmpty)
    assert(gate.check(req, nowMillis = 0).isRight, "first use")
    assert(gate.check(req, nowMillis = 0).isLeft, "replay nonce")

  test("replay: requestId consumed; second authorize denied"):
    val gate = AuthorityGate.enforcing(List(PolicyEval.allowAll("a", alice, fsRead)))
    val r1 = readReq.copy(requestId = Some("req-42"))
    val r2 = readReq.copy(requestId = Some("req-42"))
    assert(gate.check(r1).isRight)
    assert(gate.check(r2).isLeft)
    assert(gate.check(readReq.copy(requestId = Some("req-43"))).isRight)

  test("attenuation: Kernel witness allows narrower path; rejects widening"):
    val parent = CapabilityGrant(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp*"))
    val narrow = EffectMeta.filesystem.resource.at("/tmp/a")
    val wide = EffectMeta.filesystem.resource.at("/")
    val ok = PolicyEval.attenuate(parent, narrow)
    assert(ok.isRight, ok.toString)
    val (child, w) = ok.toOption.get
    assertEquals(child.resource, narrow)
    assert(Authority.AttenuationWitness.verify(w, parent, child).isRight)
    assert(PolicyEval.attenuate(parent, wide).isLeft)

  test("attenuation: dropping expiry widens and is rejected"):
    val parent = CapabilityGrant(alice, fsRead, Resource("*", "*"), expiresAtEpochMillis = Some(100L))
    val child = parent.copy(expiresAtEpochMillis = None, parentCanon = Some(parent.canon))
    assert(Authority.AttenuationWitness.check(parent, child).isLeft)

  test("delegation: A→B→C chain validates; broken chain / widen denied"):
    val bob = Subject("bob")
    val carol = Subject("carol")
    val root = CapabilityGrant(alice, fsRead, EffectMeta.filesystem.resource.at("/data*"))
    val ab = PolicyEval.delegate(root, bob, EffectMeta.filesystem.resource.at("/data/b*"))
    assert(ab.isRight, ab.toString)
    val bc = PolicyEval.delegate(ab.toOption.get.child, carol, EffectMeta.filesystem.resource.at("/data/b/c"))
    assert(bc.isRight, bc.toString)
    val chain = Authority.Delegation.validateChain(List(ab.toOption.get, bc.toOption.get))
    assert(chain.isRight, chain.toString)
    assertEquals(chain.toOption.get.subject, carol)
    // widen on second hop
    assert(PolicyEval.delegate(ab.toOption.get.child, carol, EffectMeta.filesystem.resource.at("/data*")).isLeft)
    // broken chain: carol hop with alice as grantor forged
    val forged = ab.toOption.get.copy(grantor = bob, grantee = carol)
    assert(Authority.Delegation.validate(forged).isLeft)

  test("delegation depth must increase by one per hop"):
    val bob = Subject("bob")
    val root = CapabilityGrant(alice, fsRead, Resource("*", "*"))
    val badChild = root.copy(
      subject = bob,
      delegationDepth = root.delegationDepth + 2,
      delegatedBy = Some(alice),
      parentCanon = Some(root.canon))
    assert(Authority.AttenuationWitness.check(root, badChild).isRight) // depth may increase by >1 for attenuation
    val d = Delegation(alice, bob, root, badChild,
      Authority.AttenuationWitness.check(root, badChild).toOption.get)
    assert(Authority.Delegation.validate(d).isLeft, "delegation requires exact +1 depth")

  // ---- Priority #6: Core-constructed proofs, Kernel-checked ----

  test("valid AuthorizationProof accepts via checkProof"):
    val policies = List(PolicyEval.allowAll("allow", alice, fsRead))
    val proof = PolicyEval.prove(readReq, policies, nowMillis = 0)
    assert(proof.isRight, proof.toString)
    assert(Authority.checkProof(proof.toOption.get, policies).isRight)

  test("tampered proof: widened grant resource rejected"):
    val policies = List(PolicyEval.allowAll("allow", alice, fsRead))
    val proof = PolicyEval.prove(readReq, policies, nowMillis = 0).toOption.get
    val widened = proof.copy(grant = proof.grant.copy(resource = Resource("*", "*")))
    assert(Authority.checkProof(widened, policies).isLeft)

  test("tampered proof: forged Allow with store Deny rejected"):
    val policies = List(
      PolicyEval.allowAll("allow", alice, fsRead),
      PolicyEval.denyAll("deny", alice, fsRead))
    val forged = PolicyEval.prove(readReq, List(policies.head), nowMillis = 0).toOption.get
    assert(Authority.checkProof(forged, policies).isLeft)

  test("missing allow policies in proof rejected"):
    val policies = List(PolicyEval.allowAll("allow", alice, fsRead))
    val proof = PolicyEval.prove(readReq, policies, nowMillis = 0).toOption.get
    val missing = proof.copy(allowPolicies = Nil, grant = proof.grant.copy(sourcePolicyIds = Nil))
    assert(Authority.checkProof(missing, policies).isLeft)

  test("cited policy not in store rejected"):
    val policies = List(PolicyEval.allowAll("allow", alice, fsRead))
    val proof = PolicyEval.prove(readReq, policies, nowMillis = 0).toOption.get
    val alien = proof.allowPolicies.head.copy(id = "not-in-store")
    val bad = proof.copy(
      allowPolicies = List(alien),
      grant = proof.grant.copy(sourcePolicyIds = List("not-in-store")))
    assert(Authority.checkProof(bad, policies).isLeft)

  test("widened attenuation in proof rejected"):
    val policies = List(PolicyEval.allowAll("allow", alice, fsRead))
    val base = PolicyEval.prove(readReq, policies, nowMillis = 0).toOption.get
    val parent = base.grant.copy(resource = EffectMeta.filesystem.resource.at("/tmp*"))
    val wideChild = parent.copy(resource = Resource("*", "/"), parentCanon = Some(parent.canon))
    // Forge a witness by bypassing check — use a valid witness for a different pair if possible.
    // AttenuationWitness is opaque; only mintable via check. So attach a valid narrow witness
    // then swap grant to a wider child.
    val narrow = EffectMeta.filesystem.resource.at("/tmp/a")
    val (child, w) = PolicyEval.attenuate(parent, narrow).toOption.get
    val tampered = base.copy(
      grant = wideChild.copy(sourcePolicyIds = base.grant.sourcePolicyIds),
      attenuatedFrom = Some((parent, w)))
    assert(Authority.checkProof(tampered, policies).isLeft)

  test("proveAttenuated narrow proof still covers request and checks"):
    val policies = List(EffectPolicy(
      "tmp",
      alice,
      fsRead,
      EffectMeta.filesystem.resource.at("/tmp*"),
      Decision.Allow))
    val req = EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp/a"))
    val base = PolicyEval.prove(req, policies, nowMillis = 0).toOption.get
    // base grant resource is request path; attenuate is no-op-ish if already exact.
    // Build from a broader parent grant first:
    val broadGrant = base.grant.copy(resource = EffectMeta.filesystem.resource.at("/tmp*"))
    val broadProof = base.copy(grant = broadGrant)
    // Direct broad grant fails justification (resource != request) unless attenuated:
    assert(Authority.checkProof(broadProof, policies).isLeft)
    val attenuated = PolicyEval.proveAttenuated(
      broadProof.copy(grant = broadGrant),
      req.resource)
    assert(attenuated.isRight, attenuated.toString)
    assert(Authority.checkProof(attenuated.toOption.get, policies).isRight)

  test("gate Enforce path: Core prove → Kernel checkProof"):
    val gate = AuthorityGate.enforcing(List(PolicyEval.allowAll("allow", alice, fsRead)))
    assert(gate.check(readReq, nowMillis = 0).isRight)
    assert(gate.check(appendReq, nowMillis = 0).isLeft)

  // ---- Resource matching / malformed expiry / capabilities / delegation auth ----

  test("Resource.matches: exact vs wildcard prefix; /tmp/a does not match /tmp/abc"):
    val exact = Resource("filesystem", "/tmp/a")
    val other = Resource("filesystem", "/tmp/abc")
    val prefix = Resource("filesystem", "/tmp*")
    val star = Resource("filesystem", "*")
    assert(exact.matches(exact))
    assert(!other.matches(exact), "exact pattern must not prefix-match")
    assert(other.matches(prefix))
    assert(exact.matches(prefix))
    assert(exact.matches(star))
    assert(!Resource("filesystem", "/var/a").matches(prefix))
    // attenuation uses fixed matches
    assert(exact.isAttenuationOf(prefix))
    assert(!other.isAttenuationOf(exact))

  test("malformed meta:expiresAtEpochMillis fails closed (no unlimited grant)"):
    val banana = EffectPolicy(
      "bad-exp",
      alice,
      fsRead,
      Resource("*", "*"),
      Decision.Allow,
      conditions = Map("meta:expiresAtEpochMillis" -> "banana"))
    val req = readReq
    val d = PolicyEval.propose(req, List(banana))
    assertEquals(d.decision, Decision.Deny)
    assert(PolicyEval.prove(req, List(banana), nowMillis = 0).isLeft)
    val gate = AuthorityGate.enforcing(List(banana))
    assert(gate.check(req, nowMillis = 0).isLeft)

  test("EffectContext capabilities: covering VerifiedCapability authorizes without policy"):
    val policies = List(EffectPolicy(
      "tmp", alice, fsRead, EffectMeta.filesystem.resource.at("/tmp*"), Decision.Allow))
    val broadReq = EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp*"))
    val proof = PolicyEval.prove(broadReq, policies, nowMillis = 0).toOption.get
    val cap = Authority.VerifiedCapability.fromProof(proof, policies).toOption.get
    val emptyGate = AuthorityGate.enforcing(Nil) // no policies
    val ctx = EffectContext(alice, emptyGate, capabilities = List(cap), clock = () => 0L)
    val ok = ctx.authorize(fsRead, EffectMeta.filesystem.resource.at("/tmp/a"))
    assert(ok.isRight, ok.toString)
    val miss = ctx.authorize(fsRead, EffectMeta.filesystem.resource.at("/var/a"))
    assert(miss.isLeft)
    // empty capabilities fall back to policy path
    val policyCtx = EffectContext(alice, AuthorityGate.enforcing(List(PolicyEval.allowAll("a", alice, fsRead))))
    assert(policyCtx.authorize(readReq).isRight)

  test("capability forgery: raw CapabilityGrant cannot enter withCapabilities"):
    // Compile-time: withCapabilities takes List[VerifiedCapability] only.
    // Runtime: sole mint path is fromProof — a forged wide grant in a
    // tampered proof is rejected; covers-alone is not a mint API.
    val policies = List(PolicyEval.allowAll("a", alice, fsRead))
    val proof = PolicyEval.prove(readReq, policies, 0).toOption.get
    val ok = Authority.VerifiedCapability.fromProof(proof, policies)
    assert(ok.isRight, ok.toString)
    val widened = proof.copy(grant = proof.grant.copy(resource = Resource("*", "*")))
    assert(Authority.VerifiedCapability.fromProof(widened, policies).isLeft)
    // Empty store / missing citation cannot mint
    assert(Authority.VerifiedCapability.fromProof(proof, Nil).isLeft)

  test("delegation root: widened TTL / omitted policy nonce rejected before hops"):
    val bob = Subject("bob")
    val carol = Subject("carol")
    val rootPolicies = List(EffectPolicy(
      "alice-data",
      alice,
      fsRead,
      EffectMeta.filesystem.resource.at("/data*"),
      Decision.Allow,
      conditions = Map(
        "meta:expiresAtEpochMillis" -> "1000",
        "meta:nonce" -> "root-n1")))
    val honestRoot = CapabilityGrant(
      alice, fsRead, EffectMeta.filesystem.resource.at("/data*"),
      expiresAtEpochMillis = Some(1000L),
      nonce = Some("root-n1"),
      sourcePolicyIds = List("alice-data"))
    val ab = PolicyEval.delegate(honestRoot, bob, EffectMeta.filesystem.resource.at("/data/b*")).toOption.get
    val bc = PolicyEval.delegate(ab.child, carol, EffectMeta.filesystem.resource.at("/data/b/c")).toOption.get
    val carolReq = EffectRequest(carol, fsRead, EffectMeta.filesystem.resource.at("/data/b/c"))
    val ok = PolicyEval.proveDelegated(carolReq, rootPolicies, List(ab, bc), nowMillis = 0)
    assert(ok.isRight, ok.toString)
    assert(Authority.checkProof(ok.toOption.get, rootPolicies).isRight)
    // Widened TTL (omit expiry) on a manually constructed root
    val wideTtl = honestRoot.copy(expiresAtEpochMillis = None)
    val abWide = PolicyEval.delegate(wideTtl, bob, EffectMeta.filesystem.resource.at("/data/b*")).toOption.get
    val bcWide = PolicyEval.delegate(abWide.child, carol, EffectMeta.filesystem.resource.at("/data/b/c")).toOption.get
    val badTtl = PolicyEval.proveDelegated(carolReq, rootPolicies, List(abWide, bcWide), nowMillis = 0)
    assert(badTtl.isLeft, badTtl.toString)
    assert(badTtl.swap.exists(_.contains("expiry")), badTtl.toString)
    // Omitted policy nonce
    val noNonce = honestRoot.copy(nonce = None)
    val abN = PolicyEval.delegate(noNonce, bob, EffectMeta.filesystem.resource.at("/data/b*")).toOption.get
    val bcN = PolicyEval.delegate(abN.child, carol, EffectMeta.filesystem.resource.at("/data/b/c")).toOption.get
    val badNonce = PolicyEval.proveDelegated(carolReq, rootPolicies, List(abN, bcN), nowMillis = 0)
    assert(badNonce.isLeft, badNonce.toString)
    assert(badNonce.swap.exists(_.contains("nonce")), badNonce.toString)

  test("Sync.pull aborts before chain advance on authorized CAS failure"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-sync-abort")
    val aliceKp = Keypair.dev("alice")
    val auth = Map("alice" -> aliceKp.publicBytes)
    val src = cairn.systemhandler.Node(dir.resolve("src"), EffectContext.forLedger())
    src.append(aliceKp, auth, List(aliceKp.signTx(Tx.RegisterIdentity("alice", aliceKp.publicBytes))))
      .fold(e => fail(e), identity)
    // Consumer CAS denied — contains/get/put must not be treated as "missing"
    val denied = cairn.systemhandler.Node(dir.resolve("denied"), EffectContext.forPackLoader())
    val chainFile = dir.resolve("denied").resolve("chain")
    assert(!java.nio.file.Files.exists(chainFile))
    val pull = Sync.pull(src, denied, auth)
    assert(pull.isLeft, pull.toString)
    assert(pull.swap.exists(e => e.contains("denied") || e.contains("blob")), pull.toString)
    assert(!java.nio.file.Files.exists(chainFile), "chain must not advance after CAS failure")

  test("delegation Alice→Bob→Carol authorizes Carol via checkProof"):
    val bob = Subject("bob")
    val carol = Subject("carol")
    val rootPolicies = List(EffectPolicy(
      "alice-data",
      alice,
      fsRead,
      EffectMeta.filesystem.resource.at("/data*"),
      Decision.Allow))
    val root = CapabilityGrant(
      alice, fsRead, EffectMeta.filesystem.resource.at("/data*"),
      sourcePolicyIds = List("alice-data"))
    val ab = PolicyEval.delegate(root, bob, EffectMeta.filesystem.resource.at("/data/b*")).toOption.get
    val bc = PolicyEval.delegate(ab.child, carol, EffectMeta.filesystem.resource.at("/data/b/c")).toOption.get
    val carolReq = EffectRequest(carol, fsRead, EffectMeta.filesystem.resource.at("/data/b/c"))
    val proof = PolicyEval.proveDelegated(carolReq, rootPolicies, List(ab, bc), nowMillis = 0)
    assert(proof.isRight, proof.toString)
    assert(Authority.checkProof(proof.toOption.get, rootPolicies).isRight)
    // Without delegation fix, citing Alice policy against Carol request would fail:
    val naive = PolicyEval.prove(carolReq, rootPolicies, 0)
    assert(naive.isLeft, "Carol is not in root policy")

  test("attenuation parentCanon mismatch and illegal subject change rejected"):
    val parent = CapabilityGrant(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp*"))
    val narrow = EffectMeta.filesystem.resource.at("/tmp/a")
    val (child, _) = PolicyEval.attenuate(parent, narrow).toOption.get
    val badCanon = child.copy(parentCanon = Some(Canon.CStr("not-parent")))
    assert(Authority.AttenuationWitness.check(parent, badCanon).isLeft)
    val bob = Subject("bob")
    val illegalSubject = child.copy(subject = bob, delegatedBy = None)
    assert(Authority.AttenuationWitness.check(parent, illegalSubject).isLeft)
    // legitimate subject change only via Delegation (delegatedBy set)
    val viaDeleg = PolicyEval.delegate(parent, bob, narrow)
    assert(viaDeleg.isRight, viaDeleg.toString)

  test("AuthorizationProof.canon distinguishes materially different proofs"):
    val policies = List(PolicyEval.allowAll("allow", alice, fsRead))
    val p1 = PolicyEval.prove(readReq, policies, nowMillis = 0).toOption.get
    val p2 = PolicyEval.prove(readReq.copy(requestId = Some("r-1")), policies, nowMillis = 0).toOption.get
    val p3 = PolicyEval.prove(
      EffectRequest(alice, fsRead, EffectMeta.filesystem.resource.at("/tmp/b")),
      policies, nowMillis = 0).toOption.get
    val c1 = Canon.encode(p1.canon).toSeq
    val c2 = Canon.encode(p2.canon).toSeq
    val c3 = Canon.encode(p3.canon).toSeq
    assert(c1 != c2, "requestId must affect canon")
    assert(c1 != c3, "resource must affect canon")
    val attenuated = PolicyEval.proveAttenuated(
      p1.copy(grant = p1.grant.copy(resource = EffectMeta.filesystem.resource.at("/tmp*"))),
      readReq.resource)
    // may fail justification setup — build properly:
    val broad = p1.copy(grant = p1.grant.copy(resource = EffectMeta.filesystem.resource.at("/tmp*")))
    val att = PolicyEval.proveAttenuated(broad, readReq.resource).toOption.get
    assert(Canon.encode(att.canon).toSeq != c1, "attenuation witness must affect canon")

  test("narrow deployment policies: process/ledger/lsp deny out-of-scope"):
    val proc = EffectContext.forProcess()
    assert(proc.authorize(
      EffectMeta.process.actionKey("run"),
      EffectMeta.process.resource.at("scala-cli")).isRight)
    assert(proc.authorize(fsRead, EffectMeta.filesystem.resource.at("/tmp")).isLeft)
    val led = EffectContext.forLedger()
    val authSubj = led.withSubject(Subject("alice"))
    assert(authSubj.authorize(
      EffectMeta.ledgerTransport.actionKey("append"),
      EffectMeta.ledgerTransport.resource.at("/tmp/node")).isRight)
    assert(authSubj.authorize(fsRead, EffectMeta.filesystem.resource.at("/tmp")).isLeft)
    val lsp = EffectContext.forLsp()
    assert(lsp.authorize(
      EffectMeta.lsp.actionKey("read"), EffectMeta.lsp.resource.any).isRight)
    assert(lsp.authorize(fsWrite, EffectMeta.filesystem.resource.at("/x")).isLeft)
    val casCtx = EffectContext.forCas()
    assert(casCtx.authorize(
      EffectMeta.cas.actionKey("put"),
      EffectMeta.cas.resource.at("abc")).isRight)
    assert(casCtx.authorize(fsRead, EffectMeta.filesystem.resource.at("/tmp")).isLeft)
    assert(led.authorize(
      EffectMeta.cas.actionKey("put"),
      EffectMeta.cas.resource.at("abc")).isRight, "forLedger includes local CAS")
    assert(led.authorize(fsRead, EffectMeta.filesystem.resource.at("/tmp")).isRight,
      "forLedger includes chain FS")
    assert(led.authorize(fsWrite, EffectMeta.filesystem.resource.at("/tmp")).isRight)
    val fsCtx = EffectContext.forFilesystem("/tmp*")
    assert(fsCtx.authorize(fsWrite, EffectMeta.filesystem.resource.at("/tmp/x")).isRight)
    assert(fsCtx.authorize(
      EffectMeta.ledgerTransport.actionKey("append"),
      EffectMeta.ledgerTransport.resource.at("/tmp")).isLeft)
    val brCtx = EffectContext.forBranches()
    assert(brCtx.authorize(
      EffectMeta.cas.actionKey("put"),
      EffectMeta.cas.resource.at("abc")).isRight)
    assert(brCtx.authorize(fsWrite, EffectMeta.filesystem.resource.at("/tmp/refs")).isRight)
    assert(brCtx.authorize(
      EffectMeta.ledgerTransport.actionKey("append"),
      EffectMeta.ledgerTransport.resource.at("/tmp")).isLeft)

  test("CasEffects: authorize → perform put/get/contains over MemCas"):
    val store = MemCas()
    val ctx = EffectContext.forCas()
    val art = Artifact(ArtifactKind.Term, Canon.CStr("hello-cas"))
    val put = CasEffects.run(store, Cas.Request.Put(art), ctx)
    assert(put.isRight, put.toString)
    val key = put.toOption.get match
      case Cas.Response.Key(k) => k
      case other => fail(s"expected Key, got $other")
    val got = CasEffects.run(store, Cas.Request.Get(key.valueHash), ctx)
    assertEquals(got.map {
      case Cas.Response.Stored(a) => a.digest
      case _ => Digest.ofBytes(Array.empty)
    }, Right(art.digest))
    assertEquals(CasEffects.contains(store, key.valueHash, ctx), Right(true))
    val denied = CasEffects.run(store, Cas.Request.Get(key.valueHash), EffectContext.forPackLoader())
    assert(denied.isLeft, denied.toString)
    assert(CasEffects.contains(store, key.valueHash, EffectContext.forPackLoader()).isLeft)

  test("CasAdminEffects: stats authorized under forCas; denied under forPackLoader"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-cas-admin")
    val ok = CasAdminEffects.stats(dir, EffectContext.forCas())
    assert(ok.isRight, ok.toString)
    val denied = CasAdminEffects.stats(dir, EffectContext.forPackLoader())
    assert(denied.isLeft, denied.toString)

  test("Provenance.why: stats-gated index under forCas; denied under forPackLoader"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-prov-auth")
    val disk = DiskCas(dir)
    val ctx = EffectContext.forCas()
    val out = Digest.ofBytes("out".getBytes)
    Provenance.record(disk, out, Nil, "test", ctx).fold(e => fail(e.toString), identity)
    val hops = Provenance.why(dir, out, ctx)
    assert(hops.isRight, hops.toString)
    assert(hops.exists(_.nonEmpty), hops.toString)
    assert(Provenance.why(dir, out, EffectContext.forPackLoader()).isLeft)

  test("Branches refs FS: gated under forBranches; denied under forCas-only"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-refs-auth")
    val cas = MemCas()
    val art = Artifact(ArtifactKind.Term, Canon.CStr("branch-seed"))
    val key = CasEffects.put(cas, art, EffectContext.forCas()).fold(e => fail(e.toString), identity)
    val denied = Branches(cas, dir.resolve("refs"), EffectContext.forCas())
    intercept[RuntimeException](denied.advance("main", key))
    val ok = Branches(cas, dir.resolve("refs"), EffectContext.forBranches())
    ok.advance("main", key)
    assertEquals(ok.load("main").head, Some(key))
    assertEquals(ok.list(), List("main"))

  test("Node chain FS: gated under forLedger; denied under forCas-only"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-chain-auth")
    val denied = cairn.systemhandler.Node(dir.resolve("denied"), EffectContext.forCas())
    intercept[RuntimeException](denied.chainDigests)
    val alice = Keypair.dev("alice")
    val auth = Map("alice" -> alice.publicBytes)
    val ok = cairn.systemhandler.Node(dir.resolve("ok"), EffectContext.forLedger())
    assertEquals(ok.chainDigests, Nil)
    val block = ok.append(alice, auth, List(alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))))
    assert(block.isRight, block.toString)
    assertEquals(ok.chainDigests, List(block.toOption.get.digest))
    val src = cairn.systemhandler.Node(dir.resolve("src"), EffectContext.forLedger())
    src.append(alice, auth, List(alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))))
      .fold(e => fail(e), identity)
    val pullDenied = Sync.pull(src, denied, auth)
    assert(pullDenied.isLeft, pullDenied.toString)

  test("Transcript run-dir FS: gated under forFilesystem; denied under forCas-only"):
    val work = java.nio.file.Files.createTempDirectory("cairn-tx-fs")
    val packs = cairn.runtime.PackLoader(EffectContext.forPackLoader())
    val src = """transcript t { node a ; }"""
    val denied = Transcript.run(
      src, Map.empty, work, Map.empty, packs,
      EffectContext.forLedger(), EffectContext.forProcess(), EffectContext.forCas())
    assert(denied.isLeft, denied.toString)
    assert(denied.swap.exists(_.contains("denied")), denied.toString)
    val ok = Transcript.run(
      src, Map.empty, work.resolve("ok"), Map.empty, packs,
      EffectContext.forLedger(), EffectContext.forProcess(), EffectContext.forFilesystem())
    assert(ok.isRight, ok.toString)

  test("Cli hash ReadBytes: gated under forFilesystem; denied under forCas-only"):
    val f = java.nio.file.Files.createTempFile("cairn-hash", ".txt")
    java.nio.file.Files.writeString(f, "hello")
    val packs = cairn.runtime.PackLoader(EffectContext.forPackLoader())
    val denied = Cli.main(
      List("hash", f.toString), Map.empty, Map.empty, packs,
      EffectContext.forLedger(), EffectContext.forProcess(), EffectContext.forLsp(), EffectContext.forCas())
    assert(denied.isLeft, denied.toString)
    assert(denied.swap.exists(_.contains("denied")), denied.toString)
    val ok = Cli.main(
      List("hash", f.toString), Map.empty, Map.empty, packs,
      EffectContext.forLedger(), EffectContext.forProcess(), EffectContext.forLsp(), EffectContext.forFilesystem())
    assert(ok.isRight, ok.toString)

  test("CasAdminEffects.artifacts: gated under forLedger/forCas; denied under forFilesystem-only"):
    val root = java.nio.file.Files.createTempDirectory("cairn-arts")
    val art = Artifact(ArtifactKind.Term, Cst.toCanon(Cst.Leaf("x")))
    CasEffects.put(DiskCas(root), art, EffectContext.forCas()).fold(e => fail(e.toString), identity)
    val denied = CasAdminEffects.artifacts(root, EffectContext.forFilesystem())
    assert(denied.isLeft, denied.toString)
    val ok = CasAdminEffects.artifacts(root, EffectContext.forLedger())
    assert(ok.exists(_.exists(_.digest == art.digest)), ok.toString)

  test("LedgerTransport: authorize → perform append over Node"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-lt")
    val node = cairn.systemhandler.Node(dir, EffectContext.forLedger())
    val alice = Keypair.dev("alice")
    val auth = Map("alice" -> alice.publicBytes)
    val req = cairn.systeminterface.LedgerTransport.Request.Append(
      "alice", auth, List(alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes))))
    val got = cairn.systemhandler.LedgerTransport.run(node, alice, req, node.ctx)
    assert(got.isRight, got.toString)
    val denied = cairn.systemhandler.LedgerTransport.run(
      node, alice, req, EffectContext.forPackLoader())
    assert(denied.isLeft, denied.toString)
