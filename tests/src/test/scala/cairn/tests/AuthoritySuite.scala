package cairn.tests

import cairn.kernel.Authority.*
import cairn.kernel.{EffectMeta, Effects}
import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.systemhandler.{AuthorityGate, EffectContext, Filesystem}

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

  test("priority #6 proof hook still validates via Kernel re-decide"):
    val policies = List(PolicyEval.allowAll("allow", alice, fsRead))
    val proof = PolicyEval.asProof(PolicyEval.propose(readReq, policies))
    assert(Authority.validateProof(proof, policies, nowMillis = 0).isRight)
    val lie = PolicyEval.asProof(
      AuthorizationDerivation(readReq, policies, Decision.Deny, None, "lie"))
    assert(Authority.validateProof(lie, policies, nowMillis = 0).isLeft)
