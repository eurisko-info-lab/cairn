package cairn.tests

import cairn.kernel.Authority.*
import cairn.kernel.Effects
import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.systemhandler.AuthorityGate

/** Phase 4–5 authority: audit mode records decisions; enforce mode blocks.
  * Each test constructs its own fresh `AuthorityGate` instance instead of
  * resetting shared global state — no `beforeEach` needed, and no risk of
  * one test's mode/policies leaking into another regardless of test
  * execution order.
  */
class AuthoritySuite extends munit.FunSuite:

  private val alice = Subject("alice")
  private val readReq = EffectRequest(alice, Effects.Action.FsRead, Resource("file", "/tmp/a"))
  private val appendReq = EffectRequest(alice, Effects.Action.LedgerAppend, Resource("ledger", "/tmp/node"))

  test("Phase 4 audit mode never blocks and records would-permit"):
    val gate = AuthorityGate()
    gate.install(List(PolicyEval.allowAll("allow-read", alice, Effects.Action.FsRead)))
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
    gate.install(List(PolicyEval.allowAll("allow-append", alice, Effects.Action.LedgerAppend)))
    val allowed = gate.check(appendReq)
    assert(allowed.isRight, allowed.toString)

  test("Kernel validate rejects mismatched Core derivation"):
    val policies = List(PolicyEval.denyAll("deny", alice, Effects.Action.FsRead))
    val bad = AuthorizationDerivation(readReq, policies, Decision.Allow, None, "lie")
    assert(Authority.validate(readReq, policies, bad).isLeft)

  test("deny overrides allow"):
    val policies = List(
      PolicyEval.allowAll("allow", alice, Effects.Action.FsRead),
      PolicyEval.denyAll("deny", alice, Effects.Action.FsRead))
    val d = PolicyEval.propose(readReq, policies)
    assertEquals(d.decision, Decision.Deny)

  test("AuthorityGate.forFamily gives each family its own instance, so Mode can genuinely diverge"):
    // forFamily is a shared registry, not a fresh-per-test instance like the
    // gates above — mutate and restore inside try/finally so a failed
    // assertion can't leave a family's Mode altered for other tests/suites
    // that share this JVM.
    val fs = AuthorityGate.forFamily(Effects.Family.Filesystem)
    val random = AuthorityGate.forFamily(Effects.Family.Random)
    assert(fs ne random, "different families must not share a gate instance")
    assertEquals(fs.currentMode, AuthorityGate.Mode.Enforce)
    assertEquals(random.currentMode, AuthorityGate.Mode.Enforce)
    try
      fs.setMode(AuthorityGate.Mode.Audit)
      assertEquals(fs.currentMode, AuthorityGate.Mode.Audit)
      assertEquals(random.currentMode, AuthorityGate.Mode.Enforce) // unaffected
    finally fs.setMode(AuthorityGate.Mode.Enforce) // restore

  test("AuthorityGate.forFamily is stable: the same family always returns the same instance"):
    val a = AuthorityGate.forFamily(Effects.Family.Terminal)
    val b = AuthorityGate.forFamily(Effects.Family.Terminal)
    assert(a eq b)
