package cairn.tests

import cairn.kernel.Authority.*
import cairn.kernel.Effects
import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.systemhandler.AuthorityGate

/** Phase 4–5 authority: audit mode records decisions; enforce mode blocks. */
class AuthoritySuite extends munit.FunSuite:

  override def beforeEach(context: BeforeEach): Unit =
    AuthorityGate.clearPolicies()
    AuthorityGate.setMode(AuthorityGate.Mode.Audit)
    AuthorityGate.drainEvents()

  private val alice = Subject("alice")
  private val readReq = EffectRequest(alice, Effects.Action.FsRead, Resource("file", "/tmp/a"))
  private val appendReq = EffectRequest(alice, Effects.Action.LedgerAppend, Resource("ledger", "/tmp/node"))

  test("Phase 4 audit mode never blocks and records would-permit"):
    AuthorityGate.install(List(PolicyEval.allowAll("allow-read", alice, Effects.Action.FsRead)))
    val auth = AuthorityGate.check(readReq)
    assert(auth.isRight)
    val ev = AuthorityGate.drainEvents()
    assert(ev.exists {
      case AuthorityEvent.Audited(d, would) => d.decision == Decision.Allow && would
      case _ => false
    })

  test("Phase 4 audit records would-deny when no policy matches"):
    val auth = AuthorityGate.check(readReq)
    assert(auth.isRight) // audit never blocks
    val ev = AuthorityGate.drainEvents()
    assert(ev.exists {
      case AuthorityEvent.Audited(_, would) => !would
      case _ => false
    })

  test("Phase 5 enforce mode rejects without allow policy"):
    AuthorityGate.setMode(AuthorityGate.Mode.Enforce)
    val denied = AuthorityGate.check(appendReq)
    assert(denied.isLeft, denied.toString)
    assert(AuthorityGate.drainEvents().exists {
      case AuthorityEvent.Rejected(_) => true
      case _ => false
    })

  test("Phase 5 enforce mode allows with matching policy"):
    AuthorityGate.setMode(AuthorityGate.Mode.Enforce)
    AuthorityGate.install(List(PolicyEval.allowAll("allow-append", alice, Effects.Action.LedgerAppend)))
    val allowed = AuthorityGate.check(appendReq)
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
