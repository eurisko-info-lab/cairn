package cairn.systemhandler

import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.kernel.Authority.*

/** Authority gate (Phases 4–5, priority #2 first slice). Starts in audit
  * mode; enforcement is opt-in per family via [[enforce]]. Handlers call
  * [[check]]/[[checked]] before privileged work.
  *
  * An instantiable class (not a singleton `object`) — matching the
  * `Node`/`Cas`/`DiskCas`/`Branches` pattern already used elsewhere in
  * `system-handler` — so authority state (`mode`/`policies`/`events`) is
  * explicit and constructible rather than shared, hidden, JVM-wide mutable
  * state. `AuthorityGate.default` is the process-wide instance the 8
  * effect handlers and `Node.append` use today; full per-call injection
  * (no default at all) is a separate future slice.
  */
final class AuthorityGate(
    @volatile private var mode: AuthorityGate.Mode = AuthorityGate.Mode.Audit,
    @volatile private var policies: List[EffectPolicy] = Nil):
  private val events = scala.collection.mutable.ListBuffer[AuthorityEvent]()

  def setMode(m: AuthorityGate.Mode): Unit = mode = m
  def currentMode: AuthorityGate.Mode = mode
  def install(ps: List[EffectPolicy]): Unit = policies = ps
  def clearPolicies(): Unit = policies = Nil
  def drainEvents(): List[AuthorityEvent] =
    synchronized { val out = events.toList; events.clear(); out }

  /** Check authorization. In Audit mode always returns the request wrapped as
    * authorized (after recording whether it *would* be permitted). In Enforce
    * mode, only Kernel-validated allows proceed.
    */
  def check(req: EffectRequest, nowMillis: Long = System.currentTimeMillis())
      : Either[String, AuthorizedRequest] =
    val derivation = PolicyEval.propose(req, policies)
    mode match
      case AuthorityGate.Mode.Audit =>
        val would = derivation.decision == Decision.Allow &&
          derivation.grant.exists(_.covers(req, nowMillis))
        synchronized { events += AuthorityEvent.Audited(derivation, would) }
        // audit never blocks
        Right(Authority.auditPass(req))
      case AuthorityGate.Mode.Enforce =>
        Authority.validate(req, policies, derivation, nowMillis) match
          case Right(auth) =>
            synchronized { events += AuthorityEvent.Enforced(derivation, Some(auth)) }
            Right(auth)
          case Left(err) =>
            synchronized { events += AuthorityEvent.Rejected(derivation) }
            Left(err)

  /** Phase 5 family enforcement helper — temporarily enforce for one check. */
  def enforcing[A](body: => Either[String, A]): Either[String, A] =
    val prev = mode
    try
      mode = AuthorityGate.Mode.Enforce
      body
    finally mode = prev

  /** Check-then-run: denies map to the caller's own error type via
    * `onDenied`, so handlers don't hand-roll the same check/map boilerplate.
    * In `Audit` mode (the default everywhere today) `check` never returns
    * `Left`, so `onDenied` only fires once a family opts into `Enforce`.
    */
  def checked[E, A](req: EffectRequest)(onDenied: String => E)(body: => Either[E, A]): Either[E, A] =
    check(req) match
      case Left(err) => Left(onDenied(err))
      case Right(_)  => body

object AuthorityGate:
  enum Mode:
    case Audit, Enforce

  /** Process-wide default instance — the 8 effect handlers and `Node.append`
    * use this today; full per-call injection is a follow-up (priority #2,
    * next slice).
    */
  val default: AuthorityGate = new AuthorityGate()
