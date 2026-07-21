package cairn.systemhandler

import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.kernel.Authority.*

/** Authority gate (Phases 4–5). Starts in audit mode; enforcement is
  * opt-in per family via [[enforce]]. Handlers call [[check]] before
  * privileged work.
  */
object AuthorityGate:
  enum Mode:
    case Audit, Enforce

  @volatile private var mode: Mode = Mode.Audit
  @volatile private var policies: List[EffectPolicy] = Nil
  private val events = scala.collection.mutable.ListBuffer[AuthorityEvent]()

  def setMode(m: Mode): Unit = mode = m
  def currentMode: Mode = mode
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
      case Mode.Audit =>
        val would = derivation.decision == Decision.Allow &&
          derivation.grant.exists(_.covers(req, nowMillis))
        synchronized { events += AuthorityEvent.Audited(derivation, would) }
        // audit never blocks
        Right(Authority.auditPass(req))
      case Mode.Enforce =>
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
      mode = Mode.Enforce
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
