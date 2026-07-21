package cairn.systemhandler

import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.kernel.Authority.*
import cairn.kernel.Effects

/** Authority gate (Phases 4–5, priority #2). Handlers call
  * [[check]]/[[checked]] before privileged work.
  *
  * An instantiable class — matching the `Node`/`Cas`/`DiskCas`/`Branches`
  * pattern already used elsewhere in `system-handler` — so authority state
  * (`mode`/`policies`/`events`) is explicit and constructible rather than
  * shared, hidden, JVM-wide mutable state. Callers pass a gate instance
  * explicitly (constructor injection on `Node`, method parameter on effect
  * handler `perform`s). There is no process-global registry or default
  * instance: composition roots and tests construct gates and pass them down.
  *
  * A directly-constructed `AuthorityGate()` starts in `Mode.Audit` with no
  * policies. [[AuthorityGate.bootstrapped]] builds an Enforce gate with the
  * allow-all bootstrap policies used by production wiring.
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
    * In `Audit` mode (the default for a fresh gate) `check` never returns
    * `Left`, so `onDenied` only fires once a gate is put in `Enforce`.
    */
  def checked[E, A](req: EffectRequest)(onDenied: String => E)(body: => Either[E, A]): Either[E, A] =
    check(req) match
      case Left(err) => Left(onDenied(err))
      case Right(_)  => body

object AuthorityGate:
  enum Mode:
    case Audit, Enforce

  private def bootstrapPolicies: List[EffectPolicy] =
    Effects.Action.values.toList.map(a =>
      EffectPolicy(s"bootstrap-allow-${a.name}", "*", a, Resource("*", "*"), Decision.Allow))

  /** Production/test wiring helper: `Mode.Enforce` with one allow policy per
    * known `Action`, allowing any subject (`"*"`) over any resource — not just
    * the placeholder `Subject("local")` the 8 effect handlers use, since
    * `Node.append` authenticates as the real signing authority
    * (`Subject(authority.name)`, e.g. `"alice"`). Honestly **not** meaningful
    * access control yet: a blanket allow-everyone-everything policy can't deny
    * anything. Real enforcement needs real, distinct subjects and narrower
    * policies, which needs real identity.
    *
    * Each call returns a **fresh** gate — never a shared singleton.
    */
  def bootstrapped(): AuthorityGate =
    val gate = new AuthorityGate()
    gate.install(bootstrapPolicies)
    gate.setMode(Mode.Enforce)
    gate
