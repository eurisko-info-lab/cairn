package cairn.systemhandler

import cairn.core.PolicyEval
import cairn.kernel.Authority
import cairn.kernel.Authority.*
import cairn.kernel.EffectMeta

/** Authority gate (Phases 4–5, priority #2). Composition roots authorize via
  * [[EffectContext.authorize]] (which calls [[check]]); handlers accept only
  * [[AuthorizedEffect]] and must not call [[check]]/[[checked]] themselves.
  *
  * An instantiable class — matching the `Node`/`Cas`/`DiskCas`/`Branches`
  * pattern already used elsewhere in `system-handler` — so authority state
  * (`mode`/`policies`/`events`) is explicit and constructible rather than
  * shared, hidden, JVM-wide mutable state. There is no process-global
  * registry, default instance, or thread-local context: composition roots
  * and tests construct [[EffectContext]]s and authorize through them.
  *
  * A directly-constructed `AuthorityGate()` starts in `Mode.Audit` with no
  * policies. [[AuthorityGate.bootstrapped]] builds an Enforce gate with
  * allow-all policies (test / non-pack-loader wiring). PackLoader production
  * wiring uses [[EffectContext.forPackLoader]] instead.
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

  /** Check-then-run helper retained for non-handler call sites. Prefer
    * [[EffectContext.authorize]] → handler `perform(AuthorizedEffect)`.
    * Handlers must not call this (see ModuleBoundarySuite).
    */
  def checked[E, A](req: EffectRequest)(onDenied: String => E)(body: => Either[E, A]): Either[E, A] =
    check(req) match
      case Left(err) => Left(onDenied(err))
      case Right(_)  => body

object AuthorityGate:
  enum Mode:
    case Audit, Enforce

  private def bootstrapPolicies: List[EffectPolicy] =
    EffectMeta.allActionKeys.toList.map(k =>
      EffectPolicy(s"bootstrap-allow-${k.id}", "*", k, Resource("*", "*"), Decision.Allow))

  /** Test / non-pack-loader wiring helper: `Mode.Enforce` with one allow
    * policy per known [[cairn.kernel.Effects.ActionKey]] (derived + host-only), any
    * subject (`"*"`), any resource. Still used for ledger/process/LSP and
    * suites that do not exercise path-scoped denial. PackLoader production
    * uses [[EffectContext.forPackLoader]].
    *
    * Each call returns a **fresh** gate — never a shared singleton.
    */
  def bootstrapped(): AuthorityGate =
    enforcing(bootstrapPolicies)

  /** Fresh Enforce gate with the given policies. */
  def enforcing(policies: List[EffectPolicy]): AuthorityGate =
    val gate = new AuthorityGate()
    gate.install(policies)
    gate.setMode(Mode.Enforce)
    gate
