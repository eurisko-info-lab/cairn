package cairn.systemhandler

import cairn.kernel.Authority
import cairn.kernel.Authority.*
import cairn.systeminterface.AuthorizationProver

/** Authority gate (Phases 4–5, priority #2/#5/#6). Composition roots authorize via
  * [[EffectContext.authorize]] (which calls [[check]]); handlers accept only
  * [[AuthorizedEffect]] and must not call [[check]]/[[checked]] themselves.
  *
  * Enforce path: injected [[AuthorizationProver]] → Kernel [[Authority.checkProof]] →
  * mint [[AuthorizedRequest]]. Audit path records via [[audit]] /
  * [[Authority.auditPass]] → [[AuditedRequest]] — **never** mints
  * [[AuthorizedRequest]].
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
  * wiring uses [[cairn.runtime.EffectContexts.forPackLoader]] instead.
  *
  * Replay: successful Enforce authorizations consume grant [[CapabilityGrant.nonce]]
  * and [[EffectRequest.requestId]] via a shared issuer-scoped [[ReplayStore]]
  * (default in-memory; composition roots may inject a durable filesystem store).
  *
  * Revocation: [[checkCapability]] always consults the injected
  * [[RevocationView]] before minting.
  */
final class AuthorityGate(
    @volatile private var mode: AuthorityGate.Mode = AuthorityGate.Mode.Audit,
    @volatile private var policies: List[EffectPolicy] = Nil,
    private val replay: ReplayStore = ReplayStore.memory(),
    private val revocation: RevocationView = RevocationView.empty,
    private val prover: AuthorizationProver = AuthorityGate.DefaultProver,
):
  private val events = scala.collection.mutable.ListBuffer[AuthorityEvent]()

  def setMode(m: AuthorityGate.Mode): Unit = mode = m
  def currentMode: AuthorityGate.Mode = mode
  def install(ps: List[EffectPolicy]): Unit = policies = ps
  def clearPolicies(): Unit = policies = Nil
  def drainEvents(): List[AuthorityEvent] =
    synchronized { val out = events.toList; events.clear(); out }

  /** Underlying replay store (may be shared across gates). */
  def replayStore: ReplayStore = replay

  /** Revocation view consulted on capability authorize. */
  def revocationView: RevocationView = revocation

  /** Flattened snapshot of consumed nonces / request ids (tests). */
  def replayState: (Set[String], Set[String]) =
    val s = replay.snapshot
    (s.flatNonces, s.flatRequestIds)

  /** Record audit intent — returns [[AuditedRequest]], never Authorized. */
  def audit(req: EffectRequest, nowMillis: Long = System.currentTimeMillis())
      : AuditedRequest =
    val derivation = prover.propose(req, policies)
    val would = derivation.decision == Decision.Allow &&
      derivation.grant.exists(_.covers(req, nowMillis))
    synchronized { events += AuthorityEvent.Audited(derivation, would) }
    Authority.auditPass(req)

  /** Enforce-only authorization. Audit mode cannot mint [[AuthorizedRequest]].
    */
  def check(req: EffectRequest, nowMillis: Long = System.currentTimeMillis())
      : Either[String, AuthorizedRequest] =
    mode match
      case AuthorityGate.Mode.Audit =>
        val derivation = prover.propose(req, policies)
        val would = derivation.decision == Decision.Allow &&
          derivation.grant.exists(_.covers(req, nowMillis))
        synchronized { events += AuthorityEvent.Audited(derivation, would) }
        Left("audit mode cannot mint AuthorizedRequest; use audit() for recording")
      case AuthorityGate.Mode.Enforce =>
        prover.prove(req, policies, nowMillis) match
          case Left(err) =>
            val derivation = prover.propose(req, policies)
            synchronized { events += AuthorityEvent.Rejected(derivation) }
            Left(err)
          case Right(proof) =>
            Authority.checkProof(proof, policies) match
              case Left(err) =>
                synchronized { events += AuthorityEvent.Rejected(proof.asDerivation(policies)) }
                Left(err)
              case Right(auth) =>
                val derivation = proof.asDerivation(policies)
                synchronized {
                  consumeReplay(derivation, req) match
                    case Left(err) =>
                      events += AuthorityEvent.Rejected(derivation)
                      Left(err)
                    case Right(()) =>
                      events += AuthorityEvent.Enforced(derivation, Some(auth))
                      Right(auth)
                }

  /** Capability-first path: validate a covering [[VerifiedCapability]] without
    * policy prove. Raw [[CapabilityGrant]] is rejected at the type level —
    * only Kernel-minted verified caps authorize. Always consults [[revocation]]
    * before mint (Enforce). Audit mode records and refuses to mint AuthorizedRequest.
    */
  def checkCapability(
      req: EffectRequest,
      capability: VerifiedCapability,
      nowMillis: Long = System.currentTimeMillis()
  ): Either[String, AuthorizedRequest] =
    mode match
      case AuthorityGate.Mode.Audit =>
        val grant = capability.grant
        val would = grant.covers(req, nowMillis) && !revocation.isRevoked(grant.capabilityId)
        val derivation = AuthorizationDerivation(
          req, policies,
          if would then Decision.Allow else Decision.Deny,
          if would then Some(grant) else None,
          "capability")
        synchronized { events += AuthorityEvent.Audited(derivation, would) }
        Left("audit mode cannot mint AuthorizedRequest; use audit() for recording")
      case AuthorityGate.Mode.Enforce =>
        Authority.checkCapability(req, capability, nowMillis, revocation.isRevoked) match
          case Left(err) =>
            val derivation = AuthorizationDerivation(req, policies, Decision.Deny, None, err)
            synchronized { events += AuthorityEvent.Rejected(derivation) }
            Left(err)
          case Right(auth) =>
            val grant = capability.grant
            val derivation = AuthorizationDerivation(
              req, policies, Decision.Allow, Some(grant), "capability")
            synchronized {
              consumeReplay(derivation, req) match
                case Left(err) =>
                  events += AuthorityEvent.Rejected(derivation)
                  Left(err)
                case Right(()) =>
                  events += AuthorityEvent.Enforced(derivation, Some(auth))
                  Right(auth)
            }

  private def consumeReplay(derivation: AuthorizationDerivation, req: EffectRequest): Either[String, Unit] =
    val nonceIssuer = derivation.grant.map(_.subject.id).getOrElse(req.subject.id)
    val afterNonce =
      derivation.grant.flatMap(_.nonce) match
        case None    => Right(())
        case Some(n) => replay.consumeNonce(nonceIssuer, n)
    afterNonce.flatMap { _ =>
      req.requestId match
        case None     => Right(())
        case Some(id) => replay.consumeRequestId(req.subject.id, id)
    }

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

  /** Temporary adapter keeping PolicyEval reachable until Branches/MetaActivation
    * leave the handler and `systemHandler.dependsOn(core)` can be dropped.
    * Prefer [[cairn.runtime.PolicyEvalProver]] from app composition roots.
    */
  object DefaultProver extends AuthorizationProver:
    import cairn.core.PolicyEval
    def propose(req: EffectRequest, policies: List[EffectPolicy]): AuthorizationDerivation =
      PolicyEval.propose(req, policies)
    def prove(
        req: EffectRequest,
        policies: List[EffectPolicy],
        nowMillis: Long,
    ): Either[String, AuthorizationProof] =
      PolicyEval.prove(req, policies, nowMillis)

  private def bootstrapPolicies(registry: RuntimeEffectRegistry): List[EffectPolicy] =
    registry.allActionKeys.toList.map(k =>
      EffectPolicy(s"bootstrap-allow-${k.id}", "*", k, Resource("*", "*"), Decision.Allow))

  /** Test / non-pack-loader wiring helper: `Mode.Enforce` with one allow
    * policy per known [[Effects.ActionKey]] from [[registry]] (default seeds),
    * any subject (`"*"`), any resource. Still used for ledger/process/LSP and
    * suites that do not exercise path-scoped denial. PackLoader production
    * uses [[cairn.runtime.EffectContexts.forPackLoader]].
    *
    * Each call returns a **fresh** gate — never a shared singleton.
    */
  def bootstrapped(
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds,
      revocation: RevocationView = RevocationView.empty,
  ): AuthorityGate =
    bootstrapped(registry, revocation, DefaultProver)

  def bootstrapped(
      registry: RuntimeEffectRegistry,
      revocation: RevocationView,
      prover: AuthorizationProver,
  ): AuthorityGate =
    enforcing(bootstrapPolicies(registry), ReplayStore.memory(), revocation, prover)

  /** Fresh Enforce gate with the given policies (private in-memory replay). */
  def enforcing(policies: List[EffectPolicy]): AuthorityGate =
    enforcing(policies, ReplayStore.memory(), RevocationView.empty, DefaultProver)

  /** Fresh Enforce gate with policies + revocation view. */
  def enforcing(policies: List[EffectPolicy], revocation: RevocationView): AuthorityGate =
    enforcing(policies, ReplayStore.memory(), revocation, DefaultProver)

  /** Fresh Enforce gate sharing an issuer-scoped [[ReplayStore]]. */
  def enforcing(policies: List[EffectPolicy], replay: ReplayStore): AuthorityGate =
    enforcing(policies, replay, RevocationView.empty, DefaultProver)

  /** Fresh Enforce gate with shared replay + revocation. */
  def enforcing(
      policies: List[EffectPolicy],
      replay: ReplayStore,
      revocation: RevocationView,
  ): AuthorityGate =
    enforcing(policies, replay, revocation, DefaultProver)

  def enforcing(
      policies: List[EffectPolicy],
      replay: ReplayStore,
      revocation: RevocationView,
      prover: AuthorizationProver,
  ): AuthorityGate =
    new AuthorityGate(Mode.Enforce, policies, replay, revocation, prover)

  /** Fresh Enforce allow-all gate over a shared replay store. */
  def bootstrapped(
      replay: ReplayStore,
      registry: RuntimeEffectRegistry,
      revocation: RevocationView,
  ): AuthorityGate =
    bootstrapped(replay, registry, revocation, DefaultProver)

  def bootstrapped(
      replay: ReplayStore,
      registry: RuntimeEffectRegistry,
      revocation: RevocationView,
      prover: AuthorizationProver,
  ): AuthorityGate =
    enforcing(bootstrapPolicies(registry), replay, revocation, prover)

  def bootstrapped(replay: ReplayStore): AuthorityGate =
    bootstrapped(replay, RuntimeEffectRegistry.seeds, RevocationView.empty, DefaultProver)
