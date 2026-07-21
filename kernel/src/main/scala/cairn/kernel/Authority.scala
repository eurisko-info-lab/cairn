package cairn.kernel

/** Phase 4–5 authority models. Handlers must eventually accept only
  * [[AuthorizedRequest]]; Core proposes grants; Kernel validates.
  *
  * Distinct from branch-policy CSTs (`ArtifactKind.Policy` language packs) and
  * language capability manifests (`ArtifactKind.Capability` / `core.Capabilities`).
  *
  * Action vocabulary is [[Effects.ActionKey]], derived from [[EffectMeta]]
  * effect-interface artifacts (with [[Effects.Action]] as host bridge).
  *
  * Calculus dimensions (priority #5): policy [[EffectPolicy.conditions]], grant
  * expiry / nonce, replay sets, [[Delegation]] chains, and Kernel-minted
  * [[AttenuationWitness]]. Priority #6 (Core-generated proof objects) is hooked
  * via [[AuthorizationProof]] but not yet the primary validate path.
  */
object Authority:

  /** Subject identity. */
  final case class Subject(id: String)

  /** Resource scope for an effect. Prefer constructing via
    * [[EffectMeta.ResourceSchema.at]] so `kind` comes from the interface.
    */
  final case class Resource(kind: String, path: String):
    /** Whether this concrete resource is allowed by `pattern` (exact / prefix / `*`). */
    def matches(pattern: Resource): Boolean =
      (pattern.kind == "*" || pattern.kind == kind) &&
      (pattern.path == "*" || pattern.path == path || path.startsWith(pattern.path.stripSuffix("*")))

    /** True when `this` is at least as narrow as `broader` (non-widening). */
    def isAttenuationOf(broader: Resource): Boolean =
      matches(broader)

  final case class EffectRequest(
      subject: Subject,
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty,
      /** One-shot request identity for replay protection (optional). */
      requestId: Option[String] = None):
    def canon: Canon = Canon.cmap(
      "subject" -> Canon.CStr(subject.id),
      "action" -> Canon.CStr(action.id),
      "resource" -> Canon.cmap("kind" -> Canon.CStr(resource.kind), "path" -> Canon.CStr(resource.path)),
      "args" -> Canon.cmap(args.toSeq.map((k, v) => k -> Canon.CStr(v))*),
      "requestId" -> optStr(requestId))

  enum Decision:
    case Allow, Deny

  /** User-authored effect policy (Phase 4). Deny overrides allow.
    *
    * [[conditions]] keys:
    *   - `meta:*` — grant-construction hints (ignored for match; see [[metaExpiresAt]],
    *     [[metaNonce]]); unknown `meta:` keys fail closed at match time.
    *   - any other key — exact match against [[EffectRequest.args]] (missing ⇒ fail closed).
    */
  final case class EffectPolicy(
      id: String,
      subject: Subject | "*",
      action: Effects.ActionKey | "*",
      resource: Resource,
      decision: Decision,
      conditions: Map[String, String] = Map.empty):
    def matches(req: EffectRequest): Boolean =
      val subOk = subject match
        case "*"        => true
        case s: Subject => s.id == req.subject.id
      val actOk = action match
        case "*"                  => true
        case a: Effects.ActionKey => a == req.action
      subOk && actOk && req.resource.matches(resource) && conditionsHold(req)

    /** Evaluate conditions fail-closed. */
    def conditionsHold(req: EffectRequest): Boolean =
      conditions.forall { case (key, expected) =>
        if key.startsWith("meta:") then knownMetaCondition(key)
        else req.args.get(key).contains(expected)
      }

    def metaExpiresAt: Option[Long] =
      conditions.get("meta:expiresAtEpochMillis").flatMap(s => s.toLongOption)

    def metaNonce: Option[String] =
      conditions.get("meta:nonce")

    def canon: Canon = Canon.cmap(
      "id" -> Canon.CStr(id),
      "subject" -> Canon.CStr(subject match { case "*" => "*"; case s: Subject => s.id }),
      "action" -> Canon.CStr(action match { case "*" => "*"; case a: Effects.ActionKey => a.id }),
      "resource" -> Canon.cmap("kind" -> Canon.CStr(resource.kind), "path" -> Canon.CStr(resource.path)),
      "decision" -> Canon.CStr(decision.match { case Decision.Allow => "allow"; case Decision.Deny => "deny" }),
      "conditions" -> Canon.cmap(conditions.toSeq.map((k, v) => k -> Canon.CStr(v))*))

  private def knownMetaCondition(key: String): Boolean =
    key == "meta:expiresAtEpochMillis" || key == "meta:nonce"

  final case class CapabilityGrant(
      subject: Subject,
      action: Effects.ActionKey,
      resource: Resource,
      expiresAtEpochMillis: Option[Long] = None,
      nonce: Option[String] = None,
      delegationDepth: Int = 0,
      sourcePolicyIds: List[String] = Nil,
      /** Canonical form of the parent grant when this was delegated/attenuated. */
      parentCanon: Option[Canon] = None,
      delegatedBy: Option[Subject] = None):
    def covers(req: EffectRequest, nowMillis: Long): Boolean =
      subject.id == req.subject.id &&
      action == req.action &&
      req.resource.matches(resource) &&
      expiresAtEpochMillis.forall(_ >= nowMillis) &&
      delegationDepth >= 0

    /** Narrow resource / bump depth; does not mint a witness — use
      * [[AttenuationWitness.check]] or Core [[cairn.core.PolicyEval.attenuate]].
      * Child nonce is cleared (must not reuse parent one-shot nonce). */
    def attenuate(narrower: Resource, extraDepth: Int = 0): CapabilityGrant =
      copy(
        resource = narrower,
        delegationDepth = delegationDepth + extraDepth,
        parentCanon = Some(canon),
        nonce = None)

    def canon: Canon = Canon.cmap(
      "subject" -> Canon.CStr(subject.id),
      "action" -> Canon.CStr(action.id),
      "resource" -> Canon.cmap("kind" -> Canon.CStr(resource.kind), "path" -> Canon.CStr(resource.path)),
      "depth" -> Canon.CInt(delegationDepth),
      "policies" -> Canon.cstrs(sourcePolicyIds),
      "expiresAt" -> optLong(expiresAtEpochMillis),
      "nonce" -> optStr(nonce),
      "parent" -> parentCanon.fold(Canon.CTag("none", Canon.CStr("")))(c => Canon.CTag("some", c)),
      "delegatedBy" -> delegatedBy.fold(Canon.CTag("none", Canon.CStr("")))(s =>
        Canon.CTag("some", Canon.CStr(s.id))))

  /** Kernel-checked proof that `child` does not widen `parent`. */
  opaque type AttenuationWitness = (CapabilityGrant, CapabilityGrant)
  object AttenuationWitness:
    def parent(w: AttenuationWitness): CapabilityGrant = w._1
    def child(w: AttenuationWitness): CapabilityGrant = w._2

    /** Check non-widening attenuation and mint a witness on success. */
    def check(parent: CapabilityGrant, child: CapabilityGrant): Either[String, AttenuationWitness] =
      if child.action != parent.action then
        Left(s"attenuation action mismatch: ${parent.action.id} → ${child.action.id}")
      else if !child.resource.isAttenuationOf(parent.resource) then
        Left(s"attenuation widens resource: ${parent.resource} → ${child.resource}")
      else if !expiryAttenuated(parent.expiresAtEpochMillis, child.expiresAtEpochMillis) then
        Left("attenuation widens expiry (child must expire no later than parent)")
      else if child.delegationDepth < parent.delegationDepth then
        Left("attenuation decreases delegation depth")
      else if parent.nonce.isDefined && child.nonce == parent.nonce then
        Left("attenuated child must not reuse parent nonce")
      else Right((parent, child))

    def verify(w: AttenuationWitness, parent: CapabilityGrant, child: CapabilityGrant): Either[String, Unit] =
      if w._1 != parent || w._2 != child then Left("attenuation witness mismatch")
      else check(parent, child).map(_ => ())

  private def expiryAttenuated(parent: Option[Long], child: Option[Long]): Boolean =
    (parent, child) match
      case (None, _)          => true
      case (Some(_), None)    => false
      case (Some(p), Some(c)) => c <= p

  /** One hop: grantor attenuates a grant to grantee. */
  final case class Delegation(
      grantor: Subject,
      grantee: Subject,
      parent: CapabilityGrant,
      child: CapabilityGrant,
      witness: AttenuationWitness)

  object Delegation:
    def validate(d: Delegation): Either[String, Unit] =
      if d.parent.subject != d.grantor then Left("delegation grantor mismatch")
      else if d.child.subject != d.grantee then Left("delegation grantee mismatch")
      else if d.child.delegationDepth != d.parent.delegationDepth + 1 then
        Left(s"delegation depth: expected ${d.parent.delegationDepth + 1}, got ${d.child.delegationDepth}")
      else if d.child.delegatedBy != Some(d.grantor) then Left("delegatedBy mismatch")
      else AttenuationWitness.verify(d.witness, d.parent, d.child)

    /** Validate an A→B→C… chain; each hop's grantor must be the previous grantee. */
    def validateChain(links: List[Delegation]): Either[String, CapabilityGrant] =
      if links.isEmpty then Left("empty delegation chain")
      else
        links.zipWithIndex.foldLeft[Either[String, CapabilityGrant]](Right(links.head.parent)) {
          case (Left(err), _) => Left(err)
          case (Right(expectedParent), (link, i)) =>
            if link.parent != expectedParent && i > 0 then
              Left(s"delegation chain break at hop $i")
            else if i > 0 && link.grantor != links(i - 1).grantee then
              Left(s"delegation chain subject break at hop $i")
            else validate(link).map(_ => link.child)
        }

  /** Pure replay-set algebra. Gate / runtime holds the mutable set. */
  object Replay:
    def consume(token: String, used: Set[String], kind: String): Either[String, Set[String]] =
      if token.isEmpty then Left(s"empty $kind")
      else if used.contains(token) then Left(s"$kind replay: $token")
      else Right(used + token)

    def consumeNonce(nonce: String, used: Set[String]): Either[String, Set[String]] =
      consume(nonce, used, "nonce")

    def consumeRequestId(id: String, used: Set[String]): Either[String, Set[String]] =
      consume(id, used, "requestId")

  final case class AuthorizationDerivation(
      request: EffectRequest,
      policies: List[EffectPolicy],
      decision: Decision,
      grant: Option[CapabilityGrant],
      reason: String):
    def canon: Canon = Canon.cmap(
      "decision" -> Canon.CStr(decision.match { case Decision.Allow => "allow"; case Decision.Deny => "deny" }),
      "reason" -> Canon.CStr(reason),
      "grant" -> grant.fold(Canon.CTag("none", Canon.CStr("")))(g => Canon.CTag("some", g.canon)))

  /** Hook for priority #6: Core-generated, Kernel-checked auth proof objects.
    * Today Core still emits [[AuthorizationDerivation]] and Kernel re-decides;
    * [[validateProof]] bridges until proofs replace re-decide.
    */
  trait AuthorizationProof:
    def derivation: AuthorizationDerivation
    def canon: Canon = derivation.canon

  final case class DerivationAsProof(derivation: AuthorizationDerivation) extends AuthorizationProof

  /** Kernel-controlled authorization token — not publicly constructible. */
  opaque type AuthorizedRequest = EffectRequest
  object AuthorizedRequest:
    private[kernel] def mint(req: EffectRequest): AuthorizedRequest = req
    extension (a: AuthorizedRequest)
      def request: EffectRequest = a
      def action: Effects.ActionKey = a.action
      def resource: Resource = a.resource
      def subject: Subject = a.subject

  /** Audit-mode pass-through — records intent without enforcement. Prefer
    * [[validate]] under Enforce mode. */
  def auditPass(req: EffectRequest): AuthorizedRequest = AuthorizedRequest.mint(req)

  enum AuthorityEvent:
    case Audited(derivation: AuthorizationDerivation, wouldPermit: Boolean)
    case Enforced(derivation: AuthorizationDerivation, authorized: Option[AuthorizedRequest])
    case Rejected(derivation: AuthorizationDerivation)

  private def optStr(o: Option[String]): Canon =
    o.fold(Canon.CTag("none", Canon.CStr("")))(s => Canon.CTag("some", Canon.CStr(s)))

  private def optLong(o: Option[Long]): Canon =
    o.fold(Canon.CTag("none", Canon.CStr("")))(n => Canon.CTag("some", Canon.CInt(n)))

  /** Independent Kernel check of a Core-proposed derivation. */
  def validate(
      req: EffectRequest,
      policies: List[EffectPolicy],
      derivation: AuthorizationDerivation,
      nowMillis: Long = Long.MaxValue
  ): Either[String, AuthorizedRequest] =
    if derivation.request != req then Left("derivation request mismatch")
    else if derivation.policies.map(_.id).sorted != policies.map(_.id).sorted then
      Left("derivation policy set mismatch")
    else
      val expected = decide(req, policies)
      if expected.decision != derivation.decision then
        Left(s"decision mismatch: kernel=${expected.decision}, derivation=${derivation.decision}")
      else expected.decision match
        case Decision.Deny => Left(s"denied: ${expected.reason}")
        case Decision.Allow =>
          (expected.grant, derivation.grant) match
            case (None, _) => Left("allow without grant")
            case (Some(kernelGrant), None) => Left("allow without grant")
            case (Some(kernelGrant), Some(coreGrant)) =>
              validateProposedGrant(req, kernelGrant, coreGrant, nowMillis).map(_ =>
                AuthorizedRequest.mint(req))

  /** Accept Core's grant when it equals Kernel's, or is a Kernel-witnessed
    * attenuation that still covers the request (no widening). */
  private def validateProposedGrant(
      req: EffectRequest,
      kernelGrant: CapabilityGrant,
      coreGrant: CapabilityGrant,
      nowMillis: Long
  ): Either[String, Unit] =
    if kernelGrant.expiresAtEpochMillis.exists(_ < nowMillis) then Left("grant expired")
    else if !kernelGrant.covers(req, nowMillis) then Left("grant does not cover request")
    else if coreGrant == kernelGrant then Right(())
    else if coreGrant.canon == kernelGrant.canon then Right(())
    else
      AttenuationWitness.check(kernelGrant, coreGrant).flatMap { _ =>
        if coreGrant.expiresAtEpochMillis.exists(_ < nowMillis) then Left("grant expired")
        else if !coreGrant.covers(req, nowMillis) then Left("attenuated grant does not cover request")
        else Right(())
      }

  /** Bridge for priority #6 proofs — currently delegates to [[validate]]. */
  def validateProof(
      proof: AuthorizationProof,
      policies: List[EffectPolicy],
      nowMillis: Long = Long.MaxValue
  ): Either[String, AuthorizedRequest] =
    validate(proof.derivation.request, policies, proof.derivation, nowMillis)

  /** Deterministic policy decision: deny overrides allow; no match ⇒ deny. */
  def decide(req: EffectRequest, policies: List[EffectPolicy]): AuthorizationDerivation =
    val matched = policies.filter(_.matches(req))
    val denies = matched.filter(_.decision == Decision.Deny)
    if denies.nonEmpty then
      AuthorizationDerivation(req, policies, Decision.Deny, None,
        s"deny by ${denies.map(_.id).mkString(",")}")
    else
      val allows = matched.filter(_.decision == Decision.Allow)
      if allows.isEmpty then
        AuthorizationDerivation(req, policies, Decision.Deny, None, "no matching allow policy")
      else
        val expires = allows.flatMap(_.metaExpiresAt).minOption
        val nonce = req.args.get("nonce").orElse(allows.flatMap(_.metaNonce).headOption)
        val grant = CapabilityGrant(
          req.subject, req.action, req.resource,
          expiresAtEpochMillis = expires,
          nonce = nonce,
          sourcePolicyIds = allows.map(_.id))
        AuthorizationDerivation(req, policies, Decision.Allow, Some(grant),
          s"allow by ${allows.map(_.id).mkString(",")}")
