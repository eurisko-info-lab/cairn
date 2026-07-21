package cairn.kernel

/** Phase 4–5 authority models. Handlers must eventually accept only
  * [[AuthorizedRequest]]; Core proposes grants; Kernel validates.
  *
  * Distinct from branch-policy CSTs (`ArtifactKind.Policy` language packs) and
  * language capability manifests (`ArtifactKind.Capability` / `core.Capabilities`).
  *
  * Action vocabulary is [[Effects.ActionKey]], derived from [[EffectMeta]]
  * effect-interface artifacts (with [[Effects.Action]] as host bridge).
  */
object Authority:

  /** Subject identity. */
  final case class Subject(id: String)

  /** Resource scope for an effect. Prefer constructing via
    * [[EffectMeta.ResourceSchema.at]] so `kind` comes from the interface.
    */
  final case class Resource(kind: String, path: String):
    def matches(pattern: Resource): Boolean =
      (pattern.kind == "*" || pattern.kind == kind) &&
      (pattern.path == "*" || pattern.path == path || path.startsWith(pattern.path.stripSuffix("*")))

  final case class EffectRequest(
      subject: Subject,
      action: Effects.ActionKey,
      resource: Resource,
      args: Map[String, String] = Map.empty)

  enum Decision:
    case Allow, Deny

  /** User-authored effect policy (Phase 4). Deny overrides allow. */
  final case class EffectPolicy(
      id: String,
      subject: Subject | "*",
      action: Effects.ActionKey | "*",
      resource: Resource,
      decision: Decision,
      conditions: Map[String, String] = Map.empty):
    def matches(req: EffectRequest): Boolean =
      val subOk = subject match
        case "*"       => true
        case s: Subject => s.id == req.subject.id
      val actOk = action match
        case "*"                    => true
        case a: Effects.ActionKey   => a == req.action
      subOk && actOk && req.resource.matches(resource)

    def canon: Canon = Canon.cmap(
      "id" -> Canon.CStr(id),
      "subject" -> Canon.CStr(subject match { case "*" => "*"; case s: Subject => s.id }),
      "action" -> Canon.CStr(action match { case "*" => "*"; case a: Effects.ActionKey => a.id }),
      "resource" -> Canon.cmap("kind" -> Canon.CStr(resource.kind), "path" -> Canon.CStr(resource.path)),
      "decision" -> Canon.CStr(decision.match { case Decision.Allow => "allow"; case Decision.Deny => "deny" }))

  final case class CapabilityGrant(
      subject: Subject,
      action: Effects.ActionKey,
      resource: Resource,
      expiresAtEpochMillis: Option[Long] = None,
      nonce: Option[String] = None,
      delegationDepth: Int = 0,
      sourcePolicyIds: List[String] = Nil):
    def covers(req: EffectRequest, nowMillis: Long): Boolean =
      subject.id == req.subject.id &&
      action == req.action &&
      req.resource.matches(resource) &&
      expiresAtEpochMillis.forall(_ >= nowMillis) &&
      delegationDepth >= 0

    def attenuate(narrower: Resource, extraDepth: Int = 0): CapabilityGrant =
      copy(resource = narrower, delegationDepth = delegationDepth + extraDepth)

    def canon: Canon = Canon.cmap(
      "subject" -> Canon.CStr(subject.id),
      "action" -> Canon.CStr(action.id),
      "resource" -> Canon.cmap("kind" -> Canon.CStr(resource.kind), "path" -> Canon.CStr(resource.path)),
      "depth" -> Canon.CInt(delegationDepth),
      "policies" -> Canon.cstrs(sourcePolicyIds))

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
          expected.grant match
            case None => Left("allow without grant")
            case Some(g) if !g.covers(req, nowMillis) => Left("grant does not cover request")
            case Some(_) => Right(AuthorizedRequest.mint(req))

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
        val grant = CapabilityGrant(
          req.subject, req.action, req.resource,
          sourcePolicyIds = allows.map(_.id))
        AuthorizationDerivation(req, policies, Decision.Allow, Some(grant),
          s"allow by ${allows.map(_.id).mkString(",")}")
