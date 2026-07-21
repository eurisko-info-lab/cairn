package cairn.kernel

/** Phase 4–5 authority models. Handlers must eventually accept only
  * [[AuthorizedRequest]]; Core constructs [[AuthorizationProof]]; Kernel
  * [[checkProof]]s the witness (does not re-run [[decide]] as acceptance).
  *
  * Distinct from branch-policy CSTs (`ArtifactKind.Policy` language packs) and
  * language capability manifests (`ArtifactKind.Capability` / `core.Capabilities`).
  *
  * Action vocabulary is [[Effects.ActionKey]], derived from [[EffectMeta]]
  * effect-interface artifacts (with [[Effects.Action]] as host bridge).
  *
  * Calculus dimensions (priority #5): policy [[EffectPolicy.conditions]], grant
  * expiry / nonce, replay sets, [[Delegation]] chains, and Kernel-minted
  * [[AttenuationWitness]]. Priority #6: Core-generated structured proofs.
  */
object Authority:

  /** Subject identity. */
  final case class Subject(id: String)

  /** Resource scope for an effect. Prefer constructing via
    * [[EffectMeta.ResourceSchema.at]] so `kind` comes from the interface.
    */
  final case class Resource(kind: String, path: String):
    /** Whether this concrete resource is allowed by `pattern`.
      * Exact path, full `*`, or explicit prefix pattern ending in `*` — never
      * accidental prefix of a non-wildcard path (`/tmp/a` must not match `/tmp/abc`).
      */
    def matches(pattern: Resource): Boolean =
      (pattern.kind == "*" || pattern.kind == kind) &&
      (pattern.path == "*" ||
        pattern.path == path ||
        (pattern.path.endsWith("*") && path.startsWith(pattern.path.dropRight(1))))

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

    /** Evaluate conditions fail-closed. Known `meta:*` keys validate **value**
      * shape too (malformed expiry ⇒ no match ⇒ no unlimited grant).
      */
    def conditionsHold(req: EffectRequest): Boolean =
      conditions.forall { case (key, expected) =>
        if key.startsWith("meta:") then knownMetaCondition(key, expected)
        else req.args.get(key).contains(expected)
      }

    def metaExpiresAt: Option[Long] =
      conditions.get("meta:expiresAtEpochMillis").flatMap(s => s.toLongOption)

    def metaNonce: Option[String] =
      conditions.get("meta:nonce").filter(_.nonEmpty)

    def canon: Canon = Canon.cmap(
      "id" -> Canon.CStr(id),
      "subject" -> Canon.CStr(subject match { case "*" => "*"; case s: Subject => s.id }),
      "action" -> Canon.CStr(action match { case "*" => "*"; case a: Effects.ActionKey => a.id }),
      "resource" -> Canon.cmap("kind" -> Canon.CStr(resource.kind), "path" -> Canon.CStr(resource.path)),
      "decision" -> Canon.CStr(decision.match { case Decision.Allow => "allow"; case Decision.Deny => "deny" }),
      "conditions" -> Canon.cmap(conditions.toSeq.map((k, v) => k -> Canon.CStr(v))*))

  /** Known meta keys fail closed on malformed values (not just unknown keys). */
  private def knownMetaCondition(key: String, value: String): Boolean =
    key match
      case "meta:expiresAtEpochMillis" => value.toLongOption.isDefined
      case "meta:nonce"                => value.nonEmpty
      case _                           => false

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

    /** Check non-widening attenuation and mint a witness on success.
      * Provenance: [[CapabilityGrant.parentCanon]] must equal `parent.canon`.
      * Subject may change only when `child.delegatedBy` names the parent subject
      * (ordinary attenuation keeps subject equal; Delegation validates hops).
      */
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
      else if child.parentCanon != Some(parent.canon) then
        Left("attenuation parentCanon mismatch")
      else if child.subject != parent.subject && child.delegatedBy != Some(parent.subject) then
        Left("attenuation cannot change subject without delegation")
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

  /** Structured Core-generated witness that a request is authorized.
    *
    * Kernel [[checkProof]] validates this algebraically against the policy
    * store: cited allows match, conditions/expiry/nonce obligations hold at
    * [[nowMillis]], grant covers the request, optional attenuation/delegation
    * witnesses check, and no store Deny matches. Acceptance is not
    * "re-run [[decide]] and compare".
    */
  final case class AuthorizationProof(
      request: EffectRequest,
      nowMillis: Long,
      /** Allow policies cited as matching; each must be in the store. */
      allowPolicies: List[EffectPolicy],
      grant: CapabilityGrant,
      /** Non-meta condition keys → request arg values that satisfied them. */
      conditionEvidence: Map[String, String],
      /** When set, [[grant]] is a Kernel-checked attenuation of the parent. */
      attenuatedFrom: Option[(CapabilityGrant, AttenuationWitness)] = None,
      /** Optional A→B→C… chain whose final child equals [[grant]]. */
      delegationChain: List[Delegation] = Nil):
    /** Full checked-witness canon — distinct proofs must not collide. */
    def canon: Canon = Canon.cmap(
      "request" -> request.canon,
      "now" -> Canon.CInt(nowMillis),
      "allows" -> Canon.CList(allowPolicies.map(_.canon)),
      "grant" -> grant.canon,
      "evidence" -> Canon.cmap(conditionEvidence.toSeq.map((k, v) => k -> Canon.CStr(v))*),
      "attenuatedFrom" -> attenuatedFrom.fold(Canon.CTag("none", Canon.CStr(""))) { (parent, w) =>
        Canon.CTag("some", Canon.cmap(
          "parent" -> parent.canon,
          "witnessParent" -> AttenuationWitness.parent(w).canon,
          "witnessChild" -> AttenuationWitness.child(w).canon))
      },
      "delegation" -> Canon.CList(delegationChain.map(delegationCanon)),
      "requestId" -> optStr(request.requestId))

    /** Audit/event view of this allow proof. */
    def asDerivation(store: List[EffectPolicy]): AuthorizationDerivation =
      AuthorizationDerivation(
        request, store, Decision.Allow, Some(grant),
        s"allow by ${allowPolicies.map(_.id).mkString(",")}")

  private def delegationCanon(d: Delegation): Canon = Canon.cmap(
    "grantor" -> Canon.CStr(d.grantor.id),
    "grantee" -> Canon.CStr(d.grantee.id),
    "parent" -> d.parent.canon,
    "child" -> d.child.canon,
    "witnessParent" -> AttenuationWitness.parent(d.witness).canon,
    "witnessChild" -> AttenuationWitness.child(d.witness).canon)

  /** Kernel-controlled authorization token — not publicly constructible. */
  opaque type AuthorizedRequest = EffectRequest
  object AuthorizedRequest:
    private[kernel] def mint(req: EffectRequest): AuthorizedRequest = req
    extension (a: AuthorizedRequest)
      def request: EffectRequest = a
      def action: Effects.ActionKey = a.action
      def resource: Resource = a.resource
      def subject: Subject = a.subject

  /** Kernel-minted capability for [[EffectContext]]-style grant bundles.
    *
    * Hosts may still construct [[CapabilityGrant]] values when building Core
    * proofs / delegation chains, but [[VerifiedCapability]] is only minted
    * after [[checkProof]] / [[checkCapability]] — so
    * `withCapabilities` cannot accept arbitrary forged grants.
    *
    * Residual: nonce / requestId replay sets remain gate-local
    * ([[cairn.systemhandler.AuthorityGate]]); a VerifiedCapability alone does
    * not carry a shared durable replay store.
    */
  opaque type VerifiedCapability = CapabilityGrant
  object VerifiedCapability:
    private[kernel] def mint(g: CapabilityGrant): VerifiedCapability = g

    /** Mint after a successful [[checkProof]] — sole public mint path.
      * A host-forged [[CapabilityGrant]] that merely [[CapabilityGrant.covers]]
      * a request is not enough (that would re-open forgery via `Resource("*","*")`).
      */
    def fromProof(
        proof: AuthorizationProof, store: List[EffectPolicy]
    ): Either[String, VerifiedCapability] =
      checkProof(proof, store).map(_ => mint(proof.grant))

    extension (c: VerifiedCapability)
      def grant: CapabilityGrant = c
      def covers(req: EffectRequest, nowMillis: Long): Boolean = c.covers(req, nowMillis)
      def subject: Subject = c.subject
      def action: Effects.ActionKey = c.action
      def resource: Resource = c.resource

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

  /** Independent Kernel check of a Core-proposed derivation.
    * Prefer [[checkProof]] for the primary authorize path.
    */
  def validate(
      req: EffectRequest,
      policies: List[EffectPolicy],
      derivation: AuthorizationDerivation,
      nowMillis: Long = Long.MaxValue
  ): Either[String, AuthorizedRequest] =
    if derivation.request != req then Left("derivation request mismatch")
    else if derivation.policies.map(_.id).sorted != policies.map(_.id).sorted then
      Left("derivation policy set mismatch")
    else derivation.decision match
      case Decision.Deny => Left(s"denied: ${derivation.reason}")
      case Decision.Allow =>
        derivation.grant match
          case None => Left("allow without grant")
          case Some(g) =>
            // Re-check via structured proof — do not accept by re-running decide.
            val proof = AuthorizationProof(
              request = req,
              nowMillis = nowMillis,
              allowPolicies = policies.filter(p => g.sourcePolicyIds.contains(p.id) && p.decision == Decision.Allow),
              grant = g,
              conditionEvidence = conditionEvidenceFor(req, policies.filter(p => g.sourcePolicyIds.contains(p.id))))
            checkProof(proof, policies)

  private def conditionEvidenceFor(req: EffectRequest, allows: List[EffectPolicy]): Map[String, String] =
    allows.iterator
      .flatMap(_.conditions.iterator)
      .collect { case (k, _) if !k.startsWith("meta:") => k -> req.args.getOrElse(k, "") }
      .toMap

  /** Check a Core-constructed [[AuthorizationProof]] against the policy store.
    *
    * Algorithm (no [[decide]] re-run as acceptance):
    *  1. Cited allow policies non-empty, present in store (id + equality), Decision.Allow
    *  2. Each cited allow matches the *policy subject* of the proof:
    *     root grantor (when [[delegationChain]] non-empty) else the request
    *  3. Fail-closed: no store Deny matches the **final** request
    *  4. Condition evidence matches cited policy non-meta conditions and
    *     policy-match request args (root grantor when delegated)
    *  5. When delegation/attenuation is present: **root grant** fully justified
    *     against cited policies (expiry, nonce, resource) before hop checks
    *  6. Final grant subject/action/sourcePolicyIds justified; direct grants
    *     also match expiry/nonce/resource exactly
    *  7. Grant [[CapabilityGrant.covers]] request at proof.nowMillis
    *  8. Optional attenuation / delegation witnesses via shared pure checkers
    */
  def checkProof(
      proof: AuthorizationProof,
      store: List[EffectPolicy]
  ): Either[String, AuthorizedRequest] =
    val req = proof.request
    val now = proof.nowMillis
    if proof.allowPolicies.isEmpty then Left("proof missing allow policies")
    else
      for
        _ <- checkCitedAllows(proof.allowPolicies, store, policyMatchRequest(proof))
        _ <- checkNoStoreDeny(store, req)
        _ <- checkConditionEvidence(proof)
        _ <- checkRootGrantJustification(proof)
        _ <- checkGrantJustification(proof)
        _ <- checkAttenuation(proof)
        _ <- checkDelegation(proof)
        _ <-
          if !proof.grant.covers(req, now) then Left("grant does not cover request")
          else if proof.grant.expiresAtEpochMillis.exists(_ < now) then Left("grant expired")
          else Right(())
      yield AuthorizedRequest.mint(req)

  /** Capability-first accept: covering grant already in hand — no policy re-eval. */
  def checkCapability(
      req: EffectRequest,
      grant: CapabilityGrant,
      nowMillis: Long
  ): Either[String, AuthorizedRequest] =
    if !grant.covers(req, nowMillis) then Left("capability does not cover request")
    else if grant.expiresAtEpochMillis.exists(_ < nowMillis) then Left("grant expired")
    else Right(AuthorizedRequest.mint(req))

  /** Alias for [[checkProof]]; uses [[AuthorizationProof.nowMillis]] unless overridden. */
  def validateProof(
      proof: AuthorizationProof,
      policies: List[EffectPolicy],
      nowMillis: Long = Long.MaxValue
  ): Either[String, AuthorizedRequest] =
    if nowMillis != Long.MaxValue && nowMillis != proof.nowMillis then
      checkProof(proof.copy(nowMillis = nowMillis), policies)
    else checkProof(proof, policies)

  /** Request shape against which cited root policies are matched.
    * With a delegation chain, policies justify the **root grantor**, not the
    * final grantee (Carol); the final grant must still [[CapabilityGrant.covers]]
    * the real request.
    */
  private def policyMatchRequest(proof: AuthorizationProof): EffectRequest =
    if proof.delegationChain.nonEmpty then
      val root = proof.delegationChain.head.parent
      proof.request.copy(subject = root.subject, action = root.action, resource = root.resource)
    else proof.request

  private def checkCitedAllows(
      cited: List[EffectPolicy],
      store: List[EffectPolicy],
      matchReq: EffectRequest
  ): Either[String, Unit] =
    cited.foldLeft[Either[String, Unit]](Right(())) {
      case (Left(e), _) => Left(e)
      case (Right(()), p) =>
        if p.decision != Decision.Allow then Left(s"cited policy ${p.id} is not Allow")
        else
          store.find(_.id == p.id) match
            case None => Left(s"cited policy ${p.id} not in store")
            case Some(inStore) if inStore != p => Left(s"cited policy ${p.id} does not match store")
            case Some(_) if !p.matches(matchReq) => Left(s"cited policy ${p.id} does not match request")
            case Some(_) => Right(())
    }

  private def checkNoStoreDeny(store: List[EffectPolicy], req: EffectRequest): Either[String, Unit] =
    val denies = store.filter(p => p.decision == Decision.Deny && p.matches(req))
    if denies.nonEmpty then Left(s"deny by ${denies.map(_.id).mkString(",")}")
    else Right(())

  private def checkConditionEvidence(proof: AuthorizationProof): Either[String, Unit] =
    val matchReq = policyMatchRequest(proof)
    val required =
      proof.allowPolicies.iterator
        .flatMap(_.conditions.iterator)
        .collect { case (k, expected) if !k.startsWith("meta:") => k -> expected }
        .toMap
    required.foldLeft[Either[String, Unit]](Right(())) {
      case (Left(e), _) => Left(e)
      case (Right(()), (k, expected)) =>
        if !proof.conditionEvidence.get(k).contains(expected) then
          Left(s"condition evidence missing or wrong for $k")
        else if !matchReq.args.get(k).contains(expected) then
          Left(s"request args do not satisfy condition $k")
        else Right(())
    }

  /** When hops are present, justify the root/parent against cited policies
    * (expiry, nonce, non-widening resource) **before** hop validation.
    * Delegation roots must match the root request resource exactly;
    * attenuation parents may be broader than the final request but must
    * still be policy-bound (no widened TTL / omitted policy nonce).
    */
  private def checkRootGrantJustification(proof: AuthorizationProof): Either[String, Unit] =
    val allows = proof.allowPolicies
    if proof.delegationChain.nonEmpty then
      val root = proof.delegationChain.head.parent
      val matchReq = policyMatchRequest(proof)
      rootPolicyBound(root, allows, matchReq, proof.nowMillis, requireResourceEq = true)
    else proof.attenuatedFrom match
      case None => Right(())
      case Some((parent, _)) =>
        val matchReq = proof.request.copy(
          subject = parent.subject, action = parent.action, resource = parent.resource)
        rootPolicyBound(parent, allows, matchReq, proof.nowMillis, requireResourceEq = false)

  private def rootPolicyBound(
      root: CapabilityGrant,
      allows: List[EffectPolicy],
      matchReq: EffectRequest,
      nowMillis: Long,
      requireResourceEq: Boolean
  ): Either[String, Unit] =
    val expectedExpiry = allows.flatMap(_.metaExpiresAt).minOption
    val expectedNonce =
      matchReq.args.get("nonce").orElse(allows.flatMap(_.metaNonce).headOption)
    if root.subject != matchReq.subject then Left("root grant subject mismatch")
    else if root.action != matchReq.action then Left("root grant action mismatch")
    else if allows.exists(p => !root.resource.matches(p.resource)) then
      Left("root grant widens beyond cited policy resource")
    else if requireResourceEq && root.resource != matchReq.resource then
      Left("root grant resource mismatch")
    else if root.expiresAtEpochMillis != expectedExpiry then Left("root grant expiry mismatch")
    else if root.nonce != expectedNonce then Left("root grant nonce mismatch")
    else if expectedExpiry.exists(_ < nowMillis) then Left("root grant expired")
    else Right(())

  private def checkGrantJustification(proof: AuthorizationProof): Either[String, Unit] =
    val g = proof.grant
    val allows = proof.allowPolicies
    if g.subject != proof.request.subject then Left("grant subject mismatch")
    else if g.action != proof.request.action then Left("grant action mismatch")
    else if g.sourcePolicyIds.sorted != allows.map(_.id).sorted then Left("grant sourcePolicyIds mismatch")
    else if allows.exists(p => !g.resource.matches(p.resource)) then
      Left("grant widens beyond cited policy resource")
    else if proof.attenuatedFrom.isEmpty && proof.delegationChain.isEmpty then
      val expectedExpiry = allows.flatMap(_.metaExpiresAt).minOption
      val expectedNonce =
        proof.request.args.get("nonce").orElse(allows.flatMap(_.metaNonce).headOption)
      if g.resource != proof.request.resource then Left("grant resource mismatch")
      else if g.expiresAtEpochMillis != expectedExpiry then Left("grant expiry mismatch")
      else if g.nonce != expectedNonce then Left("grant nonce mismatch")
      else Right(())
    else Right(())

  private def checkAttenuation(proof: AuthorizationProof): Either[String, Unit] =
    proof.attenuatedFrom match
      case None => Right(())
      case Some((parent, witness)) =>
        AttenuationWitness.verify(witness, parent, proof.grant)

  private def checkDelegation(proof: AuthorizationProof): Either[String, Unit] =
    if proof.delegationChain.isEmpty then Right(())
    else
      Delegation.validateChain(proof.delegationChain).flatMap { finalGrant =>
        if finalGrant != proof.grant then Left("delegation chain does not end at proof grant")
        else Right(())
      }

  /** Deterministic policy decision: deny overrides allow; no match ⇒ deny.
    * Used by Core when constructing proofs / audit; not the Kernel accept path.
    */
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
