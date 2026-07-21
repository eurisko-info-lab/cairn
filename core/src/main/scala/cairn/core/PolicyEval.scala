package cairn.core

import cairn.kernel.Authority
import cairn.kernel.Authority.*
import cairn.kernel.{EffectMeta, Effects}

/** Core policy evaluation and capability attenuation (Phase 4).
  * Kernel [[Authority.validate]] independently checks every proposal.
  *
  * Policies reference [[Effects.ActionKey]] / [[EffectMeta.ResourceSchema]]
  * derived from effect-interface artifacts.
  *
  * Attenuation and delegation are Core optimizations; Kernel mints/checks
  * [[AttenuationWitness]] so child grants cannot widen parent authority.
  * Priority #6 will replace re-decide with Core-generated [[AuthorizationProof]]s.
  */
object PolicyEval:

  /** Propose an authorization derivation for `req` under `policies`. */
  def propose(req: EffectRequest, policies: List[EffectPolicy]): AuthorizationDerivation =
    Authority.decide(req, policies)

  /** Wrap a derivation as a priority-#6 proof hook (still re-checked via decide). */
  def asProof(derivation: AuthorizationDerivation): AuthorizationProof =
    DerivationAsProof(derivation)

  /** Attenuate a grant to a narrower resource scope. Returns the child grant
    * plus a Kernel-checked [[AttenuationWitness]] (fails if widening).
    */
  def attenuate(
      grant: CapabilityGrant,
      narrower: Resource
  ): Either[String, (CapabilityGrant, AttenuationWitness)] =
    if !narrower.isAttenuationOf(grant.resource) then
      Left(s"cannot attenuate ${grant.resource.path} to ${narrower.path}")
    else
      val child = grant.attenuate(narrower)
      AttenuationWitness.check(grant, child).map(w => (child, w))

  /** Delegate `grant` from its subject to `grantee` under a (possibly narrower)
    * resource. Validates the hop as a [[Delegation]] chain link.
    */
  def delegate(
      grant: CapabilityGrant,
      grantee: Subject,
      narrower: Resource
  ): Either[String, Delegation] =
    if !narrower.isAttenuationOf(grant.resource) then
      Left(s"cannot delegate: resource widens ${grant.resource.path} → ${narrower.path}")
    else
      val child = grant.copy(
        subject = grantee,
        resource = narrower,
        delegationDepth = grant.delegationDepth + 1,
        delegatedBy = Some(grant.subject),
        parentCanon = Some(grant.canon),
        nonce = None)
      AttenuationWitness.check(grant, child).flatMap { w =>
        val d = Delegation(grant.subject, grantee, grant, child, w)
        Delegation.validate(d).map(_ => d)
      }

  /** Convenience: build a single-subject allow-all audit policy for a key. */
  def allowAll(id: String, subject: Subject, action: Effects.ActionKey): EffectPolicy =
    EffectPolicy(id, subject, action, Resource("*", "*"), Decision.Allow)

  def denyAll(id: String, subject: Subject, action: Effects.ActionKey): EffectPolicy =
    EffectPolicy(id, subject, action, Resource("*", "*"), Decision.Deny)

  /** Narrow policies for the PackLoader / Workspace language-pack workflow:
    * workspace `read` only (derived from [[EffectMeta.workspace]]), under
    * `languages*` for a single subject. Denies other actions, subjects, and paths.
    */
  def packLoaderWorkspace(subject: Subject): List[EffectPolicy] =
    List(EffectPolicy(
      "pack-loader-workspace-read",
      subject,
      EffectMeta.workspace.actionKey("read"),
      EffectMeta.workspace.resource.at("languages*"),
      Decision.Allow))
