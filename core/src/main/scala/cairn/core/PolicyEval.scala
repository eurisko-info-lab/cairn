package cairn.core

import cairn.kernel.Authority
import cairn.kernel.Authority.*
import cairn.kernel.{EffectMeta, Effects}

/** Core policy evaluation and capability attenuation (Phase 4).
  * Kernel [[Authority.validate]] independently checks every proposal.
  *
  * Policies reference [[Effects.ActionKey]] / [[EffectMeta.ResourceSchema]]
  * derived from effect-interface artifacts.
  */
object PolicyEval:

  /** Propose an authorization derivation for `req` under `policies`. */
  def propose(req: EffectRequest, policies: List[EffectPolicy]): AuthorizationDerivation =
    Authority.decide(req, policies)

  /** Attenuate a grant to a narrower resource scope (Core optimization). */
  def attenuate(grant: CapabilityGrant, narrower: Resource): Either[String, CapabilityGrant] =
    if narrower.matches(grant.resource) || grant.resource.matches(narrower) then
      // narrower path must be at least as specific
      if grant.resource.path == "*" || narrower.path.startsWith(grant.resource.path.stripSuffix("*"))
         || narrower.path == grant.resource.path then
        Right(grant.attenuate(narrower))
      else Left(s"cannot attenuate ${grant.resource.path} to ${narrower.path}")
    else Left(s"resource kind mismatch: ${grant.resource.kind} vs ${narrower.kind}")

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
