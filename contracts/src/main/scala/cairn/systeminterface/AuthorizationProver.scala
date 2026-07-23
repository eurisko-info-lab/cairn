package cairn.systeminterface

import cairn.kernel.Authority.*

/** Neutral authorization-proving contract shared by container and content.
  *
  * Container handlers ([[cairn.systemhandler.AuthorityGate]]) depend on this
  * interface — not on `cairn.core.PolicyEval`. The PolicyEval implementation
  * is supplied from the app/runtime composition root.
  */
trait AuthorizationProver:
  def propose(req: EffectRequest, policies: List[EffectPolicy]): AuthorizationDerivation
  def prove(
      req: EffectRequest,
      policies: List[EffectPolicy],
      nowMillis: Long,
  ): Either[String, AuthorizationProof]
