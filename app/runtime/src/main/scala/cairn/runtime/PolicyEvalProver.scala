package cairn.runtime

import cairn.core.PolicyEval
import cairn.kernel.Authority.*
import cairn.systeminterface.AuthorizationProver

/** App-layer [[AuthorizationProver]] backed by Core [[PolicyEval]].
  * Composition roots inject this into every
  * [[cairn.systemhandler.AuthorityGate]] construction — the `prover`
  * parameter has no default, so container code never imports `PolicyEval`
  * directly.
  */
object PolicyEvalProver extends AuthorizationProver:
  def propose(req: EffectRequest, policies: List[EffectPolicy]): AuthorizationDerivation =
    PolicyEval.propose(req, policies)

  def prove(
      req: EffectRequest,
      policies: List[EffectPolicy],
      nowMillis: Long,
  ): Either[String, AuthorizationProof] =
    PolicyEval.prove(req, policies, nowMillis)
