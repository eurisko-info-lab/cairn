package cairn.runtime

import cairn.core.PolicyEval
import cairn.kernel.Authority.*
import cairn.systeminterface.AuthorizationProver

/** App-layer [[AuthorizationProver]] backed by Core [[PolicyEval]].
  * Composition roots should inject this into [[cairn.systemhandler.AuthorityGate]]
  * so container code does not import PolicyEval directly once the temporary
  * [[cairn.systemhandler.AuthorityGate.DefaultProver]] adapter is removed.
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
