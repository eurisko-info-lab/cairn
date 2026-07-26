package cairn.core

import cairn.kernel.*

/** What must additionally hold, beyond ΔL replay, before a proposed module
  * may become a branch's new head. [[AcceptancePolicy.open]] is
  * passthrough — always available, but a NAMED, visible choice a caller
  * must make explicitly; there is no default policy parameter anywhere in
  * [[cairn.runtime.Branches]]' acceptance API, so skipping domain
  * validation can no longer happen by omission.
  *
  * Scope note: this governs the domain-invariant ([[ModuleGate]]) dimension
  * of acceptance specifically. Domain ancestry ([[DomainAgreement]]) and
  * certificate/approval requirements are separate, existing mechanisms not
  * yet folded into one policy object — a further unification, not
  * attempted here.
  *
  * [[digest]] folds in [[ModuleGate.descriptor]] when the gate has one
  * (built via [[ModuleGate.fromSpecs]] from a canonically-encodable
  * [[ModuleStructural.Spec]] list) — two gates checking the same specs are
  * then provably the same policy, not just same-named. Gates with no
  * descriptor (an open-ended host closure, built via [[ModuleGate.host]] /
  * [[ModuleGate.fromJudgment]]) keep the older, weaker identity: two such
  * gates sharing a judgment string are indistinguishable by digest, and a
  * verifying node re-runs whichever gate function it locally supplies for
  * that name rather than confirming it matches the accepting node's.
  */
final case class AcceptancePolicy(gate: ModuleGate):
  def digest: Digest = Digest.of(Canon.cmap(
    "judgment" -> Canon.CStr(gate.judgment),
    "descriptor" -> gate.descriptor.fold(Canon.CTag("none", Canon.CInt(0)))(d =>
      Canon.CTag("some", Canon.CStr(d.hex)))))

object AcceptancePolicy:
  val open: AcceptancePolicy = AcceptancePolicy(ModuleGate.passthrough)
  def gated(gate: ModuleGate): AcceptancePolicy = AcceptancePolicy(gate)

/** Proof that a specific (language, base, change, result) transition was
  * checked against a specific [[AcceptancePolicy]] — what a second node
  * needs to independently replay a transition and re-run a policy against
  * it, rather than trusting a self-reported success. Referenced from
  * `BranchManifest.gateEvidence`. When the accepting node's gate has a
  * [[ModuleGate.descriptor]] (see [[ModuleGate.fromSpecs]]), `policy`
  * genuinely identifies the check's semantics, not just its judgment name —
  * a verifying node supplying a gate with a different descriptor fails the
  * policy-identity comparison in [[AcceptancePolicy.digest]] before it even
  * re-runs the check. Without a descriptor (an open-ended host closure),
  * replay confirms only that the transition really produced `result` and
  * that a gate named `judgment` accepted it — not that the verifying
  * node's locally-supplied gate has the same semantics the accepting node
  * used.
  *
  * `validatedChangeSet` is the digest of the real
  * [[Delta.ValidatedChangeSet]]'s own artifact (`vcs.artifact.digest`) —
  * the SAME digest [[cairn.runtime.Branches]] already records as
  * `BranchManifest.acceptedChange` — never a bespoke digest of the raw
  * change term alone; a raw-term digest doesn't bind language/base/result,
  * so it can't be replayed against. `None` when the accept had no
  * underlying ΔL change at all (a pure `importModule` bootstrap, or a
  * fast-forward of one).
  */
final case class AcceptanceEvidence(
    language: Digest,
    base: Digest,
    validatedChangeSet: Option[Digest],
    result: Digest,
    policy: Digest,
    judgment: String,
    /** The [[ChangeModel]] that interpreted ΔL for this transition. Every
      * accept path today mints under [[ChangeModel.default]] — threading a
      * custom model through the CREATE side of `SemanticRepository`/`Merge`/
      * `AcceptedTip` is a materially bigger change, explicitly deferred the
      * same way PR6 deferred it for those same call sites.
      */
    changeModel: Digest = ChangeModel.default.digest,
    /** The [[ValidationModel]] governing acceptance, when the accepting
      * gate was built via [[ModuleGate.fromValidationModel]] — exactly
      * `policy.gate.descriptor` at construction time. `None` for gates with
      * no data-described form (open-ended host closures).
      */
    validationModel: Option[Digest] = None,
    /** Judgment-provider language digests `validationModel` (when present)
      * transitively names — `policy.gate.providers` at construction time.
      */
    providers: List[Digest] = Nil,
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex),
    "base" -> Canon.CStr(base.hex),
    "validatedChangeSet" -> validatedChangeSet.fold(Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "result" -> Canon.CStr(result.hex),
    "policy" -> Canon.CStr(policy.hex),
    "judgment" -> Canon.CStr(judgment),
    "changeModel" -> Canon.CStr(changeModel.hex),
    "validationModel" -> validationModel.fold(Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "providers" -> Canon.CList(providers.map(d => Canon.CStr(d.hex))))
  def artifact: Artifact = Artifact(ArtifactKind.AcceptanceEvidence, canon)
  def digest: Digest = artifact.digest

object AcceptanceEvidence:
  def fromCanon(c: Canon): Either[String, AcceptanceEvidence] =
    try
      val vcsDigest = c.field("validatedChangeSet") match
        case Canon.CTag("some", Canon.CStr(s)) => Some(Digest(s))
        case _                                 => None
      // changeModel/validationModel/providers are absent on canon minted
      // before PR9 — default to ChangeModel.default/None/Nil, the same
      // mandatory-on-type/defaulted-on-legacy-decode split every prior
      // schema addition to a durable, content-addressed type has used.
      val changeModel = c.asMap.get("changeModel").map(v => Digest(v.asStr)).getOrElse(ChangeModel.default.digest)
      val validationModel = c.asMap.get("validationModel").flatMap {
        case Canon.CTag("some", Canon.CStr(s)) => Some(Digest(s))
        case _                                 => None
      }
      val providers = c.asMap.get("providers").map(_.asList.map(v => Digest(v.asStr))).getOrElse(Nil)
      Right(AcceptanceEvidence(
        Digest(c.field("language").asStr), Digest(c.field("base").asStr),
        vcsDigest, Digest(c.field("result").asStr),
        Digest(c.field("policy").asStr), c.field("judgment").asStr,
        changeModel, validationModel, providers))
    catch case CodecError(m) => Left(m)

  /** Independently re-derive whether `evidence` genuinely holds — never
    * trusts the evidence's self-reported fields, only what can be
    * recomputed from `language`/`base`/`vcs`/`result`/`policy` themselves:
    *
    *   - `evidence.language`/`evidence.base` bind to the real language/base.
    *   - `evidence.validatedChangeSet`, when present, must equal the real
    *     `vcs.artifact.digest` — `vcs` itself is only constructible by a
    *     successful ΔL replay ([[Delta.ValidatedChangeSet.check]]/`apply`),
    *     so this is a genuine replay check, not a stored-claim comparison —
    *     and `vcs`'s own `base`/`result` must agree with `base`/`result`.
    *   - absence must agree on both sides (an evidence claiming "no change"
    *     against a supplied real `vcs`, or vice versa, is rejected).
    *   - `policy`/`judgment` identity, then re-running `policy.gate` against
    *     `result` — never trusting the evidence's self-reported success.
    */
  def verify(
      language: ComposedLanguage,
      base: Module,
      vcs: Option[Delta.ValidatedChangeSet],
      policy: AcceptancePolicy,
      result: Module,
      evidence: AcceptanceEvidence,
      model: ChangeModel = ChangeModel.default,
  ): Either[String, Unit] =
    if evidence.language != language.digest then
      Left(s"AcceptanceEvidence: language mismatch (${evidence.language.short} ≠ ${language.digest.short})")
    else if evidence.base != base.digest then
      Left(s"AcceptanceEvidence: base mismatch (${evidence.base.short} ≠ ${base.digest.short})")
    else if evidence.result != result.digest then
      Left(s"AcceptanceEvidence: result mismatch (${evidence.result.short} ≠ ${result.digest.short})")
    else if evidence.policy != policy.digest then
      Left(s"AcceptanceEvidence: policy mismatch (${evidence.policy.short} ≠ ${policy.digest.short})")
    else if evidence.judgment != policy.gate.judgment then
      Left(s"AcceptanceEvidence: judgment mismatch ('${evidence.judgment}' ≠ '${policy.gate.judgment}')")
    else if evidence.changeModel != model.digest then
      Left(s"AcceptanceEvidence: changeModel mismatch (${evidence.changeModel.short} ≠ ${model.digest.short})")
    else if evidence.validationModel != policy.gate.descriptor then
      Left(s"AcceptanceEvidence: validationModel mismatch (${evidence.validationModel.map(_.short)} ≠ ${policy.gate.descriptor.map(_.short)})")
    else if evidence.providers != policy.gate.providers then
      Left(s"AcceptanceEvidence: providers mismatch (${evidence.providers.map(_.short)} ≠ ${policy.gate.providers.map(_.short)})")
    else
      (evidence.validatedChangeSet, vcs) match
        case (None, None) => ModuleGate.require(policy.gate, result)
        case (Some(evDig), Some(v)) =>
          if v.base != base.digest then
            Left(s"AcceptanceEvidence: supplied ValidatedChangeSet base ${v.base.short} ≠ base ${base.digest.short}")
          else if v.result != result.digest then
            Left(s"AcceptanceEvidence: supplied ValidatedChangeSet result ${v.result.short} ≠ result ${result.digest.short}")
          else if v.artifact.digest != evDig then
            Left(s"AcceptanceEvidence: validatedChangeSet mismatch (${evDig.short} ≠ ${v.artifact.digest.short})")
          else ModuleGate.require(policy.gate, result)
        case (evOpt, vOpt) =>
          Left(s"AcceptanceEvidence: validatedChangeSet presence mismatch (evidence has one: ${evOpt.isDefined}, supplied one: ${vOpt.isDefined})")

/** The only way to advance a branch head under a policy: a module that has
  * both replayed cleanly against ΔL (carries a genuine
  * [[Delta.ValidatedChangeSet]] — itself only mintable by a successful
  * [[Delta.apply]]/[[Delta.applyTyped]], never forgeable) AND satisfied an
  * explicit [[AcceptancePolicy]]. Modeled on
  * [[SemanticRepository.ValidatedTip]] / [[Delta.ValidatedChangeSet]]:
  * opaque type, privately-gated mint, and `check*` functions as the only
  * path in — never trusts a caller's self-reported success.
  */
private[core] final case class AcceptedTipRepr(
    base: Module,
    module: Module,
    change: Cst,
    vcs: Delta.ValidatedChangeSet,
    policy: AcceptancePolicy,
    languageDigest: Digest,
)

opaque type AcceptedTip = AcceptedTipRepr

object AcceptedTip:
  private def mint(
      base: Module, module: Module, change: Cst,
      vcs: Delta.ValidatedChangeSet, policy: AcceptancePolicy, languageDigest: Digest,
  ): AcceptedTip = AcceptedTipRepr(base, module, change, vcs, policy, languageDigest)

  /** Check a proposed [[SemanticRepository.Tip]]: ΔL replay, then `policy`. */
  def checkTip(
      language: ComposedLanguage,
      proposed: SemanticRepository.Tip,
      policy: AcceptancePolicy,
  ): Either[String, AcceptedTip] =
    SemanticRepository.ValidatedTip.check(language, proposed).flatMap { vt =>
      ModuleGate.require(policy.gate, vt.tip).map(_ =>
        mint(vt.base, vt.tip, vt.change, vt.vcs, policy, language.digest))
    }

  /** Wrap an already ΔL-replayed merge/integrate [[SemanticRepository.Outcome.Accepted]]
    * (its `vcs` is only constructible by a successful `Delta.applyTyped` /
    * `Merge.threeWay` — never forgeable) with a `policy` check.
    */
  def checkMerged(
      language: ComposedLanguage,
      base: Module,
      outcome: SemanticRepository.Outcome.Accepted,
      policy: AcceptancePolicy,
  ): Either[String, AcceptedTip] =
    ModuleGate.require(policy.gate, outcome.module).map(_ =>
      mint(base, outcome.module, outcome.mergedChange, outcome.vcs, policy, language.digest))

  extension (a: AcceptedTip)
    def base: Module = a.base
    def module: Module = a.module
    def change: Cst = a.change
    def vcs: Delta.ValidatedChangeSet = a.vcs
    def policy: AcceptancePolicy = a.policy
    def languageDigest: Digest = a.languageDigest
    def evidence: AcceptanceEvidence = AcceptanceEvidence(
      language = a.languageDigest,
      base = a.base.digest,
      validatedChangeSet = Some(a.vcs.artifact.digest),
      result = a.module.digest,
      policy = a.policy.digest,
      judgment = a.policy.gate.judgment,
      validationModel = a.policy.gate.descriptor,
      providers = a.policy.gate.providers)
