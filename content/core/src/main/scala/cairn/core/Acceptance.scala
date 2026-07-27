package cairn.core

import cairn.kernel.*

final case class CertificateRequirement(kind: ArtifactKind, issuer: Option[String], minimum: Int = 1):
  def canon: Canon = Canon.cmap(
    "kind" -> Canon.CStr(kind.name),
    "issuer" -> issuer.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))),
    "minimum" -> Canon.CInt(minimum))

final case class AuthorityRules(required: Set[String] = Set.empty, threshold: Int = 0):
  def canon: Canon = Canon.cmap(
    "required" -> Canon.cstrs(required.toList.sorted), "threshold" -> Canon.CInt(threshold))

final case class MigrationRules(allowed: Set[Digest] = Set.empty, required: Boolean = false):
  def canon: Canon = Canon.cmap(
    "allowed" -> Canon.cstrs(allowed.toList.map(_.hex).sorted),
    "required" -> Canon.CInt(if required then 1 else 0))

enum PublicationRules:
  case Forbidden, Optional, Required
  def canon: Canon = Canon.CStr(toString.toLowerCase)

/** One canonical artifact specifies every condition under which a module may
  * become externally visible as an accepted branch head. */
final case class AcceptanceConstitution(
    changeModel: Digest,
    validationModel: Option[Digest],
    requiredDomainAgreement: Option[Digest],
    requiredCertificates: List[CertificateRequirement],
    authorityRules: AuthorityRules,
    migrationRules: MigrationRules,
    publicationRules: PublicationRules,
):
  def canon: Canon = Canon.cmap(
    "changeModel" -> Canon.CStr(changeModel.hex),
    "validationModel" -> validationModel.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "requiredDomainAgreement" -> requiredDomainAgreement.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "requiredCertificates" -> Canon.CList(requiredCertificates.map(_.canon)),
    "authorityRules" -> authorityRules.canon,
    "migrationRules" -> migrationRules.canon,
    "publicationRules" -> publicationRules.canon)
  def artifact: Artifact = Artifact(ArtifactKind.AcceptanceConstitution, canon)
  def digest: Digest = artifact.digest

object AcceptanceConstitution:
  def open(changeModel: Digest = ChangeModel.default.digest): AcceptanceConstitution =
    AcceptanceConstitution(changeModel, None, None, Nil, AuthorityRules(), MigrationRules(), PublicationRules.Optional)

  def fromPolicy(policy: AcceptancePolicy, changeModel: Digest): AcceptanceConstitution =
    open(changeModel).copy(validationModel = policy.gate.descriptor)

  def fromCanon(c: Canon): Either[String, AcceptanceConstitution] =
    try
      def optDigest(field: String): Option[Digest] = c.field(field) match
        case Canon.CTag("some", Canon.CStr(s)) => Some(Digest(s))
        case _                                  => None
      val certs = c.field("requiredCertificates").asList.map { row =>
        CertificateRequirement(
          ArtifactKind.parse(row.field("kind").asStr).fold(e => throw CodecError(e), identity),
          row.field("issuer") match
            case Canon.CTag("some", Canon.CStr(s)) => Some(s)
            case _                                  => None,
          row.field("minimum").asInt.toInt)
      }
      val authority = c.field("authorityRules")
      val migration = c.field("migrationRules")
      val publication = c.field("publicationRules").asStr match
        case "forbidden" => PublicationRules.Forbidden
        case "required"  => PublicationRules.Required
        case "optional"  => PublicationRules.Optional
        case other        => throw CodecError(s"unknown publication rule '$other'")
      Right(AcceptanceConstitution(
        Digest(c.field("changeModel").asStr), optDigest("validationModel"),
        optDigest("requiredDomainAgreement"), certs,
        AuthorityRules(authority.field("required").asList.map(_.asStr).toSet,
          authority.field("threshold").asInt.toInt),
        MigrationRules(migration.field("allowed").asList.map(x => Digest(x.asStr)).toSet,
          migration.field("required").asInt == 1L), publication))
    catch case CodecError(m) => Left(m)

  def fromArtifact(artifact: Artifact): Either[String, AcceptanceConstitution] =
    if artifact.kind != ArtifactKind.AcceptanceConstitution then Left("expected acceptance-constitution artifact")
    else fromCanon(artifact.body)

final case class CertificateFact(digest: Digest, kind: ArtifactKind, issuer: Option[String])

/** Canonical facts supplied to the pure constitution evaluator. Orchestration
  * may discover these facts, but cannot invent acceptance policy around them. */
final case class AcceptanceFacts(
    domainAgreement: Option[Digest] = None,
    certificates: List[CertificateFact] = Nil,
    authorities: Set[String] = Set.empty,
    migration: Option[Digest] = None,
    publicationRequested: Boolean = false,
):
  def canon: Canon =
    val certs = certificates.sortBy(_.digest.hex).map { f =>
      Canon.cmap(
        "digest" -> Canon.CStr(f.digest.hex),
        "kind" -> Canon.CStr(f.kind.name),
        "issuer" -> f.issuer.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x =>
          Canon.CTag("some", Canon.CStr(x))))
    }
    Canon.cmap(
      "domainAgreement" -> domainAgreement.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
      "certificates" -> Canon.CList(certs),
      "authorities" -> Canon.cstrs(authorities.toList.sorted),
      "migration" -> migration.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
      "publicationRequested" -> Canon.CInt(if publicationRequested then 1 else 0))

object AcceptanceConstitutionEvaluator:
  def check(
      constitution: AcceptanceConstitution,
      gate: ModuleGate,
      changeModel: Digest,
      result: Module,
      facts: AcceptanceFacts,
  ): Either[String, Unit] =
    def certificates: Either[String, Unit] =
      constitution.requiredCertificates.foldLeft[Either[String, Unit]](Right(())) { (acc, req) =>
        acc.flatMap { _ =>
          val count = facts.certificates.count(f => f.kind == req.kind && req.issuer.forall(i => f.issuer.contains(i)))
          Either.cond(count >= req.minimum, (),
            s"acceptance constitution requires ${req.minimum} ${req.kind.name} certificate(s)${req.issuer.fold("")(i => s" from '$i'")}, found $count")
        }
      }
    for
      _ <- Either.cond(changeModel == constitution.changeModel, (), "acceptance constitution change model mismatch")
      _ <- Either.cond(gate.descriptor == constitution.validationModel, (), "acceptance constitution validation model mismatch")
      _ <- ModuleGate.require(gate, result)
      _ <- Either.cond(constitution.requiredDomainAgreement.forall(facts.domainAgreement.contains), (),
        "acceptance constitution domain agreement requirement not satisfied")
      _ <- certificates
      _ <- Either.cond(constitution.authorityRules.required.subsetOf(facts.authorities), (),
        "acceptance constitution required authorities are missing")
      _ <- Either.cond(facts.authorities.size >= constitution.authorityRules.threshold, (),
        "acceptance constitution authority threshold not met")
      _ <- Either.cond(!constitution.migrationRules.required || facts.migration.nonEmpty, (),
        "acceptance constitution requires a migration")
      _ <- Either.cond(facts.migration.forall(d => constitution.migrationRules.allowed.isEmpty || constitution.migrationRules.allowed.contains(d)), (),
        "acceptance constitution forbids the selected migration")
      _ <- constitution.publicationRules match
        case PublicationRules.Forbidden => Either.cond(!facts.publicationRequested, (), "acceptance constitution forbids publication")
        case PublicationRules.Required  => Either.cond(facts.publicationRequested, (), "acceptance constitution requires publication")
        case PublicationRules.Optional  => Right(())
    yield ()

/** What must additionally hold, beyond ΔL replay, before a proposed module
  * may become a branch's new head. [[AcceptancePolicy.open]] is
  * passthrough — always available, but a NAMED, visible choice a caller
  * must make explicitly; there is no default policy parameter anywhere in
  * [[cairn.runtime.Branches]]' acceptance API, so skipping domain
  * validation can no longer happen by omission.
  *
  * Compatibility view for callers that only supply a [[ModuleGate]]. New
  * acceptance is governed by [[AcceptanceConstitution]]; this type constructs
  * an explicitly open constitution for the remaining dimensions.
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
    /** The [[ChangeModel]] that actually interpreted ΔL for this transition —
      * read from the minting [[Delta.ValidatedChangeSet]]'s own
      * `changeModel` (stamped by [[Delta.apply]]/[[Delta.applyTyped]] at
      * apply time), never a separately-threaded value that could drift from
      * what was really used. Defaults to [[ChangeModel.default]]'s digest
      * only for legacy-decoded evidence minted before this field existed.
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
    constitution: Option[Digest] = None,
    domainAgreement: Option[Digest] = None,
    certificates: List[Digest] = Nil,
    authorities: List[String] = Nil,
    migration: Option[Digest] = None,
    publicationRequested: Boolean = false,
    runtime: Option[Digest] = None,
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
    "providers" -> Canon.CList(providers.map(d => Canon.CStr(d.hex))),
    "constitution" -> constitution.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "domainAgreement" -> domainAgreement.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "certificates" -> Canon.cstrs(certificates.map(_.hex).sorted),
    "authorities" -> Canon.cstrs(authorities.sorted),
    "migration" -> migration.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))),
    "publicationRequested" -> Canon.CInt(if publicationRequested then 1 else 0),
    "runtime" -> runtime.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d => Canon.CTag("some", Canon.CStr(d.hex))))
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
      def optDigest(field: String): Option[Digest] = c.asMap.get(field).flatMap {
        case Canon.CTag("some", Canon.CStr(s)) => Some(Digest(s))
        case _                                  => None
      }
      Right(AcceptanceEvidence(
        Digest(c.field("language").asStr), Digest(c.field("base").asStr),
        vcsDigest, Digest(c.field("result").asStr),
        Digest(c.field("policy").asStr), c.field("judgment").asStr,
        changeModel, validationModel, providers,
        optDigest("constitution"), optDigest("domainAgreement"),
        c.asMap.get("certificates").map(_.asList.map(x => Digest(x.asStr))).getOrElse(Nil),
        c.asMap.get("authorities").map(_.asList.map(_.asStr)).getOrElse(Nil),
        optDigest("migration"),
        c.asMap.get("publicationRequested").exists(_.asInt == 1L),
        optDigest("runtime")))
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

  /** Complete verification path for post-PR15 evidence. */
  def verifyComplete(
      language: ComposedLanguage,
      base: Module,
      vcs: Option[Delta.ValidatedChangeSet],
      policy: AcceptancePolicy,
      constitution: AcceptanceConstitution,
      facts: AcceptanceFacts,
      result: Module,
      evidence: AcceptanceEvidence,
      model: ChangeModel = ChangeModel.default,
  ): Either[String, Unit] =
    for
      _ <- verify(language, base, vcs, policy, result, evidence, model)
      _ <- Either.cond(evidence.constitution.contains(constitution.digest), (),
        "AcceptanceEvidence: constitution mismatch")
      _ <- Either.cond(evidence.domainAgreement == facts.domainAgreement, (),
        "AcceptanceEvidence: domain agreement facts mismatch")
      _ <- Either.cond(evidence.certificates.sortBy(_.hex) == facts.certificates.map(_.digest).sortBy(_.hex), (),
        "AcceptanceEvidence: certificate facts mismatch")
      _ <- Either.cond(evidence.authorities.sorted == facts.authorities.toList.sorted, (),
        "AcceptanceEvidence: authority facts mismatch")
      _ <- Either.cond(evidence.migration == facts.migration, (), "AcceptanceEvidence: migration facts mismatch")
      _ <- Either.cond(evidence.publicationRequested == facts.publicationRequested, (),
        "AcceptanceEvidence: publication facts mismatch")
      _ <- AcceptanceConstitutionEvaluator.check(constitution, policy.gate, model.digest, result, facts)
    yield ()

  def verifyComplete(
      runtime: ResolvedDomainRuntime,
      base: Module,
      vcs: Option[Delta.ValidatedChangeSet],
      policy: AcceptancePolicy,
      facts: AcceptanceFacts,
      result: Module,
      evidence: AcceptanceEvidence,
  ): Either[String, Unit] =
    for
      _ <- Either.cond(evidence.runtime.contains(runtime.digest), (),
        "AcceptanceEvidence: domain runtime mismatch")
      _ <- verifyComplete(runtime.language, base, vcs, policy, runtime.acceptance,
        facts, result, evidence, runtime.changeModel)
    yield ()

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
    constitution: AcceptanceConstitution,
    facts: AcceptanceFacts,
    runtime: Option[ResolvedDomainRuntime],
    accessTrace: Option[AccessTrace],
)

opaque type AcceptedTip = AcceptedTipRepr

object AcceptedTip:
  private def mint(
      base: Module, module: Module, change: Cst,
      vcs: Delta.ValidatedChangeSet, policy: AcceptancePolicy, languageDigest: Digest,
      constitution: AcceptanceConstitution, facts: AcceptanceFacts,
      runtime: Option[ResolvedDomainRuntime] = None,
      accessTrace: Option[AccessTrace] = None,
  ): AcceptedTip = AcceptedTipRepr(base, module, change, vcs, policy, languageDigest,
    constitution, facts, runtime, accessTrace)

  /** Governed acceptance path: all selections come from one checked runtime. */
  def checkTip(
      runtime: ResolvedDomainRuntime,
      proposed: SemanticRepository.Tip,
  ): Either[String, AcceptedTip] = checkTip(runtime, proposed, AcceptanceFacts())

  def checkTip(
      runtime: ResolvedDomainRuntime,
      proposed: SemanticRepository.Tip,
      facts: AcceptanceFacts,
  ): Either[String, AcceptedTip] =
    val policy = AcceptancePolicy(runtime.moduleGate())
    SemanticRepository.ValidatedTip.check(runtime, proposed).flatMap { vt =>
      for
        trace <- ChangeAlgebra.accessTrace(runtime.language, proposed.base, proposed.change, runtime.changeModel)
          .left.map(_.render)
        _ <- AcceptanceConstitutionEvaluator.check(runtime.acceptance, policy.gate,
          runtime.changeModel.digest, vt.tip, facts)
      yield
        mint(vt.base, vt.tip, vt.change, vt.vcs, policy, runtime.language.digest,
          runtime.acceptance, facts, Some(runtime), Some(trace))
    }

  /** Check a proposed [[SemanticRepository.Tip]]: ΔL replay, then `policy`. */
  def checkTip(
      language: ComposedLanguage,
      proposed: SemanticRepository.Tip,
      policy: AcceptancePolicy,
      model: ChangeModel = ChangeModel.default,
  ): Either[String, AcceptedTip] =
    checkTip(language, proposed, policy,
      AcceptanceConstitution.fromPolicy(policy, model.digest), AcceptanceFacts(), model)

  def checkTip(
      language: ComposedLanguage,
      proposed: SemanticRepository.Tip,
      policy: AcceptancePolicy,
      constitution: AcceptanceConstitution,
      facts: AcceptanceFacts,
      model: ChangeModel,
  ): Either[String, AcceptedTip] =
    SemanticRepository.ValidatedTip.check(language, proposed, model).flatMap { vt =>
      AcceptanceConstitutionEvaluator.check(constitution, policy.gate, model.digest, vt.tip, facts).map(_ =>
        mint(vt.base, vt.tip, vt.change, vt.vcs, policy, language.digest, constitution, facts))
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
    checkMerged(language, base, outcome, policy,
      AcceptanceConstitution.fromPolicy(policy, outcome.vcs.changeModel), AcceptanceFacts())

  def checkMerged(
      language: ComposedLanguage,
      base: Module,
      outcome: SemanticRepository.Outcome.Accepted,
      policy: AcceptancePolicy,
      constitution: AcceptanceConstitution,
      facts: AcceptanceFacts,
  ): Either[String, AcceptedTip] =
    AcceptanceConstitutionEvaluator.check(
      constitution, policy.gate, outcome.vcs.changeModel, outcome.module, facts).map(_ =>
      mint(base, outcome.module, outcome.mergedChange, outcome.vcs, policy, language.digest, constitution, facts))

  def checkMerged(
      runtime: ResolvedDomainRuntime,
      base: Module,
      outcome: SemanticRepository.Outcome.Accepted,
      facts: AcceptanceFacts,
  ): Either[String, AcceptedTip] =
    val policy = AcceptancePolicy(runtime.moduleGate())
    for
      _ <- Either.cond(outcome.vcs.language == runtime.language.digest, (), "merged change language differs from runtime")
      _ <- Either.cond(outcome.vcs.changeModel == runtime.changeModel.digest, (), "merged change model differs from runtime")
      trace <- ChangeAlgebra.accessTrace(runtime.language, base, outcome.mergedChange, runtime.changeModel).left.map(_.render)
      _ <- AcceptanceConstitutionEvaluator.check(runtime.acceptance, policy.gate,
        runtime.changeModel.digest, outcome.module, facts)
    yield mint(base, outcome.module, outcome.mergedChange, outcome.vcs, policy,
      runtime.language.digest, runtime.acceptance, facts, Some(runtime), Some(trace))

  extension (a: AcceptedTip)
    def base: Module = a.base
    def module: Module = a.module
    def change: Cst = a.change
    def vcs: Delta.ValidatedChangeSet = a.vcs
    def policy: AcceptancePolicy = a.policy
    def languageDigest: Digest = a.languageDigest
    def constitution: AcceptanceConstitution = a.constitution
    def facts: AcceptanceFacts = a.facts
    def runtime: Option[Digest] = a.runtime.map(_.digest)
    def runtimeArtifacts: List[Artifact] = a.runtime.toList.flatMap(_.artifacts)
    def accessTrace: Option[AccessTrace] = a.accessTrace
    def evidence: AcceptanceEvidence = AcceptanceEvidence(
      language = a.languageDigest,
      base = a.base.digest,
      validatedChangeSet = Some(a.vcs.artifact.digest),
      result = a.module.digest,
      policy = a.policy.digest,
      judgment = a.policy.gate.judgment,
      changeModel = a.vcs.changeModel,
      validationModel = a.policy.gate.descriptor,
      providers = a.policy.gate.providers,
      constitution = Some(a.constitution.digest),
      domainAgreement = a.facts.domainAgreement,
      certificates = a.facts.certificates.map(_.digest),
      authorities = a.facts.authorities.toList.sorted,
      migration = a.facts.migration,
      publicationRequested = a.facts.publicationRequested,
      runtime = a.runtime.map(_.digest))
