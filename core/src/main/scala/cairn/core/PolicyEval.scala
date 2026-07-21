package cairn.core

import cairn.kernel.Authority
import cairn.kernel.Authority.*
import cairn.kernel.{EffectMeta, Effects}

/** Core policy evaluation and capability attenuation (Phase 4).
  *
  * Core **constructs** [[AuthorizationProof]] witnesses; Kernel
  * [[Authority.checkProof]] validates them (shared pure checkers for covers /
  * attenuation). [[propose]] / [[decide]] remain for audit and tests.
  *
  * Policies reference [[Effects.ActionKey]] / [[EffectMeta.ResourceSchema]]
  * derived from effect-interface artifacts.
  *
  * Attenuation and delegation are Core optimizations; Kernel mints/checks
  * [[AttenuationWitness]] so child grants cannot widen parent authority.
  */
object PolicyEval:

  /** Propose an authorization derivation for `req` under `policies` (audit / tests). */
  def propose(req: EffectRequest, policies: List[EffectPolicy]): AuthorizationDerivation =
    Authority.decide(req, policies)

  /** Construct a structured allow proof, or Left with a deny reason.
    * Does not mint authority — Kernel must [[Authority.checkProof]].
    */
  def prove(
      req: EffectRequest,
      policies: List[EffectPolicy],
      nowMillis: Long
  ): Either[String, AuthorizationProof] =
    val matched = policies.filter(_.matches(req))
    val denies = matched.filter(_.decision == Decision.Deny)
    if denies.nonEmpty then Left(s"deny by ${denies.map(_.id).mkString(",")}")
    else
      val allows = matched.filter(_.decision == Decision.Allow)
      if allows.isEmpty then Left("no matching allow policy")
      else
        val expires = allows.flatMap(_.metaExpiresAt).minOption
        if expires.exists(_ < nowMillis) then Left("grant expired")
        else
          val nonce = req.args.get("nonce").orElse(allows.flatMap(_.metaNonce).headOption)
          val grant = CapabilityGrant(
            req.subject, req.action, req.resource,
            expiresAtEpochMillis = expires,
            nonce = nonce,
            sourcePolicyIds = allows.map(_.id))
          if !grant.covers(req, nowMillis) then Left("grant does not cover request")
          else
            Right(AuthorizationProof(
              request = req,
              nowMillis = nowMillis,
              allowPolicies = allows,
              grant = grant,
              conditionEvidence = conditionEvidence(req, allows)))

  /** Attach a Kernel-checked attenuation step to an existing allow proof. */
  def proveAttenuated(
      base: AuthorizationProof,
      narrower: Resource
  ): Either[String, AuthorizationProof] =
    attenuate(base.grant, narrower).map { (child, w) =>
      base.copy(grant = child, attenuatedFrom = Some((base.grant, w)))
    }

  /** Prove a final grantee's request via an Alice→…→Carol delegation chain.
    *
    * Root policies must match the root grantor; the chain is Kernel-validated;
    * the final child must cover `req`. Cited allows justify the root, not Carol.
    */
  def proveDelegated(
      req: EffectRequest,
      policies: List[EffectPolicy],
      chain: List[Delegation],
      nowMillis: Long
  ): Either[String, AuthorizationProof] =
    if chain.isEmpty then Left("empty delegation chain")
    else
      Delegation.validateChain(chain).flatMap { finalGrant =>
        if !finalGrant.covers(req, nowMillis) then Left("final grant does not cover request")
        else if finalGrant.expiresAtEpochMillis.exists(_ < nowMillis) then Left("grant expired")
        else
          val root = chain.head.parent
          val rootReq = req.copy(subject = root.subject, action = root.action, resource = root.resource)
          val matched = policies.filter(_.matches(rootReq))
          val denies = matched.filter(_.decision == Decision.Deny)
          if denies.nonEmpty then Left(s"deny by ${denies.map(_.id).mkString(",")}")
          else
            val allows = matched.filter(_.decision == Decision.Allow)
            if allows.isEmpty then Left("no matching allow policy for root grantor")
            else if allows.exists(p => !root.resource.matches(p.resource)) then
              Left("root grant widens beyond cited policy resource")
            else
              val grant = finalGrant.copy(sourcePolicyIds = allows.map(_.id))
              Right(AuthorizationProof(
                request = req,
                nowMillis = nowMillis,
                allowPolicies = allows,
                grant = grant,
                conditionEvidence = conditionEvidence(req, allows),
                delegationChain = chain))
      }

  private def conditionEvidence(req: EffectRequest, allows: List[EffectPolicy]): Map[String, String] =
    allows.iterator
      .flatMap(_.conditions.iterator)
      .collect { case (k, _) if !k.startsWith("meta:") => k -> req.args.getOrElse(k, "") }
      .toMap

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

  /** Ledger append under a root-path pattern. Subject may be `"*"` so
    * Node.append can authorize as the signing authority name.
    */
  def ledgerNode(subject: Subject | "*", rootPattern: String = "*"): List[EffectPolicy] =
    List(EffectPolicy(
      "ledger-append",
      subject,
      EffectMeta.ledgerTransport.actionKey("append"),
      EffectMeta.ledgerTransport.resource.at(rootPattern),
      Decision.Allow))

  /** Process.run; optional command-name allow-list (exact, prefix `foo*`, or `*`). */
  def processRunner(subject: Subject | "*", commands: List[String] = List("*")): List[EffectPolicy] =
    commands.map(cmd => EffectPolicy(
      s"process-run-$cmd",
      subject,
      EffectMeta.process.actionKey("run"),
      EffectMeta.process.resource.at(cmd),
      Decision.Allow))

  /** LSP read/write (session-scoped `*`). */
  def lspSession(subject: Subject | "*"): List[EffectPolicy] =
    List(
      EffectPolicy(
        "lsp-read", subject, EffectMeta.lsp.actionKey("read"),
        EffectMeta.lsp.resource.any, Decision.Allow),
      EffectPolicy(
        "lsp-write", subject, EffectMeta.lsp.actionKey("write"),
        EffectMeta.lsp.resource.any, Decision.Allow))

  /** External backend find/run; optional host patterns. */
  def externalBackend(subject: Subject | "*", hosts: List[String] = List("*")): List[EffectPolicy] =
    hosts.flatMap { host =>
      List(
        EffectPolicy(
          s"backend-find-$host", subject, EffectMeta.externalBackend.actionKey("find"),
          EffectMeta.externalBackend.resource.at(host), Decision.Allow),
        EffectPolicy(
          s"backend-run-$host", subject, EffectMeta.externalBackend.actionKey("run"),
          EffectMeta.externalBackend.resource.at(host), Decision.Allow))
    }

  /** CAS put/get/stats under a digest-or-root path pattern (default `*`).
    * `contains` authorizes as `get`. Destructive admin (`fsck`/`gc`) is
    * [[casAdmin]] — not included here.
    */
  def casStore(subject: Subject | "*", pathPattern: String = "*"): List[EffectPolicy] =
    List(
      EffectPolicy(
        "cas-put", subject, EffectMeta.cas.actionKey("put"),
        EffectMeta.cas.resource.at(pathPattern), Decision.Allow),
      EffectPolicy(
        "cas-get", subject, EffectMeta.cas.actionKey("get"),
        EffectMeta.cas.resource.at(pathPattern), Decision.Allow),
      EffectPolicy(
        "cas-stats", subject, EffectMeta.cas.actionKey("stats"),
        EffectMeta.cas.resource.at(pathPattern), Decision.Allow))

  /** CAS maintenance: fsck (quarantine) and mark/sweep GC. */
  def casAdmin(subject: Subject | "*", pathPattern: String = "*"): List[EffectPolicy] =
    List(
      EffectPolicy(
        "cas-fsck", subject, EffectMeta.cas.actionKey("fsck"),
        EffectMeta.cas.resource.at(pathPattern), Decision.Allow),
      EffectPolicy(
        "cas-gc", subject, EffectMeta.cas.actionKey("gc"),
        EffectMeta.cas.resource.at(pathPattern), Decision.Allow))

  /** Filesystem read/write/mkdirs under a path pattern (default `*` = whole FS).
    * Narrower than allow-all: other families remain denied.
    */
  def filesystemStore(subject: Subject | "*", pathPattern: String = "*"): List[EffectPolicy] =
    List(
      EffectPolicy(
        "fs-read", subject, EffectMeta.filesystem.actionKey("read"),
        EffectMeta.filesystem.resource.at(pathPattern), Decision.Allow),
      EffectPolicy(
        "fs-write", subject, EffectMeta.filesystem.actionKey("write"),
        EffectMeta.filesystem.resource.at(pathPattern), Decision.Allow),
      EffectPolicy(
        "fs-mkdirs", subject, EffectMeta.filesystem.actionKey("mkdirs"),
        EffectMeta.filesystem.resource.at(pathPattern), Decision.Allow))

  /** Branches composition: CAS put/get/stats plus refs-directory FS. */
  def branchesStore(
      subject: Subject | "*" = Subject("local"),
      refsPathPattern: String = "*",
      casPathPattern: String = "*",
  ): List[EffectPolicy] =
    casStore(subject, casPathPattern) ++ filesystemStore(subject, refsPathPattern)

  /** Ledger node + its local CAS + chain-file FS (append as any subject;
    * CAS / chain FS as `local`).
    */
  def ledgerWithCas(
      subject: Subject | "*" = "*",
      rootPattern: String = "*",
      casSubject: Subject = Subject("local"),
      chainPathPattern: String = "*",
  ): List[EffectPolicy] =
    ledgerNode(subject, rootPattern) ++
      casStore(casSubject) ++
      filesystemStore(casSubject, chainPathPattern)

