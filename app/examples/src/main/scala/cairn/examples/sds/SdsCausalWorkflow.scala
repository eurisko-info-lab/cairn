package cairn.examples.sds

import cairn.systemhandler.{AuthorityGate, DiskCas, Ed25519, EffectContext, Node, Keypair}
import cairn.kernel.*
import cairn.kernel.Authority.*
import cairn.core.*
import cairn.runtime.{Branches, PackLoader, WorkflowRunner}
import java.nio.file.{Files, Path}

/** SDS verified-application path through the causal repository.
  *
  * Scripted sequence is disk SoT ([[SdsWorkflow.causal]]: author → shadow →
  * rebase → conflict → approve → sign → publish). Effectful steps run under
  * [[WorkflowRunner]] (language-defined order; Scala supplies handlers only).
  * Attaches judgment-checked certificate kinds ([[SdsCertificateKinds]]).
  * Not Studio approval UI.
  */
object SdsCausalWorkflow:
  final case class Report(
      workflowDigest: Digest,
      workflowSteps: List[String],
      certificateKindTags: List[String],
      authorDigest: Digest,
      industrialDigest: Digest,
      rebaseMerged: Boolean,
      conflictOverlap: Set[String],
      approvedDigest: Digest,
      historyFromManifestAlone: Int,
      verifiedCapabilityOk: Boolean,
      /** CAS put authorized via non-empty grant bundle (capability-first path). */
      capabilityAuthorizedPut: Boolean,
      tipSignatureHex: String,
      certificateDigests: List[Digest],
      ledgerPublished: Boolean,
      /** Steps completed by the generic [[WorkflowRunner]]. */
      runnerCompleted: List[String],
      /** Ledger domain hubs planted (PKI→LAW→SDS + CHEMISTRY). */
      domainTreeOk: Boolean,
      sdsPrimary: Option[String],
      sdsReferences: List[String],
      /** Work branch `sds-author` hangs under the SDS domain hub. */
      authorPrimary: Option[String],
  )

  def run(work: Path): Report =
    val packs = PackLoader(EffectContext.forPackLoader())
    val Sds = cairn.examples.sds.Sds(packs)
    val lang = Sds.language
    val dl = Delta.deltaOf(lang).fold(e => throw RuntimeException(e.map(_.render).mkString), identity)
    def parse(src: String): Cst =
      Parser.parse(dl.grammar, src).fold(e => throw RuntimeException(e), identity)

    val wf = SdsWorkflow.causal
    if wf.steps.map(_.name) != List(
        "author", "shadow", "rebase", "conflict", "approve", "sign", "publish") then
      throw RuntimeException(s"unexpected causal steps: ${wf.steps}")
    val certKinds = SdsCertificateKinds.workflowKinds
    if certKinds != List("sds-approval", "sds-tip-signature", "sds-publication") then
      throw RuntimeException(s"unexpected certificate kinds: $certKinds")

    val cas = DiskCas(work.resolve("cas"))
    val branches = Branches(cas, work.resolve("refs"), EffectContext.forBranches())
    val base = SdsTutorial.acetoneBase
    Sds.validate(base).fold(e => throw RuntimeException(e), identity)
    EuClp.conform(Chemicals.Acetone.thinModule) match
      case r if !r.ok => throw RuntimeException(s"EU-CLP conform: ${r.errors.mkString("; ")}")
      case _ => ()

    // Ledger domain hubs before any SDS work branch (PKI→LAW→SDS + CHEMISTRY).
    val planted = SdsDomainTree.plant(branches).fold(e => throw RuntimeException(e), identity)
    if !planted.ok then throw RuntimeException("domain tree ancestry mismatch after plant")

    // Slots filled by workflow handlers (author→…→publish).
    var industrialTip: Option[SemanticRepository.ValidatedTip] = None
    var rebaseMerged = false
    var conflictOverlap: Set[String] = Set.empty
    var approvedDigest: Digest = base.digest
    var tipSigHex = ""
    var certificateDigests: List[Digest] = Nil
    var verifiedCapabilityOk = false
    var capabilityAuthorizedPut = false
    var historyFromManifestAlone = 0
    var ledgerPublished = false
    var alice: Option[Keypair] = None

    def underSdsTip(name: String, tip: SemanticRepository.ValidatedTip): Unit =
      SdsDomainTree.underSds(branches, name).fold(e => throw RuntimeException(e), identity)
      branches.commitTip(name, tip)

    val runnerSteps = wf.steps.map(s => WorkflowRunner.Step(s.name, s.phase))
    val runner = WorkflowRunner.run(runnerSteps, step => step.name match
      case "author" =>
        SdsDomainTree.underSds(branches, "sds-author", Some(base))
          .fold(e => throw RuntimeException(e), identity)
        Right(base.digest.short)
      case "shadow" =>
        val shadowCs = parse(
          """{ add industrial = shadow cleanerProduct overrides h225 with "Extremely flammable - industrial grade" ; }""")
        val tip = SemanticRepository.tipAfter(lang, base, shadowCs)
          .fold(e => throw RuntimeException(e), identity)
        industrialTip = Some(tip)
        underSdsTip("sds-industrial", tip)
        Right(tip.tipDigest.short)
      case "rebase" =>
        val pctCs = parse("""{ replace cleaner = mixture of ( acetone pct 70 , secretBlend pct 15 ) ; }""")
        val pctTip = SemanticRepository.tipAfter(lang, base, pctCs)
          .fold(e => throw RuntimeException(e), identity)
        underSdsTip("sds-base-rev", pctTip)
        SdsDomainTree.underSds(branches, "sds-merged")
          .fold(e => throw RuntimeException(e), identity)
        rebaseMerged = branches.mergeBranches(lang, "sds-merged", "sds-base-rev", "sds-industrial") match
          case Right(Right(_)) => true
          case Right(Left(_)) => false
          case Left(e) => throw RuntimeException(e)
        Right(if rebaseMerged then "merged" else "conflict")
      case "conflict" =>
        val shadowCs = parse(
          """{ add industrial = shadow cleanerProduct overrides h225 with "Extremely flammable - industrial grade" ; }""")
        val phraseCs = parse(
          """{ replace h225 = corpus phrase h225 lang en text "Base reworded flammable phrase" ; }""")
        conflictOverlap = Sds.rebaseShadow(base, phraseCs, shadowCs) match
          case Left(c) => c.overlap
          case Right(_) => Set.empty
        Right(conflictOverlap.mkString(","))
      case "approve" =>
        val tip = industrialTip.getOrElse(throw RuntimeException("shadow before approve"))
        underSdsTip("sds-approved", tip)
        val approved = branches.headModule("sds-approved").fold(e => throw RuntimeException(e), identity)
        approvedDigest = approved.digest
        Right(approvedDigest.short)
      case "sign" =>
        val tip = industrialTip.getOrElse(throw RuntimeException("shadow before sign"))
        val kp = Keypair.dev("alice-sds")
        alice = Some(kp)
        val tipDigest = approvedDigest
        val tipSig = Ed25519.sign(kp.privateKey, tipDigest.hex.getBytes("UTF-8"))
        tipSigHex = tipSig.map(b => f"${b & 0xff}%02x").mkString
        certificateDigests = SdsCertificates.attachWorkflow(branches, "sds-approved", kp, tipDigest)
          .fold(e => throw RuntimeException(e), identity)
        val manifestCerts = branches.load("sds-approved").certificates
        if manifestCerts != certificateDigests then
          throw RuntimeException(s"manifest certificates mismatch: $manifestCerts vs $certificateDigests")
        val subject = Subject(kp.name)
        val putKey = EffectMeta.cas.actionKey("put")
        val policies = List(EffectPolicy(
          "sds-publish", subject, putKey, Resource("*", "*"), Decision.Allow))
        val req = EffectRequest(subject, putKey, EffectMeta.cas.resource.at(tipDigest.hex))
        val verifiedCap = PolicyEval.prove(req, policies, nowMillis = 0).flatMap { proof =>
          Authority.VerifiedCapability.fromProof(proof, policies)
        }
        verifiedCapabilityOk = verifiedCap.isRight
        capabilityAuthorizedPut = verifiedCap match
          case Right(cap) =>
            val capCtx = EffectContext(
              subject,
              AuthorityGate.enforcing(Nil),
              capabilities = List(cap),
              clock = () => 0L)
            capCtx.authorize(putKey, EffectMeta.cas.resource.at(tipDigest.hex)).isRight
          case Left(_) => false
        underSdsTip("sds-hist", tip)
        val tip2 = SemanticRepository.tipAfter(lang, tip.tip, parse(
          """{ add labelShadow = shadow cleanerProduct overrides h319 with "Eye hazard - industrial SDS wording" ; }"""))
          .fold(e => throw RuntimeException(e), identity)
        branches.commitTip("sds-hist", tip2)
        val refs = work.resolve("refs")
        Files.deleteIfExists(refs.resolve("sds-hist.change"))
        Files.deleteIfExists(refs.resolve("sds-hist.changes"))
        historyFromManifestAlone =
          branches.loadChangeHistory("sds-hist", lang).fold(e => throw RuntimeException(e), _.length)
        Right(tipSigHex.take(16))
      case "publish" =>
        val kp = alice.getOrElse(throw RuntimeException("sign before publish"))
        val node = Node(work.resolve("ledger"), EffectContext.forLedger())
        val auth = Map(kp.name -> kp.publicBytes)
        node.append(kp, auth, List(kp.signTx(Tx.RegisterIdentity(kp.name, kp.publicBytes))))
          .fold(e => throw RuntimeException(e), identity)
        branches.publishHead("sds-approved", node, kp, auth)
          .fold(e => throw RuntimeException(e), identity)
        ledgerPublished = node.state(auth).fold(e => throw RuntimeException(e), _.heads.contains("sds-approved"))
        Right(if ledgerPublished then "published" else "failed")
      case other => Left(s"no handler for workflow step '$other'")
    ).fold(e => throw RuntimeException(e), identity)

    if runner.completed != wf.steps.map(_.name) then
      throw RuntimeException(s"workflow runner incomplete: ${runner.completed}")

    val hubs = SdsDomainTree.requirePlanted(branches).fold(e => throw RuntimeException(e), identity)
    val authorMan = branches.load("sds-author")
    if !authorMan.primaryAncestor.contains("SDS") then
      throw RuntimeException(s"domain: sds-author primary=${authorMan.primaryAncestor}, expected SDS")
    // Advances must keep hub ancestry (approve tip under SDS).
    if !branches.load("sds-approved").primaryAncestor.contains("SDS") then
      throw RuntimeException("domain: sds-approved lost SDS primary after commitTip")

    val industrial = industrialTip.getOrElse(throw RuntimeException("missing industrial tip"))
    Report(
      workflowDigest = SdsWorkflow.causalModule.digest,
      workflowSteps = wf.steps.map(_.name),
      certificateKindTags = certKinds,
      authorDigest = base.digest,
      industrialDigest = industrial.tipDigest,
      rebaseMerged = rebaseMerged,
      conflictOverlap = conflictOverlap,
      approvedDigest = approvedDigest,
      historyFromManifestAlone = historyFromManifestAlone,
      verifiedCapabilityOk = verifiedCapabilityOk,
      capabilityAuthorizedPut = capabilityAuthorizedPut,
      tipSignatureHex = tipSigHex,
      certificateDigests = certificateDigests,
      ledgerPublished = ledgerPublished,
      runnerCompleted = runner.completed,
      domainTreeOk = hubs.ok,
      sdsPrimary = hubs.sds.primaryAncestor,
      sdsReferences = hubs.sds.references,
      authorPrimary = authorMan.primaryAncestor)
