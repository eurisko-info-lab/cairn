package cairn.examples.sds

import cairn.systemhandler.{AuthorityGate, Branches, DiskCas, Ed25519, EffectContext, Node}
import cairn.kernel.*
import cairn.kernel.Authority.*
import cairn.core.*
import cairn.ledger.Keypair
import cairn.runtime.PackLoader
import java.nio.file.{Files, Path}

/** SDS verified-application path through the causal repository.
  *
  * Scripted sequence is disk SoT ([[SdsWorkflow.causal]]: author → shadow →
  * rebase → conflict → approve → sign → publish). This host runs the
  * *effectful* steps under authority (CAS / Branches / Ed25519 / ledger) and
  * attaches judgment-checked certificate kinds ([[SdsCertificateKinds]]).
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
  )

  def run(work: Path): Report =
    val packs = PackLoader(EffectContext.forPackLoader())
    val Sds = cairn.examples.sds.Sds(packs)
    val lang = Sds.language
    val dl = Delta.deltaOf(lang).fold(e => throw RuntimeException(e.map(_.render).mkString), identity)
    def parse(src: String): Cst =
      Parser.parse(dl.grammar, src).fold(e => throw RuntimeException(e), identity)

    // Load + judgment-check workflow / certificate packs before effectful steps.
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
    // Language-checked chemical path (outline + EU-CLP) is orthogonal to the
    // phrase-corpus acetoneBase used for causal rebase/conflict demos.
    EuClp.conform(Chemicals.Acetone.thinModule) match
      case r if !r.ok => throw RuntimeException(s"EU-CLP conform: ${r.errors.mkString("; ")}")
      case _ => ()

    // 1. Author
    branches.commitModule("sds-author", base)

    // 2. Shadow tip on industrial branch
    val shadowCs = parse(
      """{ add industrial = shadow cleanerProduct overrides h225 with "Extremely flammable - industrial grade" ; }""")
    val industrialTip = SemanticRepository.tipAfter(lang, base, shadowCs)
      .fold(e => throw RuntimeException(e), identity)
    branches.commitTip("sds-industrial", industrialTip)

    // 3. Disjoint rebase: base pct change vs shadow → mergeBranches
    val pctCs = parse("""{ replace cleaner = mixture of ( acetone pct 70 , secretBlend pct 15 ) ; }""")
    val pctTip = SemanticRepository.tipAfter(lang, base, pctCs)
      .fold(e => throw RuntimeException(e), identity)
    branches.commitTip("sds-base-rev", pctTip)
    val rebaseMerged = branches.mergeBranches(lang, "sds-merged", "sds-base-rev", "sds-industrial") match
      case Right(Right(_)) => true
      case Right(Left(_)) => false
      case Left(e) => throw RuntimeException(e)

    // 4. Conflict: base rewrites shadowed phrase
    val phraseCs = parse(
      """{ replace h225 = corpus phrase h225 lang en text "Base reworded flammable phrase" ; }""")
    val conflictOverlap = Sds.rebaseShadow(base, phraseCs, shadowCs) match
      case Left(c) => c.overlap
      case Right(_) => Set.empty

    // 5. Approve industrial tip on approval branch
    branches.commitTip("sds-approved", industrialTip)
    val approved = branches.headModule("sds-approved").fold(e => throw RuntimeException(e), identity)

    // 6. Sign + link certificates on branch manifest
    val alice = Keypair.dev("alice-sds")
    val tipDigest = approved.digest
    val tipSig = Ed25519.sign(alice.privateKey, tipDigest.hex.getBytes("UTF-8"))
    val tipSigHex = tipSig.map(b => f"${b & 0xff}%02x").mkString
    val certificateDigests = SdsCertificates.attachWorkflow(branches, "sds-approved", alice, tipDigest)
      .fold(e => throw RuntimeException(e), identity)
    val manifestCerts = branches.load("sds-approved").certificates
    if manifestCerts != certificateDigests then
      throw RuntimeException(s"manifest certificates mismatch: $manifestCerts vs $certificateDigests")

    val subject = Subject(alice.name)
    val putKey = EffectMeta.cas.actionKey("put")
    val policies = List(EffectPolicy(
      "sds-publish", subject, putKey, Resource("*", "*"), Decision.Allow))
    val req = EffectRequest(subject, putKey, EffectMeta.cas.resource.at(tipDigest.hex))
    val verifiedCap = PolicyEval.prove(req, policies, nowMillis = 0).flatMap { proof =>
      Authority.VerifiedCapability.fromProof(proof, policies)
    }
    val verifiedCapabilityOk = verifiedCap.isRight
    // Thread the mint into a real EffectContext — capability-first authorize
    // (empty gate policies; grant bundle alone must cover the CAS put).
    val capabilityAuthorizedPut = verifiedCap match
      case Right(cap) =>
        val capCtx = EffectContext(
          subject,
          AuthorityGate.enforcing(Nil),
          capabilities = List(cap),
          clock = () => 0L)
        capCtx.authorize(putKey, EffectMeta.cas.resource.at(tipDigest.hex)).isRight
      case Left(_) => false

    // History reachable from manifest alone (sidecars deleted)
    branches.commitTip("sds-hist", industrialTip)
    val tip2 = SemanticRepository.tipAfter(lang, industrialTip.tip, parse(
      """{ add labelShadow = shadow cleanerProduct overrides h319 with "Eye hazard - industrial SDS wording" ; }"""))
      .fold(e => throw RuntimeException(e), identity)
    branches.commitTip("sds-hist", tip2)
    val refs = work.resolve("refs")
    Files.deleteIfExists(refs.resolve("sds-hist.change"))
    Files.deleteIfExists(refs.resolve("sds-hist.changes"))
    val historyFromManifestAlone =
      branches.loadChangeHistory("sds-hist", lang).fold(e => throw RuntimeException(e), _.length)

    // 7. Publish approved head to ledger (capability bundle on branches ctx unused
    // for ledger append — ledger still uses forLedger policy path)
    val node = Node(work.resolve("ledger"), EffectContext.forLedger())
    val auth = Map(alice.name -> alice.publicBytes)
    node.append(alice, auth, List(alice.signTx(Tx.RegisterIdentity(alice.name, alice.publicBytes))))
      .fold(e => throw RuntimeException(e), identity)
    branches.publishHead("sds-approved", node, alice, auth)
      .fold(e => throw RuntimeException(e), identity)
    val ledgerPublished = node.state(auth).fold(e => throw RuntimeException(e), _.heads.contains("sds-approved"))

    Report(
      workflowDigest = SdsWorkflow.causalModule.digest,
      workflowSteps = wf.steps.map(_.name),
      certificateKindTags = certKinds,
      authorDigest = base.digest,
      industrialDigest = industrialTip.tipDigest,
      rebaseMerged = rebaseMerged,
      conflictOverlap = conflictOverlap,
      approvedDigest = tipDigest,
      historyFromManifestAlone = historyFromManifestAlone,
      verifiedCapabilityOk = verifiedCapabilityOk,
      capabilityAuthorizedPut = capabilityAuthorizedPut,
      tipSignatureHex = tipSigHex,
      certificateDigests = certificateDigests,
      ledgerPublished = ledgerPublished)
