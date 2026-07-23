package cairn.examples.sds
import cairn.runtime.EffectContexts

import cairn.systemhandler.{AuthorityGate, CasEffects, DiskCas, EffectContext}
import cairn.kernel.*
import cairn.kernel.Authority.*
import cairn.core.*
import cairn.runtime.{PackLoader, PolicyEvalProver}
import java.nio.file.{Files, Path}

/** Full SDS tutorial workflow over the corpus + staleness machine with
  * **capability-constrained editing** (narrow EffectContext grant bundles —
  * not `AuthorityGate.bootstrapped()` allow-all).
  *
  * Walks: corpus phrases → section fields → EN restale / derived ΔSDS →
  * industrial shadows → CAS put authorized only via a VerifiedCapability
  * covering the module digest.
  *
  * Run:
  * {{{
  * sbt "examples/runMain cairn.examples.Main sds-tutorial"
  * # or
  * sbt "examples/runMain cairn.examples.Main transcript transcripts/sds-corpus-tutorial.cairn"
  * }}}
  *
  * Report surfaces (`sds-report`) are orthogonal — this tutorial stays in SDS
  * vocabulary + ΔSDS; it does not project JSON/XML/CSV/PDF.
  */
object SdsCorpusTutorial:
  final case class Step(name: String, detail: String)

  final case class Report(
      steps: List[Step],
      baseDigest: Digest,
      corpusPhraseOfficial: Boolean,
      freeTextStaleAfterRewrite: Boolean,
      sectionFieldStaleAfterRewrite: Boolean,
      shadowApplied: Boolean,
      capabilityPutOk: Boolean,
      capabilityDeniedWithoutGrant: Boolean,
      editedDigest: Digest,
  ):
    def render: String =
      val lines = steps.map(s => s"  - ${s.name}: ${s.detail}")
      s"""SDS corpus tutorial
         |base=${baseDigest.hex}
         |edited=${editedDigest.hex}
         |corpusOfficial=$corpusPhraseOfficial
         |phraseStale=$freeTextStaleAfterRewrite
         |sectionFieldStale=$sectionFieldStaleAfterRewrite
         |shadow=$shadowApplied
         |capPut=$capabilityPutOk
         |capDenied=$capabilityDeniedWithoutGrant
         |steps:
         |${lines.mkString("\n")}
         |""".stripMargin.trim

  /** Mint a VerifiedCapability that allows Cas.put at exactly `digestHex`. */
  private def putCapability(
      subject: Subject,
      digestHex: String,
  ): Either[String, VerifiedCapability] =
    val putKey = EffectMeta.cas.actionKey("put")
    val resource = EffectMeta.cas.resource.at(digestHex)
    val policies = List(EffectPolicy(
      "sds-corpus-edit", subject, putKey, resource, Decision.Allow))
    val req = EffectRequest(subject, putKey, resource)
    PolicyEval.prove(req, policies, nowMillis = 0).flatMap { proof =>
      VerifiedCapability.fromProof(proof, policies)
    }

  def run(work: Path): Report =
    Files.createDirectories(work)
    val packs = PackLoader(EffectContexts.forPackLoader())
    val Sds = cairn.examples.sds.Sds(packs)
    val lang = Sds.language
    val dl = Delta.deltaOf(lang).fold(e => throw RuntimeException(e.map(_.render).mkString), identity)
    def parseDelta(src: String): Cst =
      Parser.parse(dl.grammar, src).fold(e => throw RuntimeException(e), identity)

    val log = List.newBuilder[Step]
    def step(name: String, detail: String): Unit = log += Step(name, detail)

    // 1. Acetone spine — corpus H-phrases + free-text product label
    val base = SdsTutorial.acetoneBase
    Sds.validate(base).fold(e => throw RuntimeException(e), identity)
    step("author", s"acetone base ${base.digest.short} (corpus + free-text phrases)")

    // 2. Corpus phrases never stale
    val h225 = PhraseStaleness.project(base, "h225")
    val corpusOfficial = h225.get("en").exists(_.state == PhraseStaleness.State.OfficialCorpus) &&
      h225.get("fr").exists(_.state == PhraseStaleness.State.OfficialCorpus)
    step("corpus", s"h225 locales=${h225.keys.toList.sorted.mkString(",")} official=$corpusOfficial")

    // 3. Free-text prodName — EN rewrite derives ΔSDS + translationState
    val (phraseRewritten, _) = PhraseStaleness.applyEnRewrite(
      Sds.applySds, lang, base, "prodName", "Acetone Cleaner — reformulated")
      .fold(e => throw RuntimeException(e), identity)
    val prodProj = PhraseStaleness.project(phraseRewritten, "prodName")
    val phraseStale = prodProj.get("fr").exists(_.state == PhraseStaleness.State.StaleBecauseSourceChanged)
    step("phrase-restale", s"prodName FR stale=$phraseStale after EN rewrite")

    // 4. Section-field restale on chemical thin module (typed locales / sectionField rows)
    val thin = Chemicals.Acetone.thinModule
    Sds.validate(thin).fold(e => throw RuntimeException(e), identity)
    EuClp.conform(thin) match
      case r if !r.ok => throw RuntimeException(s"EU-CLP: ${r.errors.mkString("; ")}")
      case _ => step("eu-clp", "Acetone.thinModule conforms")
    val (fieldRewritten, _) = SectionFieldStaleness.applyEnRewrite(
      Sds.applySds, lang, thin, "s1", "productName", "Acetone Cleaner — section rewrite")
      .fold(e => throw RuntimeException(e), identity)
    val fieldProj = SectionFieldStaleness.project(fieldRewritten, "s1", "productName")
    val fieldStale = fieldProj.get("fr").exists(_.state == PhraseStaleness.State.StaleBecauseSourceChanged)
    step("section-field-restale", s"s1/productName FR stale=$fieldStale")

    // 5. Industrial shadow on phrase-rewritten module
    val shadowCs = parseDelta(
      """{ add industrial = shadow cleanerProduct overrides h225 with "Extremely flammable - industrial grade" ; }""")
    val (shadowed, _) = Sds.applySds(phraseRewritten, shadowCs)
      .fold(e => throw RuntimeException(e), identity)
    val industrialDoc = Sds.render(shadowed, "cleanerProduct", "en")
      .fold(e => throw RuntimeException(e), identity)
    val shadowOk = industrialDoc.contains("Extremely flammable - industrial grade")
    step("shadow", s"industrial override applied=$shadowOk")

    // 6. Capability-constrained CAS put of the edited module
    val cas = DiskCas(work.resolve("cas"))
    val editor = Subject("sds-editor")
    val digHex = shadowed.digest.hex
    val cap = putCapability(editor, digHex).fold(e => throw RuntimeException(e), identity)
    val capCtx = EffectContext(
      editor,
      AuthorityGate.enforcing(Nil, PolicyEvalProver),
      capabilities = List(cap),
      clock = () => 0L)
    val putOk = CasEffects.put(cas, shadowed.artifact, capCtx).isRight
    step("cap-put", s"VerifiedCapability Cas.put@${shadowed.digest.short} ok=$putOk")

    // 7. Empty grant bundle must deny the same put
    val denyCtx = EffectContext(
      editor,
      AuthorityGate.enforcing(Nil, PolicyEvalProver),
      capabilities = Nil,
      clock = () => 0L)
    val denied = CasEffects.put(cas, shadowed.artifact, denyCtx).isLeft
    step("cap-deny", s"empty grant bundle denies put=$denied")

    Report(
      steps = log.result(),
      baseDigest = base.digest,
      corpusPhraseOfficial = corpusOfficial,
      freeTextStaleAfterRewrite = phraseStale,
      sectionFieldStaleAfterRewrite = fieldStale,
      shadowApplied = shadowOk,
      capabilityPutOk = putOk,
      capabilityDeniedWithoutGrant = denied,
      editedDigest = shadowed.digest)
