package cairn.examples.sds

import cairn.systemhandler.EffectContext
import cairn.kernel.*
import cairn.core.*
import cairn.ledger.{Encryption, Keypair, Node}
import cairn.runtime.PackLoader
import cairn.examples.pki.DemoPki

/** Acetone tutorial spine — on par with GRANITE `SdsTutorial`: build base →
  * multilingual render → industrial shadow → rebase vs conflict → seal
  * confidential composition via PKI encryption cert → publish on ledger.
  */
object SdsTutorial:
  final case class Report(
      baseDigest: Digest,
      renderedEn: String,
      renderedFrFallback: String,
      industrialLabel: String,
      rebaseOk: Boolean,
      conflictPaths: Set[String],
      sealedRecovered: Boolean,
      sealedWrongKeyFails: Boolean,
      ledgerBranch: Boolean
  )

  def acetoneBase: Module = Module(List(
    "acetone" -> Cst.node("substance", Cst.Leaf("67-64-1"), Cst.Leaf("Acetone")),
    "secretBlend" -> Cst.node("substance", Cst.Leaf("trade-secret"), Cst.Leaf("Proprietary Degreaser Base")),
    "h225" -> Cst.node("phrase", Cst.Leaf("h225"), Cst.Leaf("en"),
      Cst.Leaf("Highly flammable liquid and vapour")),
    "h225fr" -> Cst.node("phrase", Cst.Leaf("h225"), Cst.Leaf("fr"),
      Cst.Leaf("Liquide et vapeurs extremement inflammables")),
    "h319" -> Cst.node("phrase", Cst.Leaf("h319"), Cst.Leaf("en"),
      Cst.Leaf("Causes serious eye irritation")),
    // FR missing for h319 — render must fall back to EN
    "cleaner" -> Cst.node("mixture", Cst.Node("list", List(
      Cst.node("component", Cst.Leaf("acetone"), Cst.Leaf("60")),
      Cst.node("component", Cst.Leaf("secretBlend"), Cst.Leaf("15"))))),
    "cleanerProduct" -> Cst.node("product", Cst.Leaf("Acetone Cleaner"), Cst.Leaf("cleaner"),
      Cst.Node("list", List(Cst.Leaf("h225"), Cst.Leaf("h319")))),
    // Law citation (Section 3 SDS duty) — SDS → Law edge at the object level
    "regBasis" -> Cst.node("basis", Cst.Leaf("cleanerProduct"), Cst.Leaf("3")))).sorted

  def run(work: java.nio.file.Path): Report =
    val Sds = cairn.examples.sds.Sds(PackLoader(EffectContext.bootstrapped()))
    val base = acetoneBase
    Sds.validate(base).fold(e => throw RuntimeException(e), identity)
    val en = Sds.render(base, "cleanerProduct", "en").fold(e => throw RuntimeException(e), identity)
    val fr = Sds.render(base, "cleanerProduct", "fr").fold(e => throw RuntimeException(e), identity)

    // Industrial shadow overrides product label + one phrase
    val dl = Delta.deltaOf(Sds.language).fold(e => throw RuntimeException(e.map(_.render).mkString), identity)
    val addShadow = Parser.parse(dl.grammar,
      """{ add industrial = shadow cleanerProduct overrides h225 with "Extremely flammable - industrial grade" ; add labelShadow = shadow cleanerProduct overrides h319 with "Eye hazard - industrial SDS wording" ; }""")
      .fold(e => throw RuntimeException(e), identity)
    val (industrial, _) = Sds.applySds(base, addShadow).fold(e => throw RuntimeException(e), identity)
    val industrialDoc = Sds.render(industrial, "cleanerProduct", "en").fold(e => throw RuntimeException(e), identity)

    // Rebase: base changes a non-shadowed mixture pct — disjoint footprint merge
    val basePct = Parser.parse(dl.grammar,
      """{ replace cleaner = mixture of ( acetone pct 70 , secretBlend pct 15 ) ; }""")
      .fold(e => throw RuntimeException(e), identity)
    val rebase = Sds.rebaseShadow(base, basePct, addShadow)
    val rebaseOk = rebase.isRight

    // Conflict: base replaces a phrase the shadow overrides
    val basePhrase = Parser.parse(dl.grammar,
      """{ replace h225 = phrase h225 lang en text "Base reworded flammable phrase" ; }""")
      .fold(e => throw RuntimeException(e), identity)
    val conflict = Sds.rebaseShadow(base, basePhrase, addShadow)
    val conflictPaths = conflict.fold(c => c.overlap, _ => Set.empty)

    // Composition sealing via DemoPki encryption cert
    val h = DemoPki.hierarchy()
    val encKp = h.regulatorEnc.encryption.get
    val composition = CompositionSealing.seal(
      base, "cleaner", Set("secretBlend"), List("regulator" -> encKp.getPublic))
      .fold(e => throw RuntimeException(e), identity)
    val sealedEntry = composition.entries.collectFirst { case CompositionSealing.DisclosureEntry.Sealed(s) => s }.get
    val recovered = CompositionSealing.unseal(sealedEntry, "regulator", encKp.getPrivate)
      .exists(r => r.name == "Proprietary Degreaser Base" && r.exactPercent == 15.0)
    val wrong = CompositionSealing.unseal(sealedEntry, "regulator", Encryption.generateKeyPair().getPrivate).isEmpty

    // Ledger publish of industrial module
    val alice = Keypair.dev("alice")
    val node = Node(work, EffectContext.bootstrapped())
    node.cas.put(industrial.artifact)
    node.append(alice, Map("alice" -> alice.publicBytes), List(
      alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes)),
      alice.signTx(Tx.PublishArtifact(industrial.artifact.key)),
      alice.signTx(Tx.SetBranchHead("acetone-industrial", industrial.artifact.key))))
      .fold(e => throw RuntimeException(e), identity)
    val branched = node.state(Map("alice" -> alice.publicBytes))
      .fold(e => throw RuntimeException(e), _.heads.contains("acetone-industrial"))

    Report(
      baseDigest = base.artifact.digest,
      renderedEn = en,
      renderedFrFallback = fr,
      industrialLabel = industrialDoc,
      rebaseOk = rebaseOk,
      conflictPaths = conflictPaths,
      sealedRecovered = recovered,
      sealedWrongKeyFails = wrong,
      ledgerBranch = branched)
