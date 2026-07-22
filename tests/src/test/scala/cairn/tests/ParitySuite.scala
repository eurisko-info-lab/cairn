package cairn.tests

import cairn.kernel.*
import cairn.ledger.Encryption
import cairn.core.*
import cairn.core.Ports2.*
import cairn.systemhandler.EffectContext
import cairn.runtime.PackLoader
import cairn.examples.pki.{DemoPki, PkiMax, PkiTutorial}
import cairn.examples.sds.{CompositionSealing, SdsTutorial}
import cairn.examples.law.LawTutorial
import cairn.examples.quicksort.QuickSortApp

/** Parity vs source top-level surfaces (GRANITE PKI/SDS/Law/encryption,
  * ROSETTA QuickSort sample app). Closes the "on par with summarized
  * projects' top-level" constitution requirement.
  */
class ParitySuite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private val packs = PackLoader(EffectContext.forPackLoader())
  private val Law = cairn.examples.law.Law(packs)
  private val Sds = cairn.examples.sds.Sds(packs)

  // ---- Encryption (GRANITE sharing/encryption) ----

  test("parity: X25519 hybrid seal/open; wrong key fails closed"):
    val alice = Encryption.generateKeyPair()
    val bob = Encryption.generateKeyPair()
    val env = Encryption.seal("payload".getBytes("UTF-8"), List("alice" -> alice.getPublic, "bob" -> bob.getPublic))
    assertEquals(env.recipientIds, Set("alice", "bob"))
    assertEquals(Encryption.open(env, "alice", alice.getPrivate).map(b => new String(b, "UTF-8")), Some("payload"))
    assertEquals(Encryption.open(env, "bob", bob.getPrivate).map(b => new String(b, "UTF-8")), Some("payload"))
    assertEquals(Encryption.open(env, "alice", bob.getPrivate), None)
    assertEquals(Encryption.open(env, "carol", alice.getPrivate), None)

  // ---- PKI tutorial (GRANITE PkiTutorial) ----

  test("parity: PKI tutorial issue/validate/revoke/tamper/publish/encrypt"):
    val work = java.nio.file.Files.createTempDirectory("cairn-pki-tut")
    val r = PkiTutorial.run(work)
    assertEquals(r.issuedNames.size, 4)
    assert(r.validationBeforeRevoke)
    assert(r.validationAfterRevoke)
    assert(r.validationAfterTamper)
    assert(r.ledgerHeads.contains("trust-anchor:root"))
    assert(r.encryptionOpenOk)

  test("parity: DemoPki hierarchy chainOk under PkiMax checker"):
    val h = DemoPki.hierarchy()
    assert(PkiMax.validate(h.registry, "leaf", 1000L, Set("root")).isRight)

  // ---- SDS tutorial (GRANITE SdsTutorial + CompositionSealing) ----

  test("parity: SDS acetone tutorial multilingual/rebase/conflict/seal/publish"):
    val work = java.nio.file.Files.createTempDirectory("cairn-sds-tut")
    val r = SdsTutorial.run(work)
    assert(r.renderedEn.contains("Highly flammable"), r.renderedEn)
    assert(r.renderedFrFallback.contains("Liquide et vapeurs"), r.renderedFrFallback)
    // h319 has no FR — falls back to EN
    assert(r.renderedFrFallback.contains("Causes serious eye irritation"), r.renderedFrFallback)
    assert(r.industrialLabel.contains("Extremely flammable - industrial grade"), r.industrialLabel)
    assert(r.rebaseOk)
    assert(r.conflictPaths.contains("h225"), r.conflictPaths.toString)
    assert(r.sealedRecovered)
    assert(r.sealedWrongKeyFails)
    assert(r.ledgerBranch)

  test("parity: SDS causal workflow author/shadow/rebase/conflict/approve/sign/publish"):
    import cairn.examples.sds.SdsCausalWorkflow
    val work = java.nio.file.Files.createTempDirectory("cairn-sds-causal")
    val r = SdsCausalWorkflow.run(work)
    assert(r.rebaseMerged, "disjoint industrial+pct merge should accept")
    assert(r.conflictOverlap.contains("h225"), r.conflictOverlap.toString)
    assertEquals(r.historyFromManifestAlone, 2)
    assert(r.verifiedCapabilityOk)
    assert(r.tipSignatureHex.nonEmpty)
    assert(r.ledgerPublished)

  test("parity: SDS composition sealing open/clear bands"):
    val base = SdsTutorial.acetoneBase
    val kp = Encryption.generateKeyPair()
    val composition = CompositionSealing.seal(base, "cleaner", Set("secretBlend"), List("reg" -> kp.getPublic))
      .fold(e => fail(e), identity)
    assert(composition.entries.exists {
      case CompositionSealing.DisclosureEntry.Open("Acetone", 60.0) => true
      case _ => false
    })
    val s = composition.entries.collectFirst { case CompositionSealing.DisclosureEntry.Sealed(x) => x }.get
    assertEquals(s.rangeBand, (10.0, 25.0))
    assertEquals(
      CompositionSealing.unseal(s, "reg", kp.getPrivate).map(r => (r.name, r.exactPercent)),
      Some(("Proprietary Degreaser Base", 15.0)))

  // ---- Law pack (GRANITE PKI→Law→SDS middle link) ----

  test("parity: Law statute round-trips; citation judgment; repeal via ΔLaw"):
    val act = cairn.examples.law.Law.modelAct
    RoundTrip.check(Law.language.grammar, act.defs.find(_._1 == "s2").get._2)
      .fold(e => fail(e), identity)
    Law.validate(act).fold(e => fail(e), identity)
    assert(act.get("authority").exists {
      case Cst.Node("enactedBy", List(Cst.Leaf("act"), Cst.Leaf("root"))) => true
      case _ => false
    })
    val dl = Delta.deltaOf(Law.language).fold(e => fail(e.map(_.render).mkString), identity)
    // repeal Section 1 — Section 2's citation becomes dangling
    val repeal = Parser.parse(dl.grammar, """{ remove s1 ; }""").fold(e => fail(e), identity)
    val after = Delta.apply(Law.language, act, repeal).fold(e => fail(e), _._1)
    val errs = Law.citationCheck(after)
    assert(errs.exists(_.contains("cites Section 1")), errs.toString)

  test("parity: Law tutorial closes over PKI; SDS language includes Law+PKI"):
    val r = LawTutorial.run()
    assert(r.languageRequiresMet && r.citationOk && r.repealDangling)
    assert(Sds.language.constructors.contains("cert"))
    assert(Sds.language.constructors.contains("enactedBy"))

  // ---- Rosetta QuickSort sample app (ROSETTA QuickSortOrdEffects entrypoints) ----

  test("parity: QuickSortApp has Nat/Ord/trace/runSample; four ports fixpoint"):
    val m = QuickSortApp.module
    assert(m.datas.exists(_.name == "Peano"))
    assert(m.defs.exists(_.name == "sortNatWithTrace"))
    assert(m.defs.exists(_.name == "runSample"))
    assert(m.effects.exists(_.name == "counter"))
    assert(m.defs.exists(_.name == "sortNatWithTrace"))
    for port <- List(ScalaPort2, LeanPort2, HaskellPort2, RustPort2) do
      PortV2.verified(port, m) match
        case Right(out) =>
          assert(
            out.text.contains("runSample") || out.text.contains("run_sample"),
            s"${port.hostName}: missing runSample\n${out.text.take(800)}")
        case Left(e) => fail(s"${port.hostName}: $e")
