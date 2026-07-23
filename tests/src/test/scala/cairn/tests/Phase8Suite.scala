package cairn.tests

import cairn.systemhandler.{EffectContext, Filesystem, Keypair, Node}
import cairn.runtime.PackLoader
import cairn.kernel.*
import cairn.core.*
import cairn.surface.Transcript
import cairn.systeminterface.Filesystem as Fs
import cairn.examples.stlc.Stlc

/** Phase 8 acceptance (S46–S47): transcript DSL runs the MVP end-to-end;
  * PKI pack with generic ΔPKI + Ed25519 chain validation + trust anchors.
  */
class Phase8Suite extends munit.FunSuite:
  private val packs = PackLoader(EffectContext.forPackLoader())
  private val Pki = cairn.examples.pki.Pki(packs)
  private val ledgerCtx = EffectContext.forLedger()
  private val processCtx = EffectContext.forProcess()
  private val fsCtx = EffectContext.forFilesystem()

  private def readTranscriptSource(candidates: List[String], missing: String): String =
    val paths = candidates.map(java.nio.file.Path.of(_))
    val path = paths.view
      .flatMap { p =>
        Filesystem.run(Fs.Request.Exists(Fs.Path(p.toString)), fsCtx) match
          case Right(Fs.Response.Bool(true)) => Some(p)
          case _                             => None
      }
      .headOption
      .getOrElse(fail(missing))
    Filesystem.run(Fs.Request.Read(Fs.Path(path.toString)), fsCtx) match
      case Right(Fs.Response.Text(s)) => s
      case Left(e)                    => fail(e.toString)
      case Right(other)               => fail(s"unexpected fs: $other")

  test("mvp transcript runs from checkout (S46 acceptance, §9.9)"):
    val src = readTranscriptSource(
      List("transcripts/mvp.cairn", "../transcripts/mvp.cairn"),
      "transcripts/mvp.cairn not found")
    val work = java.nio.file.Files.createTempDirectory("cairn-mvp")
    Transcript.run(src, Map("stlc" -> Stlc.language), work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
      case Right(report) =>
        assert(report.steps.exists(_.startsWith("published main")), report.render)
        assert(report.steps.exists(_.startsWith("fetched main")), report.render)
        assert(report.steps.count(_.startsWith("eval")) == 3, report.render)
      case Left(e) => fail(e)

  test("transcript grammar round-trips (S46 + S11 law)"):
    val src = """transcript t { lang stlc ; eval "true" expect "true" ; publish main ; }"""
    val cst = Parser.parse(Transcript.grammar, src).toOption.get
    RoundTrip.check(Transcript.grammar, cst).fold(e => fail(e), identity)

  test("transcript failure is a structured error, not silence (S46)"):
    val src = """transcript t { lang stlc ; eval "true" expect "false" ; }"""
    val work = java.nio.file.Files.createTempDirectory("cairn-bad")
    Transcript.run(src, Map("stlc" -> Stlc.language), work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
      case Left(e)  => assert(e.contains("eval mismatch"), e)
      case Right(r) => fail(s"expected failure, got ${r.render}")

  // Adapted from granit-rust / Marble-Charb / GRANITE packs (see transcripts/SOURCES.md).
  for name <- List(
      "repository-workflow", "chain-sync", "pki-surface", "law-surface", "sds-surface")
  do
    test(s"imported transcript $name.cairn runs end-to-end"):
      val src = readTranscriptSource(
        List(s"transcripts/$name.cairn", s"../transcripts/$name.cairn"),
        s"transcripts/$name.cairn not found")
      val work = java.nio.file.Files.createTempDirectory(s"cairn-$name")
      Transcript.run(
          src, packs.loadClosed(), work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
        case Right(report) =>
          assert(report.steps.exists(_.startsWith("published")), report.render)
          assert(
            report.steps.exists(_.startsWith("fetched")) ||
              report.steps.exists(_.contains("gossip")),
            report.render)
        case Left(e) => fail(e)

  // ---- PKI (S47) ----
  val root = Keypair.dev("root")
  val alice = Keypair.dev("alice")
  val bob = Keypair.dev("bob")
  val mallory = Keypair.dev("mallory")

  def registryWith(terms: (String, Cst)*): Module = Module(terms.toList).sorted

  test("pki certificates parse and round-trip in the pki language (S47)"):
    val term = Pki.certTerm("alice", alice, root)
    val printed = cairn.core.Printer.print(Pki.language.grammar, term).toOption.get
    assertEquals(Parser.parse(Pki.language.grammar, printed), Right(term))

  test("ΔPKI free duals: issue≈add, revoke≈remove (S47)"):
    val lang = Pki.language
    val dl = Delta.deltaOf(lang).toOption.get
    assertEquals(dl.name, "Δpki")
    val rootT = Pki.rootTerm(root)
    val aliceT = Pki.certTerm("alice", alice, root)
    val rootSrc = cairn.core.Printer.print(lang.grammar, rootT).toOption.get
    val aliceSrc = cairn.core.Printer.print(lang.grammar, aliceT).toOption.get
    val change = Parser.parse(dl.grammar, s"{ add root = $rootSrc ; add alice = $aliceSrc ; }").toOption.get
    val Right((registry, _)) = Delta.apply(lang, Module(Nil), change): @unchecked
    assertEquals(registry.get("alice"), Some(aliceT))
    val revoke = Parser.parse(dl.grammar, "{ remove alice ; }").toOption.get
    val Right((r2, _)) = Delta.apply(lang, registry, revoke): @unchecked
    assertEquals(r2.get("alice"), None)

  test("chain validation accepts a good chain (S47 acceptance)"):
    val registry = registryWith(
      "root" -> Pki.rootTerm(root),
      "alice" -> Pki.certTerm("alice", alice, root),
      "bob" -> Pki.certTerm("bob", bob, alice))
    assertEquals(Pki.validateChain(registry, "bob", Set("root")),
      Right(List("bob", "alice", "root")))

  test("chain validation rejects forged signature (S47 acceptance)"):
    // mallory signs alice's cert, but the registry says root is the issuer
    val forged = Pki.certTerm("alice", alice, mallory.copy(name = "root"))
    val registry = registryWith("root" -> Pki.rootTerm(root), "alice" -> forged)
    assert(Pki.validateChain(registry, "alice", Set("root"))
      .swap.exists(_.reason.contains("does not verify")))

  test("chain validation rejects revoked issuer (S47 acceptance)"):
    // Absent issuer (never issued)
    val missing = registryWith(
      "root" -> Pki.rootTerm(root),
      "bob" -> Pki.certTerm("bob", bob, alice))
    assert(Pki.validateChain(missing, "bob", Set("root"))
      .swap.exists(_.reason.contains("never issued")))
    // Soft revoke via free ΔL `add` of a revocation object
    val full = registryWith(
      "root" -> Pki.rootTerm(root),
      "alice" -> Pki.certTerm("alice", alice, root),
      "bob" -> Pki.certTerm("bob", bob, alice))
    val dl = Delta.deltaOf(Pki.language).toOption.get
    val rev = Parser.parse(dl.grammar,
      """{ add revAlice = revoked alice reason "key compromise" at "1" ; }""").toOption.get
    val Right((revoked, _)) = Delta.apply(Pki.language, full, rev): @unchecked
    assert(Pki.validateChain(revoked, "bob", Set("root"))
      .swap.exists(_.reason.contains("revoked")))

  test("self-signed non-anchor rejected (S47)"):
    val registry = registryWith("mallory" -> Pki.rootTerm(mallory))
    assert(Pki.validateChain(registry, "mallory", Set("root")).isLeft)

  test("trust anchor published on ledger (S47 acceptance)"):
    val registry = registryWith("root" -> Pki.rootTerm(root))
    val anchorDigest = Pki.anchorCertificateDigest(registry, "root").toOption.get
    val authority = Keypair.dev("authority")
    val auth = Map("authority" -> authority.publicBytes)
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-pki"), EffectContext.forLedger())
    val txs = List(
      authority.signTx(Tx.RegisterIdentity("authority", authority.publicBytes)),
      authority.signTx(Tx.RecordCertificate(anchorDigest)))
    node.append(authority, auth, txs).toOption.get
    assertEquals(node.state(auth).map(_.certificates.contains(anchorDigest.hex)), Right(true))
