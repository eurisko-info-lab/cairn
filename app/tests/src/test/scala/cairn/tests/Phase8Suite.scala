package cairn.tests

import cairn.systemhandler.{EffectContext, Filesystem, Keypair, Node}
import cairn.runtime.PackLoader
import cairn.kernel.*
import cairn.core.*
import cairn.surface.{Porcelain, Transcript}
import cairn.systeminterface.Filesystem as Fs
import cairn.examples.stlc.Stlc
import scala.jdk.CollectionConverters.*

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
        assert(report.summaries.exists(_.startsWith("published main")), report.render)
        assert(report.summaries.exists(_.startsWith("fetched main")), report.render)
        assert(report.summaries.count(_.startsWith("eval")) == 3, report.render)
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
      "repository-workflow", "chain-sync", "pki-surface", "law-surface", "sds-surface",
      "e2e-path", "chain-divergence", "patch-conflict", "multi-language",
      "minitt-surface", "leancore-surface", "unisoncore-surface")
  do
    test(s"imported transcript $name.cairn runs end-to-end"):
      val src = readTranscriptSource(
        List(s"transcripts/$name.cairn", s"../transcripts/$name.cairn"),
        s"transcripts/$name.cairn not found")
      val work = java.nio.file.Files.createTempDirectory(s"cairn-$name")
      Transcript.run(
          src, packs.loadClosed(), work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
        case Right(report) =>
          assert(
            report.summaries.exists(_.startsWith("published")) ||
              report.summaries.exists(_.startsWith("expected failure")),
            report.render)
          assert(
            report.summaries.exists(_.startsWith("fetched")) ||
              report.summaries.exists(_.contains("gossip")) ||
              report.summaries.exists(_.startsWith("expected failure")),
            report.render)
        case Left(e) => fail(e)

  private lazy val expectedDispositions: Map[String, String] =
    val candidates =
      List("transcripts/charb/dispositions.tsv", "../transcripts/charb/dispositions.tsv")
        .map(java.nio.file.Path.of(_))
    val path = candidates.find(java.nio.file.Files.isRegularFile(_)).getOrElse(
      fail("transcripts/charb/dispositions.tsv missing — run scripts/gen-charb-transcripts.py"))
    val text = Filesystem.run(Fs.Request.Read(Fs.Path(path.toString)), fsCtx) match
      case Right(Fs.Response.Text(s)) => s
      case other => fail(s"read dispositions: $other")
    text.linesIterator
      .map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#"))
      .map { line =>
        val parts = line.split("\t", 3)
        require(parts.length >= 2, s"bad dispositions row: $line")
        parts(0) -> parts(1)
      }.toMap

  private lazy val dispositionHashes: Map[String, String] =
    val candidates =
      List("transcripts/charb/dispositions.tsv", "../transcripts/charb/dispositions.tsv")
        .map(java.nio.file.Path.of(_))
    val path = candidates.find(java.nio.file.Files.isRegularFile(_)).getOrElse(
      fail("transcripts/charb/dispositions.tsv missing"))
    val text = Filesystem.run(Fs.Request.Read(Fs.Path(path.toString)), fsCtx) match
      case Right(Fs.Response.Text(s)) => s
      case other => fail(s"read dispositions: $other")
    text.linesIterator
      .map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#"))
      .map { line =>
        val parts = line.split("\t", 3)
        require(parts.length >= 3 && parts(2).nonEmpty,
          s"missing source sha256 for ${parts.lift(0).getOrElse("?")}: run gen with --source")
        parts(0) -> parts(2)
      }.toMap

  test("Charb dispositions pin source sha256 and SOURCES.md counts match ledger"):
    assertEquals(dispositionHashes.size, 85)
    assert(dispositionHashes.values.forall(_.matches("[0-9a-f]{64}")), "expected sha256 hex")
    val src = readTranscriptSource(
      List("transcripts/SOURCES.md", "../transcripts/SOURCES.md"),
      "SOURCES.md missing")
    val counts = expectedDispositions.values.groupBy(identity).view.mapValues(_.size).toMap
    assert(src.contains(s"| Rich / thin runnable | ${counts("runnable")} |"), src)
    assert(src.contains(s"| **Porcelain-promoted** | ${counts("porcelain")} |"), src)
    assert(src.contains(s"| Still `deferred` | ${counts("deferred")} |"), src)

  test("all Charb YAML ports under transcripts/charb/ run"):
    val dirCandidates =
      List("transcripts/charb", "../transcripts/charb").map(java.nio.file.Path.of(_))
    val dir = dirCandidates.find(java.nio.file.Files.isDirectory(_)).getOrElse(
      fail("transcripts/charb/ missing"))
    val files =
      java.nio.file.Files.list(dir).iterator().asScala
        .filter(p => p.getFileName.toString.endsWith(".cairn"))
        .toList.sortBy(_.getFileName.toString)
    assertEquals(files.length, 85, s"expected 85 Charb ports, found ${files.length}")
    var deferred = 0
    var runnable = 0
    var porcelain = 0
    for f <- files do
      val src = Filesystem.run(Fs.Request.Read(Fs.Path(f.toString)), fsCtx) match
        case Right(Fs.Response.Text(s)) => s
        case other => fail(s"read ${f.getFileName}: $other")
      val work = java.nio.file.Files.createTempDirectory(s"cairn-charb-${f.getFileName}")
      Transcript.run(
          src, packs.loadClosed(), work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
        case Right(report) =>
          val disposition =
            if report.steps.exists {
                  case Transcript.StepOutcome.Deferred(_, _) => true
                  case _                                     => false
                }
            then "deferred"
            else if report.steps.exists {
                  case Transcript.StepOutcome.Executed(Canon.CTag("porcelain", _)) => true
                  case _ => false
                }
            then "porcelain"
            else "runnable"
          val ok =
            disposition == "deferred" || disposition == "porcelain" ||
              report.summaries.exists(_.startsWith("published")) ||
              report.summaries.exists(_.startsWith("expected failure"))
          assert(ok, report.render)
          disposition match
            case "deferred"  => deferred += 1
            case "porcelain" => porcelain += 1
            case _           => runnable += 1
          val name = f.getFileName.toString.stripSuffix(".cairn")
          expectedDispositions.get(name) match
            case Some(want) =>
              assertEquals(disposition, want, s"$name disposition drift")
            case None =>
              fail(s"missing pinned disposition for $name (regenerate dispositions.tsv)")
        case Left(e) => fail(s"${f.getFileName}: $e")
    assertEquals(runnable + porcelain + deferred, 85)
    assertEquals(deferred, expectedDispositions.values.count(_ == "deferred"))
    assertEquals(porcelain, expectedDispositions.values.count(_ == "porcelain"))
    assertEquals(runnable, expectedDispositions.values.count(_ == "runnable"))

  test("porcelain CLI auth check and chain status"):
    val home = java.nio.file.Files.createTempDirectory("cairn-porcelain")
    assert(Porcelain.dispatch(List("auth", "check", "alice"), home, packs, ledgerCtx)
      .exists(_.contains("ALLOW")))
    assert(Porcelain.dispatch(List("chain", "status"), home, packs, ledgerCtx)
      .exists(_.contains("chain status")))
    assert(Porcelain.dispatch(List("porcelain", "authorization"), home, packs, ledgerCtx)
      .exists(_.contains("auth check")))
    assert(Porcelain.dispatch(List("catalog", "export"), home, packs, ledgerCtx)
      .exists(_.contains("catalog export")))

  test("granit-rust ledger-settlement is honestly deferred"):
    val src = readTranscriptSource(
      List("transcripts/granit-rust/ledger-settlement.cairn",
        "../transcripts/granit-rust/ledger-settlement.cairn"),
      "ledger-settlement.cairn missing")
    val work = java.nio.file.Files.createTempDirectory("cairn-ledger-settlement")
    Transcript.run(src, Map.empty, work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
      case Right(report) =>
        assert(report.steps.exists {
          case Transcript.StepOutcome.Deferred(_, _) => true
          case _                                     => false
        }, report.render)
      case Left(e) => fail(e)

  test("sds-domain-journey transcript plants PKI→LAW→SDS + CHEMISTRY ref"):
    val src = readTranscriptSource(
      List("transcripts/sds-domain-journey.cairn", "../transcripts/sds-domain-journey.cairn"),
      "sds-domain-journey.cairn missing")
    val work = java.nio.file.Files.createTempDirectory("cairn-sds-domain")
    Transcript.run(src, Map.empty, work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
      case Right(report) =>
        assert(report.summaries.exists(_.startsWith("fork-from SDS of LAW")), report.render)
        assert(report.summaries.exists(_.startsWith("refer SDS to CHEMISTRY")), report.render)
        assert(report.summaries.exists(_.startsWith("fork-from sds-author of SDS")), report.render)
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
