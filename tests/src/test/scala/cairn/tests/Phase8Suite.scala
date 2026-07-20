package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.ledger.*
import cairn.surface.Transcript
import cairn.examples.pki.Pki
import cairn.examples.stlc.Stlc

/** Phase 8 acceptance (S46–S47): transcript DSL runs the MVP end-to-end;
  * PKI pack with generic ΔPKI + Ed25519 chain validation + trust anchors.
  */
class Phase8Suite extends munit.FunSuite:

  test("mvp transcript runs from checkout (S46 acceptance, §9.9)"):
    val candidates = List("transcripts/mvp.cairn", "../transcripts/mvp.cairn").map(java.nio.file.Path.of(_))
    val path = candidates.find(java.nio.file.Files.exists(_)).getOrElse(fail("transcripts/mvp.cairn not found"))
    val src = java.nio.file.Files.readString(path)
    val work = java.nio.file.Files.createTempDirectory("cairn-mvp")
    Transcript.run(src, Map("stlc" -> Stlc.language), work) match
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
    Transcript.run(src, Map("stlc" -> Stlc.language), work) match
      case Left(e)  => assert(e.contains("eval mismatch"), e)
      case Right(r) => fail(s"expected failure, got ${r.render}")

  // ---- PKI (S47) ----
  val root = Keypair.dev("root")
  val alice = Keypair.dev("alice")
  val bob = Keypair.dev("bob")
  val mallory = Keypair.dev("mallory")

  def registryWith(terms: (String, Cst)*): Module = Module(terms.toList).sorted

  test("pki certificates parse and round-trip in the pki language (S47)"):
    val term = Pki.certTerm("alice", alice, root)
    val printed = cairn.workbench.Printer.print(Pki.language.grammar, term).toOption.get
    assertEquals(Parser.parse(Pki.language.grammar, printed), Right(term))

  test("ΔPKI is the generic ΔL: issue = add, revoke = remove (S47)"):
    val lang = Pki.language
    val dl = Delta.deltaOf(lang).toOption.get
    assertEquals(dl.name, "Δpki")
    val rootT = Pki.rootTerm(root)
    val aliceT = Pki.certTerm("alice", alice, root)
    // issue via parsed ΔL surface
    val rootSrc = cairn.workbench.Printer.print(lang.grammar, rootT).toOption.get
    val aliceSrc = cairn.workbench.Printer.print(lang.grammar, aliceT).toOption.get
    val change = Parser.parse(dl.grammar, s"{ add root = $rootSrc ; add alice = $aliceSrc ; }").toOption.get
    val Right((registry, _)) = Delta.apply(lang, Module(Nil), change): @unchecked
    assertEquals(registry.get("alice"), Some(aliceT))
    // revoke
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
    val registry = registryWith(
      "root" -> Pki.rootTerm(root),
      "bob" -> Pki.certTerm("bob", bob, alice)) // alice revoked/absent
    assert(Pki.validateChain(registry, "bob", Set("root"))
      .swap.exists(_.reason.contains("revoked or never issued")))

  test("self-signed non-anchor rejected (S47)"):
    val registry = registryWith("mallory" -> Pki.rootTerm(mallory))
    assert(Pki.validateChain(registry, "mallory", Set("root")).isLeft)

  test("trust anchor published on ledger (S47 acceptance)"):
    val registry = registryWith("root" -> Pki.rootTerm(root))
    val anchorDigest = Pki.anchorCertificateDigest(registry, "root").toOption.get
    val authority = Keypair.dev("authority")
    val auth = Map("authority" -> authority.publicBytes)
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-pki"))
    val txs = List(
      authority.signTx(Tx.RegisterIdentity("authority", authority.publicBytes)),
      authority.signTx(Tx.RecordCertificate(anchorDigest)))
    node.append(authority, auth, txs).toOption.get
    assertEquals(node.state(auth).map(_.certificates.contains(anchorDigest.hex)), Right(true))
