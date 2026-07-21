package cairn.tests

import cairn.systemhandler.{CasEffects, DiskCas, EffectContext}
import cairn.kernel.*
import cairn.workbench.*
import cairn.surface.*
import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.examples.pki.PkiMax
import cairn.ledger.Keypair

/** Wave H part 2 (M43–M49). */
class WaveH2Suite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(300, "s")

  private val packs = PackLoader(EffectContext.forPackLoader())
  private val Pki = cairn.examples.pki.Pki(packs)
  private val Sds = cairn.examples.sds.Sds(packs)
  private val SearchPack = cairn.examples.search.Search(packs)
  private val Riemann = cairn.examples.riemann.Riemann(packs)
  private val UnisonCore = cairn.examples.unison.UnisonCore(packs)
  private val Unison = cairn.examples.unison.Unison(UnisonCore)
  private val ledgerCtx = EffectContext.forLedger()
  private val processCtx = EffectContext.forProcess()
  private val lspCtx = EffectContext.forLsp()
  private val fsCtx = EffectContext.forFilesystem()

  // ---- M43: capability manifests ----

  test("M43: manifests for shipped languages, lint enforced"):
    for l <- List(Stlc.language, Pki.language, Sds.language,
                  Query.language, cairn.ledger.PolicyLang.language) do
      val surf = packs.surfacesFor(l.name).map((n, s) => n -> s.digest)
      val m = Capabilities.build(l, Map.empty, surf).fold(e => fail(e), identity)
      assertEquals(m.artifact.kind, ArtifactKind.Capability)
      assertEquals(Capabilities.requiredRows.toSet, m.rows.keySet)
      // STLC has rules + judgments; PKI/SDS/policy at least grammar + ΔL
      assert(m.render.contains("changes"))
    // lint fails on undeclared/missing rows
    val stlcM = Capabilities.build(Stlc.language, Map.empty,
      packs.surfacesFor("stlc").map((n, s) => n -> s.digest)).toOption.get
    assert(Capabilities.lint(stlcM.copy(rows = stlcM.rows - "traces")).isLeft)
    assert(Capabilities.lint(stlcM.copy(rows = stlcM.rows + ("bogus" -> Capabilities.Row.Deferred("x")))).isLeft)

  test("M43: Riemann/Search manifests build; obligations row can cite a real Claim"):
    val riemannM = Capabilities.build(Riemann.language, Map.empty)
      .fold(e => fail(e), identity)
    assertEquals(Capabilities.requiredRows.toSet, riemannM.rows.keySet)
    // Default "obligations" is PlatformProvided (not a CAS key) — Capabilities lives
    // in L1 and can't see example-layer Claims (rule 11 layering). `extra` overrides
    // with Present(claimDigest) when a pack has a real Claim artifact.
    assert(riemannM.rows("obligations").isInstanceOf[Capabilities.Row.PlatformProvided])
    val riemannClaim = Riemann.riemannHypothesisClaim
    val riemannClaimM = Capabilities.build(Riemann.language,
      Map("obligations" -> Capabilities.Row.Present(riemannClaim.artifact.digest)))
      .fold(e => fail(e), identity)
    assertEquals(riemannClaimM.rows("obligations"), Capabilities.Row.Present(riemannClaim.artifact.digest))
    assert(riemannClaimM.rows("obligations") != riemannM.rows("obligations"),
      "override must promote PlatformProvided → Present(claim digest)")

    val searchClaim = SearchPack.goalMetClaim(cairn.examples.search.Search.seedBoard)
    val searchM = Capabilities.build(SearchPack.language,
      Map("obligations" -> Capabilities.Row.Present(searchClaim.artifact.digest)))
      .fold(e => fail(e), identity)
    assertEquals(Capabilities.requiredRows.toSet, searchM.rows.keySet)
    assert(searchM.rows("judgments").isInstanceOf[Capabilities.Row.Present],
      "search.cairn declares wellFormed/goalMet judgments")

  test("M43: interpreters/judgments present for stlc; platform vs deferred honest"):
    val m = Capabilities.build(Stlc.language, Map.empty,
      packs.surfacesFor("stlc").map((n, s) => n -> s.digest)).toOption.get
    assert(m.rows("interpreters").isInstanceOf[Capabilities.Row.Present])
    assert(m.rows("judgments").isInstanceOf[Capabilities.Row.Present])
    assert(m.rows("grammar").isInstanceOf[Capabilities.Row.Present])
    assert(m.rows("surfaces").isInstanceOf[Capabilities.Row.Present])
    assert(m.rows("changes").isInstanceOf[Capabilities.Row.Present])
    m.rows("traces") match
      case Capabilities.Row.PlatformProvided("eval-trace", _) => ()
      case other => fail(s"traces should be PlatformProvided(eval-trace), got $other")
    m.rows("migrations") match
      case Capabilities.Row.PlatformProvided("lang-migration", _) => ()
      case other => fail(s"migrations should be PlatformProvided(lang-migration), got $other")
    m.rows("laws") match
      case Capabilities.Row.PlatformProvided("parse-print-roundtrip", _) => ()
      case other => fail(s"laws should be PlatformProvided(parse-print-roundtrip), got $other")
    assert(m.rows("workflows").isInstanceOf[Capabilities.Row.Deferred])
    assert(m.render.contains("platform:eval-trace"), m.render)
    assert(m.render.contains("deferred:"), m.render)

  // ---- M44: LSP + REPL ----

  def stlcTypeOf(term: Cst): Either[String, String] =
    val cfg = CheckerCfg(Stlc.language.judgments.values.toList,
      binderSpec = Stlc.language.binderSpec, varCtor = "var")
    Search.infer(cfg, Cst.node("hasType", Cst.node("ctxNil"), term, Cst.Leaf("$T")))
      .flatMap(_.conclusion match
        case Cst.Node("hasType", List(_, _, ty)) =>
          Printer.print(Stlc.language.grammar, ty)
        case other => Left(s"odd conclusion ${other.render}"))

  val lspCfg = LspConfig(Stlc.language, typeOf = Some(stlcTypeOf))

  def openDoc(server: LspServer, uri: String, text: String): List[Cst] =
    server.handle(J.obj(
      "jsonrpc" -> J.str("2.0"), "method" -> J.str("textDocument/didOpen"),
      "params" -> J.obj("textDocument" -> J.obj("uri" -> J.str(uri), "text" -> J.str(text)))))

  test("M44: diagnostics on broken doc, clean on fixed doc"):
    val server = LspServer(lspCfg)
    val bad = openDoc(server, "file:///m.stlc", "id = fun x : . x ;")
    val diags = bad.head
    assert(J.print(diags).contains("publishDiagnostics"))
    assert(J.print(diags).contains("expected"), J.print(diags))
    val good = openDoc(server, "file:///m.stlc", "id = fun x : Bool . x ;")
    assert(J.print(good.head).contains("\"diagnostics\": []"), J.print(good.head))

  test("M44: formatting = the generic printer"):
    val server = LspServer(lspCfg)
    openDoc(server, "file:///m.stlc", "id   =   fun x :    Bool .   x ;")
    val resp = server.handle(J.obj(
      "jsonrpc" -> J.str("2.0"), "id" -> J.num(1), "method" -> J.str("textDocument/formatting"),
      "params" -> J.obj("textDocument" -> J.obj("uri" -> J.str("file:///m.stlc")))))
    assert(J.print(resp.head).contains("id = fun x : Bool . x ;"), J.print(resp.head))

  test("M44: rename is the generic ΔL rename and emits a ValidatedChangeSet"):
    val server = LspServer(lspCfg)
    val text = "id = fun x : Bool . x ;\nuse = (id true) ;"
    openDoc(server, "file:///m.stlc", text)
    val resp = server.handle(J.obj(
      "jsonrpc" -> J.str("2.0"), "id" -> J.num(2), "method" -> J.str("textDocument/rename"),
      "params" -> J.obj(
        "textDocument" -> J.obj("uri" -> J.str("file:///m.stlc")),
        "position" -> J.obj("line" -> J.num(0), "character" -> J.num(1)),
        "newName" -> J.str("ident"))))
    val out = J.print(resp.head)
    assert(out.contains("ident = fun x : Bool . x ;"), out)
    assert(out.contains("(ident true)"), out) // footprint-aware reference update
    assertEquals(server.changeSets.length, 1)  // kernel-gated change-set emitted
    assert(server.changeSets.head.result != server.changeSets.head.base)

  test("M44: rename preserves comments/formatting on untouched defs (format-preserving apply)"):
    val server = LspServer(lspCfg)
    val text = "-- id's own comment\nid = fun x : Bool . x ;\n-- unrelated comment, untouched def\nother = true ;\n"
    openDoc(server, "file:///m2.stlc", text)
    val resp = server.handle(J.obj(
      "jsonrpc" -> J.str("2.0"), "id" -> J.num(4), "method" -> J.str("textDocument/rename"),
      "params" -> J.obj(
        "textDocument" -> J.obj("uri" -> J.str("file:///m2.stlc")),
        "position" -> J.obj("line" -> J.num(1), "character" -> J.num(1)),
        "newName" -> J.str("ident"))))
    val out = J.print(resp.head)
    assert(out.contains("ident = fun x : Bool . x ;"), out)
    assert(out.contains("id's own comment"), out)
    assert(out.contains("unrelated comment, untouched def"), out)
    assert(out.contains("other = true ;"), out)

  def execCommand(server: LspServer, reqId: Long, command: String, args: List[Cst]): Cst =
    server.handle(J.obj(
      "jsonrpc" -> J.str("2.0"), "id" -> J.num(reqId), "method" -> J.str("workspace/executeCommand"),
      "params" -> J.obj("command" -> J.str(command), "arguments" -> J.arr(args)))).head

  test("workspace/executeCommand cairn.addDef: appends, preserves existing comments"):
    val server = LspServer(lspCfg)
    val uri = "file:///add.stlc"
    val text = "-- a's comment\na = true ;\n"
    openDoc(server, uri, text)
    val resp = execCommand(server, 10, "cairn.addDef", List(J.str(uri), J.str("b"), J.str("false")))
    val out = J.print(resp)
    assert(out.contains("b = false ;"), out)
    assert(out.contains("a's comment"), out)
    assertEquals(server.changeSets.length, 1)
    assert(server.changeSets.head.result != server.changeSets.head.base)

  test("workspace/executeCommand cairn.replaceDef: touches only the target def"):
    val server = LspServer(lspCfg)
    val uri = "file:///replace.stlc"
    val text = "-- a's comment\na = true ;\n-- b's comment\nb = false ;\n"
    openDoc(server, uri, text)
    val resp = execCommand(server, 11, "cairn.replaceDef", List(J.str(uri), J.str("a"), J.str("false")))
    val out = J.print(resp)
    assert(out.contains("a's comment"), out)
    assert(out.contains("a = false ;"), out)
    assert(out.contains("b's comment"), out)
    assert(out.contains("b = false ;"), out)

  test("workspace/executeCommand cairn.removeDef: drops the def and its own comment"):
    val server = LspServer(lspCfg)
    val uri = "file:///remove.stlc"
    val text = "-- a's comment\na = true ;\n-- b's comment\nb = false ;\n"
    openDoc(server, uri, text)
    val resp = execCommand(server, 12, "cairn.removeDef", List(J.str(uri), J.str("a")))
    val out = J.print(resp)
    assert(!out.contains("a's comment"), out)
    assert(!(out.contains("a = true") || out.contains("a = false")), out)
    assert(out.contains("b's comment"), out)
    assert(out.contains("b = false ;"), out)

  test("workspace/executeCommand cairn.editDefAt: touches only the targeted subterm"):
    val server = LspServer(lspCfg)
    val uri = "file:///edit.stlc"
    val text = "-- c comment\nc = (f x) ; -- inline\n"
    openDoc(server, uri, text)
    val resp = execCommand(server, 13, "cairn.editDefAt", List(J.str(uri), J.str("c"), J.arr(List(J.num(1))), J.str("y")))
    val out = J.print(resp)
    assert(out.contains("(f y)"), out)
    assert(out.contains("c comment"), out)
    assert(out.contains("inline"), out)

  test("workspace/executeCommand: unknown command and missing args fail cleanly"):
    val server = LspServer(lspCfg)
    val uri = "file:///err.stlc"
    openDoc(server, uri, "a = true ;\n")
    val bad = execCommand(server, 14, "cairn.bogus", List(J.str(uri)))
    assert(J.print(bad).contains("\"error\""), J.print(bad))
    val missing = execCommand(server, 15, "cairn.addDef", List(J.str(uri)))
    assert(J.print(missing).contains("\"error\""), J.print(missing))

  test("M44: hover reports the inferred type"):
    val server = LspServer(lspCfg)
    openDoc(server, "file:///m.stlc", "id = fun x : Bool . x ;")
    val resp = server.handle(J.obj(
      "jsonrpc" -> J.str("2.0"), "id" -> J.num(3), "method" -> J.str("textDocument/hover"),
      "params" -> J.obj(
        "textDocument" -> J.obj("uri" -> J.str("file:///m.stlc")),
        "position" -> J.obj("line" -> J.num(0), "character" -> J.num(1)))))
    assert(J.print(resp.head).contains("id : Bool -> Bool"), J.print(resp.head))

  test("M44: framed transport round-trips one initialize"):
    val request = """{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}}"""
    val exitNote = """{"jsonrpc": "2.0", "method": "exit"}"""
    val inBytes = new java.io.ByteArrayOutputStream()
    cairn.systemhandler.LspTransport.writeMessage(inBytes, request)
    cairn.systemhandler.LspTransport.writeMessage(inBytes, exitNote)
    val out = new java.io.ByteArrayOutputStream()
    Lsp.serve(lspCfg, new java.io.ByteArrayInputStream(inBytes.toByteArray), out, lspCtx)
    val response = out.toString
    assert(response.contains("Content-Length:"), response)
    assert(response.contains("renameProvider"), response)

  test("M44: REPL evals, applies deltas, lists defs"):
    val repl = Repl(Stlc.language)
    assertEquals(repl.eval("((fun x : Bool . x) true)"), "true")
    assert(repl.eval(":delta { add id = fun x : Bool . x ; }").startsWith("module"))
    assert(repl.eval(":defs").contains("id = fun x : Bool . x"))
    assert(repl.eval("(fun x : . x)").startsWith("error:"))

  // ---- M45: query language ----

  val queryModule = Module(List(
    "id" -> Stlc.idBool,
    "k" -> Stlc.churchTrue,
    "flag" -> Stlc.tru,
    "call" -> Stlc.app1(Stlc.idBool, Stlc.tru)))

  def inferTy(t: Cst): Either[String, Cst] =
    val cfg = CheckerCfg(Stlc.language.judgments.values.toList,
      binderSpec = Stlc.language.binderSpec, varCtor = "var")
    Search.infer(cfg, Cst.node("hasType", Cst.node("ctxNil"), t, Cst.Leaf("$T")))
      .map(_.conclusion match { case Cst.Node("hasType", List(_, _, ty)) => ty; case o => o })

  test("M45: three golden queries; results are artifacts; ΔQuery exists"):
    // 1. structural: all defs containing a lambda
    val q1 = Query.parse("defs matching lam($x, $T, $b)").fold(e => fail(e), identity)
    val r1 = Query.run(q1, queryModule).fold(e => fail(e), identity)
    assertEquals(r1.hits.map(_._1).toSet, Set("id", "k", "call"))
    // 2. type-driven: all defs whose type is an arrow
    val q2 = Query.parse("defs typed arrow($a, $b)").fold(e => fail(e), identity)
    val r2 = Query.run(q2, queryModule, typeOf = Some(inferTy)).fold(e => fail(e), identity)
    assertEquals(r2.hits.map(_._1).toSet, Set("id", "k"))
    // 3. CAS: artifacts by kind
    val dir = java.nio.file.Files.createTempDirectory("cairn-query")
    val cas = DiskCas(dir)
    val casCtx = EffectContext.forCas()
    val claimArt = cairn.proof.Claim("c1", Cst.node("x"), Stlc.language.digest).artifact
    val baseArt = Stlc.base.artifact
    CasEffects.put(cas, claimArt, casCtx).fold(e => fail(e.toString), identity)
    CasEffects.put(cas, baseArt, casCtx).fold(e => fail(e.toString), identity)
    val q3 = Query.parse("artifacts kind claim").fold(e => fail(e), identity)
    val r3 = Query.run(q3, Module(Nil), artifacts = Some(List(claimArt, baseArt))).fold(e => fail(e), identity)
    assertEquals(r3.hits.length, 1)
    // results are artifacts; the query language is a language (ΔQuery forced)
    assertEquals(r1.artifact.kind, ArtifactKind.QueryResult)
    assert(Delta.deltaOf(Query.language).isRight)
    RoundTrip.check(Query.language.grammar, q1).fold(e => fail(e), identity)

  // ---- M46: PKI maximal ----

  val root = Keypair.dev("root")
  val inter = Keypair.dev("inter")
  val leaf = Keypair.dev("leaf")
  val now = 1000L

  def goodRegistry: Cst = PkiMax.registryCtx(List(
    "root" -> PkiMax.certTerm("root", root, root, 0, 2000),
    "inter" -> PkiMax.certTerm("inter", inter, root, 0, 2000),
    "leaf" -> PkiMax.certTerm("leaf", leaf, inter, 500, 1500)))

  test("M46: declarative chain judgment checked by the SAME kernel checker"):
    val d = PkiMax.validate(goodRegistry, "leaf", now, Set("root")).fold(e => fail(e), identity)
    assertEquals(d.rule, "chain-step")
    // the identical Checker.check API that validates STLC typing validates chains
    assertEquals(Checker.check(PkiMax.checkerCfg(Set("root")), d), Right(()))

  test("M46: expiry windows enforced via $le side conditions"):
    // leaf valid only in [500, 1500]
    assert(PkiMax.validate(goodRegistry, "leaf", 100, Set("root")).isLeft)  // not yet valid
    assert(PkiMax.validate(goodRegistry, "leaf", 1800, Set("root")).isLeft) // expired
    assert(PkiMax.validate(goodRegistry, "leaf", 1000, Set("root")).isRight)

  test("M46: forged signature / wrong anchor / revoked issuer rejected"):
    // forged: leaf cert claims issuer inter but signed by root
    val forged = PkiMax.registryCtx(List(
      "root" -> PkiMax.certTerm("root", root, root, 0, 2000),
      "inter" -> PkiMax.certTerm("inter", inter, root, 0, 2000),
      "leaf" -> PkiMax.certTerm("leaf", leaf, root.copy(name = "inter"), 0, 2000)))
    assert(PkiMax.validate(forged, "leaf", now, Set("root")).isLeft)
    // self-signed non-anchor
    val rogue = Keypair.dev("rogue")
    val rogueReg = PkiMax.registryCtx(List("rogue" -> PkiMax.certTerm("rogue", rogue, rogue, 0, 2000)))
    assert(PkiMax.validate(rogueReg, "rogue", now, Set("root")).isLeft)
    // CRL revokes the intermediate: chain must break
    val crl = PkiMax.Crl.issue(root, List("inter"))
    assert(PkiMax.Crl.verify(crl, root.publicBytes))
    val revoked = PkiMax.applyCrl(goodRegistry, crl)
    assert(PkiMax.validate(revoked, "leaf", now, Set("root")).isLeft)
    assert(PkiMax.validate(revoked, "root", now, Set("root")).isRight)
    // tampered CRL fails verification
    assert(!PkiMax.Crl.verify(crl.copy(revoked = List("root")), root.publicBytes))

  test("M46: CRL is a ledger-anchorable artifact"):
    val crl = PkiMax.Crl.issue(root, List("inter"))
    assertEquals(crl.artifact.kind, ArtifactKind.Certificate)

  // ---- M47: SDS ----

  def acetone: Module = Module(List(
    "acetone" -> Cst.node("substance", Cst.Leaf("67-64-1"), Cst.Leaf("Acetone")),
    "h225" -> Cst.node("phrase", Cst.Leaf("h225"), Cst.Leaf("en"),
      Cst.Leaf("Highly flammable liquid and vapour")),
    "h319" -> Cst.node("phrase", Cst.Leaf("h319"), Cst.Leaf("en"),
      Cst.Leaf("Causes serious eye irritation")),
    "cleaner" -> Cst.node("mixture", Cst.Node("list", List(
      Cst.node("component", Cst.Leaf("acetone"), Cst.Leaf("60"))))),
    "cleanerProduct" -> Cst.node("product", Cst.Leaf("Acetone Cleaner"), Cst.Leaf("cleaner"),
      Cst.Node("list", List(Cst.Leaf("h225"), Cst.Leaf("h319")))))).sorted

  test("M47: acetone SDS builds; objects round-trip the sds surface"):
    Sds.validate(acetone).fold(e => fail(e), identity)
    for (_, term) <- acetone.defs do
      RoundTrip.check(Sds.language.grammar, term).fold(e => fail(e), identity)

  test("M47: ΔSDS = generic ΔL + domain gate (percentages, broken refs)"):
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    // over-100% mixture rejected by the domain gate
    val overload = Parser.parse(dl.grammar,
      """{ replace cleaner = mixture of ( acetone pct 160 ) ; }""").fold(e => fail(e), identity)
    assert(Sds.applySds(acetone, overload).swap.exists(_.contains("sum to 160")))
    // broken reference rejected
    val broken = Parser.parse(dl.grammar,
      """{ replace cleaner = mixture of ( phantom pct 10 ) ; }""").fold(e => fail(e), identity)
    assert(Sds.applySds(acetone, broken).swap.exists(_.contains("unknown substance 'phantom'")))
    // legitimate change passes
    val ok = Parser.parse(dl.grammar,
      """{ replace cleaner = mixture of ( acetone pct 80 ) ; }""").fold(e => fail(e), identity)
    assert(Sds.applySds(acetone, ok).isRight)

  test("M47: shadow overrides one phrase; render round-trips; pack publishes"):
    val doc0 = Sds.render(acetone, "cleanerProduct", "en").fold(e => fail(e), identity)
    assert(doc0.contains("Highly flammable liquid and vapour"), doc0)
    // shadow override via ΔSDS
    val dl = Delta.deltaOf(Sds.language).toOption.get
    val addShadow = Parser.parse(dl.grammar,
      """{ add corpShadow = shadow cleanerProduct overrides h225 with "Extremely flammable - corporate wording" ; }""")
      .fold(e => fail(e), identity)
    val Right((m2, _)) = Sds.applySds(acetone, addShadow): @unchecked
    val doc1 = Sds.render(m2, "cleanerProduct", "en").fold(e => fail(e), identity)
    assert(doc1.contains("Extremely flammable - corporate wording"), doc1)
    assert(!doc1.contains("Highly flammable liquid and vapour"), doc1)
    assert(doc1.contains("Causes serious eye irritation"), doc1) // non-shadowed intact
    // ledger publish
    val alice = Keypair.dev("alice")
    val node = cairn.ledger.Node(java.nio.file.Files.createTempDirectory("cairn-sds"), ledgerCtx)
    CasEffects.put(node.cas, m2.artifact, node.ctx).fold(e => fail(e.toString), identity)
    node.append(alice, Map("alice" -> alice.publicBytes), List(
      alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes)),
      alice.signTx(Tx.PublishArtifact(m2.artifact.key)),
      alice.signTx(Tx.SetBranchHead("sds", m2.artifact.key)))).fold(e => fail(e), identity)
    assertEquals(node.state(Map("alice" -> alice.publicBytes)).map(_.heads.contains("sds")), Right(true))

  // ---- M48: Unison-inspired pack ----

  test("M48: definitions share digests regardless of names; renames change nothing"):
    val cb0 = Unison.Codebase.empty
      .define("identity", Stlc.idBool)
      .define("konst", Stlc.churchTrue)
    // an alpha-variant under a different name: SAME stored definition
    val cb1 = cb0.define("id2", Stlc.lam1("y", Stlc.tBool, Stlc.v("y")))
    assertEquals(cb1.store.size, 2) // idBool and id2 dedup to one definition
    assertEquals(cb1.digestOf("identity"), cb1.digestOf("id2"))

  test("M48: patch (ΔL over names) renames everything; store untouched — no builds"):
    val cb = Unison.Codebase.empty
      .define("identity", Stlc.idBool)
      .define("konst", Stlc.churchTrue)
    val before = cb.names.defs.map((n, _) => n)
    val (cb2, vcs) = Unison.applyPatch(cb,
      "{ rename identity to id footprint [] ; rename konst to k footprint [] ; }")
      .fold(e => fail(e), identity)
    assertEquals(cb2.names.defs.map(_._1).toSet, Set("id", "k"))
    assertEquals(cb2.store, cb.store)                      // untouched
    assertEquals(cb2.digestOf("id"), cb.digestOf("identity")) // aliases moved, content stable
    assertEquals(vcs.artifact.kind, ArtifactKind.ChangeSet)   // patches are kernel-gated artifacts

  // ---- M49: the max transcript ----

  test("M49: transcripts/max.cairn runs end-to-end (3 nodes, gossip, ports, queries)"):
    val candidates = List("transcripts/max.cairn", "../transcripts/max.cairn").map(java.nio.file.Path.of(_))
    val path = candidates.find(java.nio.file.Files.exists(_)).getOrElse(fail("max.cairn missing"))
    val src = java.nio.file.Files.readString(path)
    val work = java.nio.file.Files.createTempDirectory("cairn-max")
    Transcript.run(src,
      Map("stlc" -> Stlc.language),
      work,
      portModules = Map("quicksort2" -> cairn.examples.quicksort.QuickSort2.module),
      packLoader = packs,
      ledgerCtx = ledgerCtx,
      processCtx = processCtx,
      fsCtx = fsCtx) match
      case Right(report) =>
        assert(report.steps.exists(_.startsWith("loaded language stlc")), report.render)
        assert(report.steps.exists(_.startsWith("gossip converged")), report.render)
        assert(report.steps.count(_.startsWith("expected failure")) == 2, report.render)
        assert(report.steps.exists(_.startsWith("query ok")), report.render)
        assert(report.steps.exists(_.startsWith("port scala")), report.render)
      case Left(e) => fail(e)

  test("M49: transcript v2 grammar round-trips and lints clean"):
    assertEquals(GrammarLint.errors(Transcript.grammar), Nil)
    val src = """transcript t { node a ; publish main on a ; expectfail "boom" eval "true" expect "false" ; }"""
    val cst = Parser.parse(Transcript.grammar, src).fold(e => fail(e), identity)
    RoundTrip.check(Transcript.grammar, cst).fold(e => fail(e), identity)
