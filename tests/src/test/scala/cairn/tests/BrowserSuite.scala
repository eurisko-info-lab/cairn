package cairn.tests

import cairn.systemhandler.{CasEffects, EffectContext, Keypair, Node}
import cairn.kernel.*
import cairn.surface.BrowserServer
import cairn.core.Module
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files

/** Browser API: navigate local node/CAS with typed artifact views. */
class BrowserSuite extends munit.FunSuite:
  private val client = HttpClient.newHttpClient()
  private val packs = cairn.runtime.PackLoader(EffectContext.forPackLoader())
  private val Search = cairn.examples.search.Search(packs)

  private def casPut(node: Node, art: Artifact): Unit =
    CasEffects.put(node.cas, art, node.ctx).fold(e => fail(e.toString), identity)

  private def get(port: Int, path: String): (Int, String) =
    val resp = client.send(
      HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$path")).GET().build(),
      HttpResponse.BodyHandlers.ofString())
    (resp.statusCode(), resp.body())

  test("browser API serves overview, chain, blocks, typed artifact view"):
    val root = Files.createTempDirectory("cairn-ui")
    val node = Node(root, EffectContext.forLedger())
    val alice = Keypair.dev("alice")
    val auth = Map(alice.name -> alice.publicBytes)
    val term = Artifact(ArtifactKind.Term, Cst.toCanon(Cst.Leaf("true")))
    casPut(node, term)
    node.append(alice, auth, List(
      alice.signTx(Tx.RegisterIdentity(alice.name, alice.publicBytes)),
      alice.signTx(Tx.PublishArtifact(term.key)),
      alice.signTx(Tx.SetBranchHead("main", term.key)))).fold(e => fail(e), identity)

    val langs = Map("stlc" -> cairn.examples.stlc.Stlc.language)
    val srv = BrowserServer(node, langs, 0)
    val port = srv.start()
    try
      val (hCode, health) = get(port, "/api/health")
      assertEquals(hCode, 200)
      assert(health.contains("\"ok\":true"), health)

      val (oCode, overview) = get(port, "/api/overview")
      assertEquals(oCode, 200)
      assert(overview.contains("\"chainLength\":1"), overview)

      val (bCode, blocks) = get(port, "/api/blocks")
      assertEquals(bCode, 200)
      assert(blocks.contains("\"height\":0"), blocks)

      val (aCode, art) = get(port, s"/api/artifacts/${term.digest.hex}")
      assertEquals(aCode, 200)
      assert(art.contains("\"kind\":\"term\""), art)
      assert(art.contains("\"viewers\""), art)

      val (vCode, view) = get(port, s"/api/artifacts/${term.digest.hex}/view?surface=json")
      assertEquals(vCode, 200)
      assert(view.contains("\"surface\":\"json\""), view)

      val (uiCode, html) = get(port, "/")
      assertEquals(uiCode, 200)
      assert(html.contains("Cairn Explorer"), html)

      val (cssCode, css) = get(port, "/ui/style.css")
      assertEquals(cssCode, 200)
      assert(css.contains("--bg"), css)

      val (jsCode, js) = get(port, "/ui/app.js")
      assertEquals(jsCode, 200)
      assert(js.contains("Cairn Explorer") || js.contains("loadOverview"), js)
    finally srv.stop()

  test("GET /api/schema/<lang>/<sort>: constructors + child sorts, derived from the composed language"):
    val root = Files.createTempDirectory("cairn-schema")
    val node = Node(root, EffectContext.forLedger())
    val langs = Map("stlc" -> cairn.examples.stlc.Stlc.language)
    val srv = BrowserServer(node, langs, 0)
    val port = srv.start()
    try
      val (code, body) = get(port, "/api/schema/stlc/Term")
      assertEquals(code, 200)
      assert(body.contains("\"name\":\"var\"") && body.contains("\"argSorts\":[\"Name\"]"), body)
      assert(body.contains("\"name\":\"app\"") && body.contains("\"argSorts\":[\"Term\",\"Term\"]"), body)
      assert(body.contains("\"name\":\"lam\"") && body.contains("\"argSorts\":[\"Name\",\"Type\",\"Term\"]"), body)
      val (missCode, missBody) = get(port, "/api/schema/nope/Term")
      assertEquals(missCode, 404)
      assert(missBody.contains("unknown language"), missBody)
    finally srv.stop()

  test("POST /api/parse validates editor buffer against a language"):
    val root = Files.createTempDirectory("cairn-ui-parse")
    val node = Node(root, EffectContext.forLedger())
    val langs = Map("stlc" -> cairn.examples.stlc.Stlc.language)
    val srv = BrowserServer(node, langs, 0)
    val port = srv.start()
    try
      val body = """{"lang":"stlc","text":"true"}"""
      val resp = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/api/parse"))
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(body)).build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(resp.statusCode(), 200)
      assert(resp.body().contains("\"ok\":true"), resp.body())
      assert(resp.body().contains("printed"), resp.body())
    finally srv.stop()

  test("GET /api/board returns Fact–Intent graph from IR module"):
    val root = Files.createTempDirectory("cairn-ui-board")
    val node = Node(root, EffectContext.forLedger())
    val board = Module(List(
      "origin" -> Cst.node("origin", Cst.Leaf("start")),
      "goal" -> Cst.node("goal", Cst.Leaf("done")),
      "i" -> Cst.node("intent", Cst.Leaf("work")),
      "f" -> Cst.node("fact", Cst.Leaf("found")),
      "e" -> Cst.node("supports", Cst.Leaf("i"), Cst.Leaf("f"))
    )).sorted
    casPut(node, board.artifact)
    val langs = Map("search" -> Search.language)
    val srv = BrowserServer(node, langs, 0)
    val port = srv.start()
    try
      val (code, body) = get(port, "/api/board")
      assertEquals(code, 200)
      assert(body.contains("\"viewer\":\"board\""), body)
      assert(body.contains("\"kind\":\"fact\""), body)
      assert(body.contains("\"kind\":\"supports\""), body)
      val (byDig, digBody) = get(port, s"/api/board?digest=${board.digest.hex}")
      assertEquals(byDig, 200)
      assert(digBody.contains(board.digest.hex), digBody)
    finally srv.stop()

  test("browser trust API: revoke + delegate publish CAS digests"):
    import cairn.systemhandler.{DelegationLog, RevocationLog}
    val root = Files.createTempDirectory("cairn-ui-trust")
    val node = Node(root, EffectContext.forLedger())
    val rev = RevocationLog()
    val del = DelegationLog()
    val srv = BrowserServer(node, Map.empty, 0, revocations = rev, delegations = del)
    val port = srv.start()
    try
      val (tCode, trust) = get(port, "/api/trust")
      assertEquals(tCode, 200)
      assert(trust.contains("revokedCount"), trust)
      assert(trust.contains("delegationCount"), trust)

      val revokeBody = """{"grantId":"grant-demo-1"}"""
      val revResp = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/api/trust/revoke"))
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(revokeBody)).build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(revResp.statusCode(), 200)
      assert(revResp.body().contains("grant-demo-1"), revResp.body())
      assert(rev.isRevoked("grant-demo-1"))

      val dlgBody =
        """{"grantor":"alice","grantee":"bob","action":"Cas.put","resourceKind":"cas","resourcePath":"*","depth":"1"}"""
      val dlgResp = client.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port/api/trust/delegate"))
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(dlgBody)).build(),
        HttpResponse.BodyHandlers.ofString())
      assertEquals(dlgResp.statusCode(), 200, dlgResp.body())
      assert(dlgResp.body().contains("alice"), dlgResp.body())
      assertEquals(del.snapshot.length, 1)

      val (rCode, revs) = get(port, "/api/trust/revocations")
      assertEquals(rCode, 200)
      assert(revs.contains("grant-demo-1"), revs)
      val (dCode, dels) = get(port, "/api/trust/delegations")
      assertEquals(dCode, 200)
      assert(dels.contains("\"grantee\":\"bob\""), dels)

      val (jsCode, js) = get(port, "/ui/app.js")
      assertEquals(jsCode, 200)
      assert(js.contains("loadTrust") || js.contains("trust/revocations"), js)
    finally srv.stop()
