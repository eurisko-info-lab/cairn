package cairn.tests

import cairn.systemhandler.AuthorityGate
import cairn.kernel.*
import cairn.ledger.{Keypair, Node}
import cairn.surface.BrowserServer
import cairn.core.Module
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files

/** Browser API: navigate local node/CAS with typed artifact views. */
class BrowserSuite extends munit.FunSuite:
  private val client = HttpClient.newHttpClient()
  private val packs = cairn.runtime.PackLoader(AuthorityGate.bootstrapped())
  private val Search = cairn.examples.search.Search(packs)

  private def get(port: Int, path: String): (Int, String) =
    val resp = client.send(
      HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$path")).GET().build(),
      HttpResponse.BodyHandlers.ofString())
    (resp.statusCode(), resp.body())

  test("browser API serves overview, chain, blocks, typed artifact view"):
    val root = Files.createTempDirectory("cairn-ui")
    val node = Node(root, AuthorityGate.bootstrapped())
    val alice = Keypair.dev("alice")
    val auth = Map(alice.name -> alice.publicBytes)
    val term = Artifact(ArtifactKind.Term, Cst.toCanon(Cst.Leaf("true")))
    node.cas.put(term)
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

  test("POST /api/parse validates editor buffer against a language"):
    val root = Files.createTempDirectory("cairn-ui-parse")
    val node = Node(root, AuthorityGate.bootstrapped())
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
    val node = Node(root, AuthorityGate.bootstrapped())
    val board = Module(List(
      "origin" -> Cst.node("origin", Cst.Leaf("start")),
      "goal" -> Cst.node("goal", Cst.Leaf("done")),
      "i" -> Cst.node("intent", Cst.Leaf("work")),
      "f" -> Cst.node("fact", Cst.Leaf("found")),
      "e" -> Cst.node("supports", Cst.Leaf("i"), Cst.Leaf("f"))
    )).sorted
    node.cas.put(board.artifact)
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
