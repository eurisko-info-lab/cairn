package cairn.surface

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{CasAdminEffects, CasEffects, DelegationLog, EffectContext, Node, RevocationLog}
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

/** L6 browser API + static UI for navigating a local Node/CAS and opening
  * typed artifact viewers/editors. UI proposes only; kernel/CAS remain the
  * certifiers (§4.6 / CAIRN-PROMPT working style).
  *
 * Endpoints (JSON unless noted):
 *   GET  /api/health, /api/overview, /api/chain, /api/blocks
 *   GET  /api/blocks/{height|digest}
 *   GET  /api/artifacts/{digest}
 *   GET  /api/artifacts/{digest}/view?lang=&surface=text|json|canon
 *   GET  /api/board[?digest=] — Fact–Intent–Hint graph over an IR module
 *   GET  /api/languages, /api/cas/stats
 *   GET  /api/trust — revocation + delegation overview
 *   GET  /api/trust/revocations, POST /api/trust/revoke
 *   GET  /api/trust/delegations, POST /api/trust/delegate
 *   POST /api/parse  body JSON lang+text — validate editor buffer
 *   GET  / and /ui/... — static UI assets
 */
final class BrowserServer(
    node: Node,
    languages: Map[String, ComposedLanguage],
    port: Int = 0,
    fsCtx: EffectContext = EffectContext.forFilesystem(),
    revocations: RevocationLog = RevocationLog(),
    delegations: DelegationLog = DelegationLog(),
):
  private var server: HttpServer | Null = null
  private val uiRoot = "cairn/ui"

  def start(): Int =
    val s = HttpServer.create(InetSocketAddress(port), 0)
    // Longest-prefix wins: register /api and /ui before /
    s.createContext("/api", ex => handleApi(ex))
    s.createContext("/ui", ex => handleUi(ex))
    s.createContext("/", ex => handleIndex(ex))
    s.setExecutor(java.util.concurrent.Executors.newCachedThreadPool())
    s.start()
    server = s
    s.getAddress.getPort

  def stop(): Unit = Option(server).foreach(_.stop(0))

  private def openUi(name: String): Option[Array[Byte]] =
    val resource = s"$uiRoot/$name"
    val loaders = List(
      Thread.currentThread.getContextClassLoader,
      getClass.getClassLoader,
      classOf[BrowserServer].getClassLoader)
    val fromCp = loaders.view
      .filter(_ != null)
      .flatMap(cl => Option(cl.getResourceAsStream(resource)))
      .headOption
      .map { in => try in.readAllBytes() finally in.close() }
    fromCp.orElse {
      val candidates = List(
        Path.of("surface/src/main/resources", resource),
        Path.of("src/main/resources", resource),
        Path.of("../surface/src/main/resources", resource))
      candidates.view
        .flatMap { p =>
          Transcript.fsIsRegularFile(p, fsCtx).toOption.filter(identity).flatMap { _ =>
            Transcript.fsReadBytes(p, fsCtx).toOption
          }
        }
        .headOption
    }

  private def reply(ex: HttpExchange, code: Int, body: Array[Byte], ctype: String): Unit =
    ex.getResponseHeaders.set("Content-Type", ctype)
    ex.getResponseHeaders.set("Access-Control-Allow-Origin", "*")
    ex.getResponseHeaders.set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    ex.getResponseHeaders.set("Access-Control-Allow-Headers", "Content-Type")
    if ex.getRequestMethod == "OPTIONS" then
      ex.sendResponseHeaders(204, -1)
      ex.close()
    else
      ex.sendResponseHeaders(code, body.length.toLong)
      val out = ex.getResponseBody
      if body.nonEmpty then out.write(body)
      out.close()
      ex.close()

  private def json(ex: HttpExchange, code: Int, body: String): Unit =
    reply(ex, code, body.getBytes(UTF_8), "application/json; charset=utf-8")

  private def html(ex: HttpExchange, code: Int, body: String): Unit =
    reply(ex, code, body.getBytes(UTF_8), "text/html; charset=utf-8")

  private def err(ex: HttpExchange, code: Int, msg: String): Unit =
    json(ex, code, Json.obj("error" -> Json.str(msg)))

  private def handleIndex(ex: HttpExchange): Unit =
    if ex.getRequestMethod == "OPTIONS" then
      reply(ex, 204, Array.emptyByteArray, "text/plain"); return
    val path = ex.getRequestURI.getPath
    // Only serve the SPA shell from / and /index.html; anything else under /
    // that isn't /api or /ui should still get the shell (client-side routing).
    if path.startsWith("/api") || path.startsWith("/ui") then
      err(ex, 404, s"unhandled path $path"); return
    openUi("index.html") match
      case Some(bytes) => reply(ex, 200, bytes, "text/html; charset=utf-8")
      case None =>
        html(ex, 500,
          """<!doctype html><meta charset=utf-8><title>Cairn Explorer</title>
            |<h1>Cairn Explorer</h1>
            |<p>UI assets missing from classpath (<code>cairn/ui/index.html</code>).
            |Rebuild with <code>sbt surface/compile</code> or run from the repo root.</p>
            |<p>API check: <a href="/api/health">/api/health</a></p>""".stripMargin)

  private def handleUi(ex: HttpExchange): Unit =
    if ex.getRequestMethod == "OPTIONS" then
      reply(ex, 204, Array.emptyByteArray, "text/plain"); return
    val name = ex.getRequestURI.getPath.stripPrefix("/ui").stripPrefix("/")
    if name.isEmpty || name.contains("..") then
      err(ex, 404, "missing ui asset"); return
    openUi(name) match
      case None => err(ex, 404, s"ui asset not found: $name")
      case Some(bytes) =>
        val ctype =
          if name.endsWith(".html") then "text/html; charset=utf-8"
          else if name.endsWith(".js") then "application/javascript; charset=utf-8"
          else if name.endsWith(".css") then "text/css; charset=utf-8"
          else if name.endsWith(".svg") then "image/svg+xml"
          else "application/octet-stream"
        reply(ex, 200, bytes, ctype)

  private def handleApi(ex: HttpExchange): Unit =
    if ex.getRequestMethod == "OPTIONS" then
      reply(ex, 204, Array.emptyByteArray, "text/plain"); return
    val path = ex.getRequestURI.getPath.stripPrefix("/api").stripPrefix("/")
    val q = Option(ex.getRequestURI.getQuery).getOrElse("")
    val params = q.split('&').filter(_.nonEmpty).map { p =>
      val i = p.indexOf('=')
      if i < 0 then p -> ""
      else java.net.URLDecoder.decode(p.substring(0, i), UTF_8) ->
        java.net.URLDecoder.decode(p.substring(i + 1), UTF_8)
    }.toMap
    try
      (ex.getRequestMethod, path) match
        case ("GET", "health") =>
          json(ex, 200, Json.obj(
            "ok" -> Json.bool(true),
            "root" -> Json.str(node.root.toAbsolutePath.toString)))
        case ("GET", "overview") =>
          json(ex, 200, overviewJson)
        case ("GET", "chain") =>
          json(ex, 200, Json.arr(node.chainDigests.map(d => Json.str(d.hex))))
        case ("GET", "blocks") =>
          node.blocks match
            case Left(e)  => err(ex, 500, e)
            case Right(bs) => json(ex, 200, Json.arr(bs.map(blockSummary)))
        case ("GET", p) if p.startsWith("blocks/") =>
          val id = p.stripPrefix("blocks/")
          blockDetail(id) match
            case Left(e)  => err(ex, 404, e)
            case Right(j) => json(ex, 200, j)
        case ("GET", p) if p.startsWith("artifacts/") && p.endsWith("/view") =>
          val dig = p.stripPrefix("artifacts/").stripSuffix("/view")
          artifactView(dig, params.get("lang"), params.getOrElse("surface", "text")) match
            case Left(e)  => err(ex, 404, e)
            case Right(j) => json(ex, 200, j)
        case ("GET", p) if p.startsWith("artifacts/") =>
          val dig = p.stripPrefix("artifacts/")
          artifactJson(dig) match
            case Left(e)  => err(ex, 404, e)
            case Right(j) => json(ex, 200, j)
        case ("GET", "languages") =>
          json(ex, 200, Json.arr(languages.toList.sortBy(_._1).map { (n, l) =>
            Json.obj(
              "name" -> Json.str(n),
              "digest" -> Json.str(l.digest.hex),
              "top" -> Json.str(l.grammar.top),
              "ctors" -> Json.arr(l.constructors.keys.toList.sorted.map(Json.str)))
          }))
        case ("GET", p) if p.startsWith("schema/") =>
          // Projectional-editing proof-of-concept: for <lang>/<sort>, every
          // valid constructor + positional child sorts, derived directly from
          // the composed language's own Fragment data (no per-language code) —
          // same schema the LSP's cairn/schema request exposes.
          p.stripPrefix("schema/").split("/", 2) match
            case Array(lang, sort) =>
              languages.get(lang) match
                case None => err(ex, 404, s"unknown language '$lang'")
                case Some(l) =>
                  val ctors = l.constructors.values.filter(_.sort == sort).toList.sortBy(_.name)
                  json(ex, 200, Json.arr(ctors.map(c => Json.obj(
                    "name" -> Json.str(c.name),
                    "argSorts" -> Json.arr(c.argSorts.map(Json.str))))))
            case _ => err(ex, 404, "expected /api/schema/<lang>/<sort>")
        case ("GET", "board") =>
          boardJson(params.get("digest")) match
            case Left(e)  => err(ex, 404, e)
            case Right(j) => json(ex, 200, j)
        case ("GET", "cas/stats") =>
          CasAdminEffects.stats(node.root, node.ctx) match
            case Left(e) => err(ex, 403, e.toString)
            case Right(st) =>
              json(ex, 200, Json.obj(
                "objects" -> Json.num(st.objects.toLong),
                "bytes" -> Json.num(st.bytes),
                "byKind" -> Json.obj(st.byKind.toList.sortBy(_._1).map((k, n) => k -> Json.num(n.toLong))*)))
        case ("GET", "trust") =>
          json(ex, 200, trustOverviewJson)
        case ("GET", "trust/revocations") =>
          json(ex, 200, revocationsJson)
        case ("POST", "trust/revoke") =>
          val body = new String(ex.getRequestBody.readAllBytes(), UTF_8)
          trustRevoke(body) match
            case Left(e)  => err(ex, 400, e)
            case Right(j) => json(ex, 200, j)
        case ("GET", "trust/delegations") =>
          json(ex, 200, delegationsJson)
        case ("POST", "trust/delegate") =>
          val body = new String(ex.getRequestBody.readAllBytes(), UTF_8)
          trustDelegate(body) match
            case Left(e)  => err(ex, 400, e)
            case Right(j) => json(ex, 200, j)
        case ("POST", "parse") =>
          val body = new String(ex.getRequestBody.readAllBytes(), UTF_8)
          parseEditor(body) match
            case Left(e)  => err(ex, 400, e)
            case Right(j) => json(ex, 200, j)
        case _ => err(ex, 404, s"unknown api path /$path")
    catch case e: Exception =>
      err(ex, 500, Option(e.getMessage).getOrElse(e.toString))

  private def overviewJson: String =
    val chain = node.chainDigests
    val stats = CasAdminEffects.stats(node.root, node.ctx).toOption
    val last = node.blocks.toOption.flatMap(_.lastOption)
    Json.obj(
      "root" -> Json.str(node.root.toAbsolutePath.toString),
      "chainLength" -> Json.num(chain.length.toLong),
      "headDigest" -> chain.lastOption.fold(Json.nul)(d => Json.str(d.hex)),
      "lastHeight" -> last.fold(Json.nul)(b => Json.num(b.height)),
      "casObjects" -> stats.fold(Json.nul)(s => Json.num(s.objects.toLong)),
      "casBytes" -> stats.fold(Json.nul)(s => Json.num(s.bytes)),
      "languages" -> Json.arr(languages.keys.toList.sorted.map(Json.str)),
      "byKind" -> stats.fold(Json.obj())(s =>
        Json.obj(s.byKind.toList.sortBy(_._1).map((k, n) => k -> Json.num(n.toLong))*)))

  private def blockSummary(b: Block): String =
    Json.obj(
      "height" -> Json.num(b.height),
      "digest" -> Json.str(b.digest.hex),
      "parent" -> Json.str(b.parent.hex),
      "authority" -> Json.str(b.authority),
      "stateRoot" -> Json.str(b.stateRoot.hex),
      "txCount" -> Json.num(b.txs.length.toLong))

  private def txJson(stx: SignedTx): String =
    val tx = stx.tx
    val payload = tx match
      case Tx.RegisterIdentity(n, pk) =>
        Json.obj("name" -> Json.str(n), "publicKey" -> Json.str(pk.map(b => f"${b & 0xff}%02x").mkString))
      case Tx.PublishArtifact(k) =>
        Json.obj("key" -> Json.str(k.render), "kind" -> Json.str(k.kind.name),
          "valueHash" -> Json.str(k.valueHash.hex), "typeHash" -> Json.str(k.typeHash.hex))
      case Tx.SetBranchHead(br, h, c) =>
        Json.obj("branch" -> Json.str(br), "head" -> Json.str(h.render),
          "kind" -> Json.str(h.kind.name), "valueHash" -> Json.str(h.valueHash.hex),
          "certRef" -> c.fold(Json.nul)(d => Json.str(d.hex)))
      case Tx.RecordCertificate(c, m) =>
        Json.obj("cert" -> Json.str(c.hex), "method" -> Json.str(m))
      case Tx.AddAuthority(n, _, _) => Json.obj("name" -> Json.str(n))
      case Tx.RemoveAuthority(n, _) => Json.obj("name" -> Json.str(n))
      case Tx.SetPolicy(br, p) =>
        Json.obj("branch" -> Json.str(br), "policy" -> Json.ofCst(p))
    Json.obj(
      "tag" -> Json.str(tx match
        case _: Tx.RegisterIdentity  => "RegisterIdentity"
        case _: Tx.PublishArtifact   => "PublishArtifact"
        case _: Tx.SetBranchHead     => "SetBranchHead"
        case _: Tx.RecordCertificate => "RecordCertificate"
        case _: Tx.AddAuthority      => "AddAuthority"
        case _: Tx.RemoveAuthority   => "RemoveAuthority"
        case _: Tx.SetPolicy         => "SetPolicy"),
      "signer" -> Json.str(stx.signer),
      "digest" -> Json.str(Tx.digest(tx).hex),
      "payload" -> payload,
      "canon" -> Json.ofCanon(Tx.toCanon(tx)))

  private def blockDetail(id: String): Either[String, String] =
    node.blocks.flatMap { bs =>
      val byHeight = id.toLongOption.flatMap(h => bs.find(_.height == h))
      val byDig = Digest.parse(id).toOption.flatMap(d => bs.find(_.digest == d))
      (byHeight orElse byDig).toRight(s"block not found: $id").map { b =>
        Json.obj(
          "height" -> Json.num(b.height),
          "digest" -> Json.str(b.digest.hex),
          "parent" -> Json.str(b.parent.hex),
          "authority" -> Json.str(b.authority),
          "stateRoot" -> Json.str(b.stateRoot.hex),
          "txs" -> Json.arr(b.txs.map(txJson)),
          "canon" -> Json.ofCanon(b.artifact.body),
          "viewer" -> Json.str("block"))
      }
    }

  private def loadArtifact(hex: String): Either[String, Artifact] =
    Digest.parse(hex).flatMap { d =>
      CasEffects.get(node.cas, d, node.ctx).left.map {
        case cairn.systeminterface.Cas.Error.Missing(_) => s"blob ${d.short} not in CAS"
        case cairn.systeminterface.Cas.Error.Io(m)      => m
      }
    }

  private def artifactJson(hex: String): Either[String, String] =
    loadArtifact(hex).map { a =>
      val kindViews = suggestedViewers(a.kind)
      Json.obj(
        "digest" -> Json.str(a.digest.hex),
        "key" -> Json.str(a.key.render),
        "kind" -> Json.str(a.kind.name),
        "typeHash" -> Json.str(a.key.typeHash.hex),
        "canon" -> Json.ofCanon(a.body),
        "viewers" -> Json.arr(kindViews.map(Json.str)),
        "decoded" -> decodeTyped(a))
    }

  private def suggestedViewers(k: ArtifactKind): List[String] = k match
    case ArtifactKind.Block            => List("block", "canon", "json")
    case ArtifactKind.Transaction      => List("tx", "canon", "json")
    case ArtifactKind.Language         => List("language", "text", "json", "canon")
    case ArtifactKind.Fragment         => List("fragment", "text", "json", "canon")
    case ArtifactKind.Ir               => List("module", "text", "json", "canon")
    case ArtifactKind.Source           => List("text", "json", "canon")
    case ArtifactKind.ChangeSet        => List("changeset", "text", "json", "canon")
    case ArtifactKind.BranchManifest   => List("branch", "json", "canon")
    case ArtifactKind.Certificate      => List("certificate", "json", "canon")
    case ArtifactKind.Policy           => List("policy", "text", "json", "canon")
    case _                             => List("json", "canon")

  private def decodeTyped(a: Artifact): String =
    try a.kind match
      case ArtifactKind.Block =>
        val b = Block.fromCanon(a.body)
        Json.obj("viewer" -> Json.str("block"), "summary" -> blockSummary(b))
      case ArtifactKind.Ir =>
        val m = Module.fromCanon(a.body)
        Json.obj(
          "viewer" -> Json.str("module"),
          "defs" -> Json.arr(m.defs.map((n, t) =>
            Json.obj("name" -> Json.str(n), "term" -> Json.ofCst(t),
              "text" -> Json.str(t.render)))))
      case ArtifactKind.Language =>
        Json.obj("viewer" -> Json.str("language"), "digest" -> Json.str(a.digest.hex))
      case ArtifactKind.Fragment =>
        val f = FragmentCodec.fromCanon(a.body)
        Json.obj(
          "viewer" -> Json.str("fragment"),
          "name" -> Json.str(f.name),
          "provides" -> Json.arr(f.provides.map(Json.str)),
          "requires" -> Json.arr(f.requires.map(Json.str)),
          "ctors" -> Json.arr(f.constructors.map(c => Json.str(c.name))))
      case ArtifactKind.BranchManifest =>
        Json.obj("viewer" -> Json.str("branch"), "canon" -> Json.ofCanon(a.body))
      case _ =>
        Json.obj("viewer" -> Json.str("canon"), "body" -> Json.ofCanon(a.body))
    catch case e: Exception =>
      Json.obj("viewer" -> Json.str("canon"), "error" -> Json.str(e.getMessage),
        "body" -> Json.ofCanon(a.body))

  private def artifactView(hex: String, langName: Option[String], surface: String): Either[String, String] =
    for
      a <- loadArtifact(hex)
      view <- renderSurface(a, langName, surface)
    yield Json.obj(
      "digest" -> Json.str(a.digest.hex),
      "kind" -> Json.str(a.kind.name),
      "lang" -> langName.fold(Json.nul)(Json.str),
      "surface" -> Json.str(surface),
      "editable" -> Json.bool(surface == "text" && langName.isDefined),
      "text" -> Json.str(view))

  private def renderSurface(a: Artifact, langName: Option[String], surface: String): Either[String, String] =
    surface match
      case "canon" => Right(a.body.toString)
      case "json"  => Right(Json.ofCanon(a.body))
      case "text" =>
        def printWith(lang: ComposedLanguage): Either[String, String] = a.kind match
          case ArtifactKind.Ir =>
            val m = Module.fromCanon(a.body)
            Right(m.defs.map { (name, term) =>
              Printer.print(lang.grammar, term) match
                case Right(t) => s"$name = $t ;"
                case Left(_)  => s"$name = ${term.render} ;"
            }.mkString("\n"))
          case ArtifactKind.Fragment =>
            val f = FragmentCodec.fromCanon(a.body)
            Meta.printLanguage(f.name, List(f))
          case ArtifactKind.Language =>
            languages.find(_._2.digest == a.digest) match
              case Some((name, l)) => Meta.printLanguage(name, l.fragments)
              case None => Meta.printLanguage(lang.name, lang.fragments)
          case ArtifactKind.Source | ArtifactKind.Term | ArtifactKind.Policy =>
            try Printer.print(lang.grammar, Cst.fromCanon(a.body))
            catch case e: Exception => Left(e.getMessage)
          case ArtifactKind.ChangeSet =>
            Right(Json.ofCanon(a.body))
          case _ =>
            Right(Json.ofCanon(a.body))

        langName match
          case Some(n) =>
            languages.get(n).toRight(s"unknown language '$n'").flatMap(printWith)
          case None =>
            a.kind match
              case ArtifactKind.Ir =>
                val m = Module.fromCanon(a.body)
                Right(m.defs.map((n, t) => s"$n = ${t.render} ;").mkString("\n"))
              case ArtifactKind.Fragment =>
                val f = FragmentCodec.fromCanon(a.body)
                Meta.printLanguage(f.name, List(f))
              case ArtifactKind.Language =>
                languages.find(_._2.digest == a.digest) match
                  case Some((name, l)) => Meta.printLanguage(name, l.fragments)
                  case None => Right(s"-- language ${a.digest.hex}\n${Json.ofCanon(a.body)}")
              case _ => Right(Json.ofCanon(a.body))
      case other => Left(s"unknown surface '$other' (use text|json|canon)")

  private def parseEditor(raw: String): Either[String, String] =
    // minimal JSON field extract: "lang":"...","text":"..."
    def field(name: String): Either[String, String] =
      jsonField(raw, name)
    for
      langName <- field("lang")
      text <- field("text")
      lang <- languages.get(langName).toRight(s"unknown language '$langName'")
      cst <- Parser.parse(lang.grammar, text).left.map(e => s"parse error: $e")
      printed <- Printer.print(lang.grammar, cst)
    yield Json.obj(
      "ok" -> Json.bool(true),
      "lang" -> Json.str(langName),
      "cst" -> Json.ofCst(cst),
      "printed" -> Json.str(printed))

  private def jsonField(raw: String, name: String): Either[String, String] =
    val key = s"\"$name\""
    val i = raw.indexOf(key)
    if i < 0 then Left(s"missing field $name")
    else
      val colon = raw.indexOf(':', i + key.length)
      val q1 = raw.indexOf('"', colon + 1)
      if q1 < 0 then Left(s"bad field $name")
      else
        val b = StringBuilder()
        var j = q1 + 1
        var ok = true
        while ok && j < raw.length do
          raw.charAt(j) match
            case '"'  => ok = false
            case '\\' if j + 1 < raw.length =>
              raw.charAt(j + 1) match
                case 'n' => b += '\n'; j += 2
                case 't' => b += '\t'; j += 2
                case 'r' => b += '\r'; j += 2
                case '"' => b += '"'; j += 2
                case '\\' => b += '\\'; j += 2
                case c => b += c; j += 2
            case c => b += c; j += 1
        Right(b.result())

  private def jsonFieldOpt(raw: String, name: String): Option[String] =
    jsonField(raw, name).toOption.filter(_.nonEmpty)

  private def trustOverviewJson: String =
    Json.obj(
      "revokedCount" -> Json.num(revocations.snapshot.size.toLong),
      "revocationDigests" -> Json.arr(revocations.digests.toList.sortBy(_.hex).map(d => Json.str(d.hex))),
      "delegationCount" -> Json.num(delegations.snapshot.size.toLong),
      "delegationDigests" -> Json.arr(delegations.digests.toList.sortBy(_.hex).map(d => Json.str(d.hex))),
      "note" -> Json.str(
        "Explorer surfaces over ReplayReplication / RevocationLog / DelegationLog — digest-merge, not BFT; no Studio."))

  private def revocationsJson: String =
    Json.obj(
      "grants" -> Json.arr(revocations.snapshot.toList.sorted.map(Json.str)),
      "digests" -> Json.arr(revocations.digests.toList.sortBy(_.hex).map(d => Json.str(d.hex))))

  private def delegationsJson: String =
    Json.obj(
      "entries" -> Json.arr(delegations.snapshot.map { e =>
        Json.obj(
          "grantor" -> Json.str(e.grantor),
          "grantee" -> Json.str(e.grantee),
          "action" -> Json.str(e.action),
          "resourceKind" -> Json.str(e.resourceKind),
          "resourcePath" -> Json.str(e.resourcePath),
          "depth" -> Json.num(e.depth.toLong),
          "digest" -> e.digest.fold(Json.nul)(d => Json.str(d.hex)))
      }))

  private def trustRevoke(raw: String): Either[String, String] =
    for
      grantId <- jsonField(raw, "grantId")
      dig <- revocations.publish(node.cas, node.ctx, List(grantId))
    yield Json.obj(
      "ok" -> Json.bool(true),
      "grantId" -> Json.str(grantId),
      "digest" -> Json.str(dig.hex),
      "revokedCount" -> Json.num(revocations.snapshot.size.toLong))

  private def trustDelegate(raw: String): Either[String, String] =
    for
      grantor <- jsonField(raw, "grantor")
      grantee <- jsonField(raw, "grantee")
      action <- jsonField(raw, "action")
      _ <- Either.cond(grantor.nonEmpty && grantee.nonEmpty, (), "grantor/grantee required")
      depth = jsonFieldOpt(raw, "depth").flatMap(_.toIntOption).getOrElse(1)
      _ <- Either.cond(depth >= 1, (), "depth must be >= 1")
      entry = DelegationLog.Entry(
        grantor = grantor,
        grantee = grantee,
        action = action,
        resourceKind = jsonFieldOpt(raw, "resourceKind").getOrElse("cas"),
        resourcePath = jsonFieldOpt(raw, "resourcePath").getOrElse("*"),
        depth = depth)
      dig <- delegations.publish(node.cas, node.ctx, entry)
    yield Json.obj(
      "ok" -> Json.bool(true),
      "digest" -> Json.str(dig.hex),
      "entry" -> Json.obj(
        "grantor" -> Json.str(entry.grantor),
        "grantee" -> Json.str(entry.grantee),
        "action" -> Json.str(entry.action),
        "resourceKind" -> Json.str(entry.resourceKind),
        "resourcePath" -> Json.str(entry.resourcePath),
        "depth" -> Json.num(entry.depth.toLong),
        "digest" -> Json.str(dig.hex)))

  /** Read-only Fact–Intent–Hint board graph from an IR module digest.
    * Looks up `digest` when given; otherwise uses the latest published IR
    * artifact that contains search-shaped constructors.
    */
  private def boardJson(digestOpt: Option[String]): Either[String, String] =
    def graphFromModule(m: Module, dig: Digest): String =
      val nodes = List.newBuilder[String]
      val edges = List.newBuilder[String]
      for (name, term) <- m.defs do term match
        case Cst.Node(k @ ("origin" | "fact" | "goal" | "intent" | "hint"), List(Cst.Leaf(t))) =>
          nodes += Json.obj(
            "name" -> Json.str(name), "kind" -> Json.str(k), "text" -> Json.str(t))
        case Cst.Node(k @ ("supports" | "spawns"), List(Cst.Leaf(a), Cst.Leaf(b))) =>
          edges += Json.obj(
            "name" -> Json.str(name), "kind" -> Json.str(k),
            "from" -> Json.str(a), "to" -> Json.str(b))
        case Cst.Node("board", List(Cst.Node("list", ns))) =>
          nodes += Json.obj(
            "name" -> Json.str(name), "kind" -> Json.str("board"),
            "text" -> Json.str(ns.collect { case Cst.Leaf(n) => n }.mkString(", ")))
        case _ => ()
      Json.obj(
        "digest" -> Json.str(dig.hex),
        "viewer" -> Json.str("board"),
        "nodes" -> Json.arr(nodes.result()),
        "edges" -> Json.arr(edges.result()))

    def isSearchShaped(m: Module): Boolean =
      m.defs.exists((_, t) => t match
        case Cst.Node(k, _) if Set("origin", "goal", "fact", "intent", "hint", "supports", "spawns").contains(k) =>
          true
        case _ => false)

    digestOpt match
      case Some(hex) =>
        for
          a <- loadArtifact(hex)
          _ <- Either.cond(a.kind == ArtifactKind.Ir, (), s"artifact $hex is ${a.kind.name}, want ir")
          m = Module.fromCanon(a.body)
        yield graphFromModule(m, a.digest)
      case None =>
        def casErr(e: cairn.systeminterface.Cas.Error): String = e match
          case cairn.systeminterface.Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
          case cairn.systeminterface.Cas.Error.Io(m)      => m
        CasAdminEffects.artifacts(node.root, node.ctx).left.map(casErr).flatMap { arts =>
          if arts.isEmpty then Left("CAS empty — publish a search board first")
          else
            val found = arts.iterator
              .filter(_.kind == ArtifactKind.Ir)
              .map(a => a -> Module.fromCanon(a.body))
              .find((_, m) => isSearchShaped(m))
            found match
              case Some((a, m)) => Right(graphFromModule(m, a.digest))
              case None => Left("no search-shaped board module in CAS — run transcripts/search-board.cairn")
        }

object BrowserServer:
  /** Serve UI for a local node/CAS root until interrupted.
    * Root directory creation is authorized via [[fsCtx]] ([[EffectContext.forFilesystem]]).
    */
  def serve(
      root: Path,
      languages: Map[String, ComposedLanguage],
      ledgerCtx: cairn.systemhandler.EffectContext,
      fsCtx: cairn.systemhandler.EffectContext,
      port: Int = 8765
  ): Either[String, Int] =
    Transcript.fsMkdirs(root, fsCtx).map { _ =>
      val node = Node(root, ledgerCtx)
      val srv = BrowserServer(node, languages, port, fsCtx)
      srv.start()
    }
