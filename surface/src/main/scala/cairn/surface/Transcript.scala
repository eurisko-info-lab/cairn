package cairn.surface

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.systemhandler.{DiskCas, EffectContext}
import cairn.core.TreeEngine
import cairn.ledger.*
import cairn.runtime.PackLoader
import java.nio.file.{Files, Path}

/** Transcript DSL (S46, §2 Transcript, §7 "Transcripts are CI").
  *
  * The transcript language is itself defined with the generic grammar engine
  * (dogfooding): parse/print come from one GrammarSpec; the runner interprets
  * the parsed Cst against CAS, engines, and the ledger.
  *
  * Steps:
  *   lang NAME ;                       select a registered language pack
  *   roundtrip "SRC" ;                 parse SRC, assert print∘parse law
  *   eval "SRC" expect "SRC" ;         normalize and compare
  *   delta "ΔSRC" ;                    apply a ΔL change-set to the module
  *   claim NAME "SRC" expect "SRC" ;   test-certified claim over the module
  *   publish BRANCH ;                  publish language+module, set head
  *   fetch BRANCH ;                    second node pulls + verifies by hash
  */
object Transcript:
  val grammar: GrammarSpec = GrammarSpec(
    name = "cairn-transcript",
    tokens = TokenSpec(
      keywords = List("transcript", "lang", "roundtrip", "eval", "expect",
        "delta", "claim", "publish", "fetch", "node", "on", "from", "to",
        "gossip", "port", "expect-tests-pass", "query", "expectfail", "load-language"),
      puncts = List("{", "}", ";", ","),
      lineComment = Some("--"),
      identContExtra = "_'-"),
    categories = List(
      CategorySpec("transcript", List(
        ConstructorSpec("transcript", List(
          Elem.Tok("transcript"), Elem.NameLeaf, Elem.Tok("{"),
          Elem.Star(Elem.Cat("step")), Elem.Tok("}"))))),
      CategorySpec("step", List(
        ConstructorSpec("lang", List(Elem.Tok("lang"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("loadLang", List(Elem.Tok("load-language"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("nodeD", List(Elem.Tok("node"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("roundtrip", List(Elem.Tok("roundtrip"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("eval", List(Elem.Tok("eval"), Elem.StrLeaf, Elem.Tok("expect"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("delta", List(Elem.Tok("delta"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("claim", List(Elem.Tok("claim"), Elem.NameLeaf, Elem.StrLeaf, Elem.Tok("expect"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("publishOn", List(Elem.Tok("publish"), Elem.NameLeaf, Elem.Tok("on"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("publish", List(Elem.Tok("publish"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("fetchBetween", List(
          Elem.Tok("fetch"), Elem.NameLeaf, Elem.Tok("from"), Elem.NameLeaf, Elem.Tok("to"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("fetch", List(Elem.Tok("fetch"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("gossip", List(Elem.Tok("gossip"), Elem.SepBy1(Elem.NameLeaf, ","), Elem.Tok(";"))),
        ConstructorSpec("port", List(Elem.Tok("port"), Elem.NameLeaf, Elem.Tok("expect-tests-pass"), Elem.Tok(";"))),
        ConstructorSpec("query", List(Elem.Tok("query"), Elem.StrLeaf, Elem.Tok("expect"), Elem.NumLeaf, Elem.Tok(";"))),
        ConstructorSpec("expectfail", List(Elem.Tok("expectfail"), Elem.StrLeaf, Elem.Cat("step")))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("transcript", List(
        PrintSeg.Lit("transcript"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("{"), PrintSeg.Newline, PrintSeg.IndentIn,
        PrintSeg.SepFields(1, "\n"), PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
      PrintRule("lang", List(PrintSeg.Lit("lang"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("loadLang", List(PrintSeg.Lit("load-language"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("nodeD", List(PrintSeg.Lit("node"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("roundtrip", List(PrintSeg.Lit("roundtrip"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("eval", List(
        PrintSeg.Lit("eval"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space,
        PrintSeg.Lit("expect"), PrintSeg.Space, PrintSeg.StrField(1), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("delta", List(PrintSeg.Lit("delta"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("claim", List(
        PrintSeg.Lit("claim"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.StrField(1),
        PrintSeg.Space, PrintSeg.Lit("expect"), PrintSeg.Space, PrintSeg.StrField(2), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("publishOn", List(
        PrintSeg.Lit("publish"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("on"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("publish", List(PrintSeg.Lit("publish"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("fetchBetween", List(
        PrintSeg.Lit("fetch"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("from"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
        PrintSeg.Lit("to"), PrintSeg.Space, PrintSeg.Field(2), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("fetch", List(PrintSeg.Lit("fetch"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("gossip", List(PrintSeg.Lit("gossip"), PrintSeg.Space, PrintSeg.SepFields(0, ", "), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("port", List(
        PrintSeg.Lit("port"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("expect-tests-pass"), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("query", List(
        PrintSeg.Lit("query"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space,
        PrintSeg.Lit("expect"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("expectfail", List(
        PrintSeg.Lit("expectfail"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Field(1)))),
    top = "transcript")

  final case class Report(name: String, steps: List[String], workDir: Path, nodes: List[(String, Path)] = Nil):
    def render: String =
      val body = (s"transcript '$name':" :: steps.map("  ✓ " + _)).mkString("\n")
      val nodeLines =
        if nodes.isEmpty then Nil
        else
          "" :: "blockchain nodes:" :: nodes.sortBy(_._1).map { (n, p) =>
            val abs = p.toAbsolutePath.normalize
            val tip = if Files.exists(abs.resolve("chain")) then " (has chain)" else ""
            s"  $n = $abs$tip"
          }
      val browse =
        nodes.find(_._1 == "nodeA").orElse(nodes.headOption) match
          case Some((_, p)) =>
            val abs = p.toAbsolutePath.normalize
            List(
              "",
              s"explorer: sbt \"examples/runMain cairn.examples.Main ui $abs\"",
              s"         → http://127.0.0.1:8765  (serving $abs)")
          case None =>
            List("", s"workDir: ${workDir.toAbsolutePath.normalize}")
      (body :: nodeLines ++ browse).mkString("\n")

  /** Interpret a transcript against a working directory. `packs` is the
    * language registry (domain packs stay out of the surface layer — they are
    * injected by callers, §4.11). EffectContexts and packLoader are explicit — no
    * ambient AuthorityGate / PackAccess.
    */
  def run(
      src: String,
      packs: Map[String, ComposedLanguage],
      workDir: Path,
      portModules: Map[String, cairn.core.RosettaModule2] = Map.empty,
      packLoader: PackLoader,
      ledgerCtx: EffectContext,
      processCtx: EffectContext,
  ): Either[String, Report] =
    Parser.parse(grammar, src).flatMap {
      case Cst.Node("transcript", List(Cst.Leaf(name), Cst.Node("list", steps))) =>
        runSteps(name, steps, packs, workDir, portModules, packLoader, ledgerCtx, processCtx)
      case other => Left(s"not a transcript: ${other.render}")
    }

  private def runSteps(
      name: String,
      steps: List[Cst],
      packsIn: Map[String, ComposedLanguage],
      workDir: Path,
      portModules: Map[String, cairn.core.RosettaModule2],
      packLoader: PackLoader,
      ledgerCtx: EffectContext,
      processCtx: EffectContext,
  ): Either[String, Report] =
    var packs = packsIn
    var lang: Option[ComposedLanguage] = None
    var module = Module(Nil)
    val authority = Keypair.dev("dev-authority")
    def authorities = Map(authority.name -> authority.publicBytes)
    val log = List.newBuilder[String]
    val nodes = scala.collection.mutable.LinkedHashMap[String, Node]()
    def nodeOf(n: String): Node =
      nodes.getOrElseUpdate(n, {
        val path = workDir.resolve(n).toAbsolutePath.normalize
        Files.createDirectories(path)
        log += s"node $n at $path"
        Node(path, ledgerCtx)
      })

    def need: Either[String, ComposedLanguage] = lang.toRight("no language selected (use `lang NAME ;` first)")
    def parseIn(l: ComposedLanguage, s: String): Either[String, Cst] = Parser.parse(l.grammar, s)

    def publishTo(node: Node, branch: String): Either[String, Unit] =
      need.flatMap { l =>
        l.fragments.foreach(f => node.cas.put(f.artifact))
        node.cas.put(l.artifact)
        node.cas.put(module.artifact)
        val txs =
          List(authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes))) ++
          l.fragments.map(f => authority.signTx(Tx.PublishArtifact(f.artifact.key))) ++
          List(
            authority.signTx(Tx.PublishArtifact(l.artifact.key)),
            authority.signTx(Tx.PublishArtifact(module.artifact.key)),
            authority.signTx(Tx.SetBranchHead(branch, module.artifact.key)))
        node.append(authority, authorities, txs)
          .map { b =>
            log += s"published $branch at block ${b.digest.short} root ${b.stateRoot.short}"
            log += s"blockchain node: ${node.root.toAbsolutePath.normalize}"
          }
      }

    def fetchBetween(branch: String, fromN: String, toN: String): Either[String, Unit] =
      for
        _ <- Sync.pull(nodeOf(fromN), nodeOf(toN), authorities)
        st <- nodeOf(toN).state(authorities)
        head <- st.heads.get(branch).toRight(s"branch '$branch' not on ledger")
        art <- nodeOf(toN).cas.get(head)
      yield log += s"fetched $branch head ${art.digest.short} on $toN"

    def runStep(step: Cst): Either[String, Unit] =
      step match
        case Cst.Node("lang", List(Cst.Leaf(n))) =>
          packs.get(n).toRight(s"unknown language pack '$n' (registered: ${packs.keys.mkString(", ")})")
            .map { l => lang = Some(l); module = Module(Nil); log += s"lang $n (${l.digest.short})" }
        case Cst.Node("loadLang", List(Cst.Leaf(file))) =>
          val p = Path.of(file)
          val src = if Files.exists(p) then Right(Files.readString(p))
                    else Left(s"no such language file: $file")
          src.flatMap { text =>
            Meta.parseLanguageAst(text).flatMap { (name, fs) =>
              val dir = Option(p.toAbsolutePath.normalize.getParent).getOrElse(Path.of("."))
              val packsHere = packLoader.loadRaw(dir) + (name -> fs)
              val surfaces = packLoader.loadSurfaces(dir)
              packLoader.close(name, packsHere, surfaces).left.map(_.map(_.render).mkString("\n"))
            }
          }.map { l =>
            packs = packs + (l.name -> l)
            log += s"loaded language ${l.name} (${l.digest.short}) from $file" }
        case Cst.Node("nodeD", List(Cst.Leaf(n))) =>
          nodeOf(n); Right(())
        case Cst.Node("roundtrip", List(Cst.Leaf(s))) =>
          for
            l <- need
            t <- parseIn(l, s)
            _ <- RoundTrip.check(l.grammar, t)
          yield log += s"roundtrip ok: $s"
        case Cst.Node("eval", List(Cst.Leaf(src), Cst.Leaf(expected))) =>
          for
            l <- need
            t <- parseIn(l, src)
            e <- parseIn(l, expected)
            v <- TreeEngine.normalize(l, t)
            _ <- if v == e then Right(()) else Left(s"eval mismatch: $src ~> ${v.render}, expected ${e.render}")
          yield log += s"eval $src => $expected"
        case Cst.Node("delta", List(Cst.Leaf(src))) =>
          for
            l <- need
            dl <- Delta.deltaOf(l).left.map(_.map(_.render).mkString("; "))
            ch <- Parser.parse(dl.grammar, src)
            res <- Delta.apply(l, module, ch)
          yield
            module = res._1
            log += s"delta applied: module ${res._2.base.short} -> ${res._2.result.short}"
        case Cst.Node("claim", List(Cst.Leaf(cn), Cst.Leaf(input), Cst.Leaf(expected))) =>
          for
            l <- need
            i <- parseIn(l, input)
            e <- parseIn(l, expected)
            claim = cairn.proof.Claim(cn, Cst.node("claimEval", i, e), module.digest)
            suite = cairn.proof.TestSuite(s"$cn-tests", module.digest,
              List(cairn.proof.TestCase(cn, i, e)))
            cert <- cairn.proof.Certify.byTests(claim, suite, t => TreeEngine.normalize(l, t))
          yield log += s"claim $cn certified (${cert.artifact.digest.short})"
        case Cst.Node("publishOn", List(Cst.Leaf(branch), Cst.Leaf(nodeName))) =>
          publishTo(nodeOf(nodeName), branch)
        case Cst.Node("publish", List(Cst.Leaf(branch))) =>
          publishTo(nodeOf("nodeA"), branch)
        case Cst.Node("fetchBetween", List(Cst.Leaf(branch), Cst.Leaf(f), Cst.Leaf(t))) =>
          fetchBetween(branch, f, t)
        case Cst.Node("fetch", List(Cst.Leaf(branch))) =>
          fetchBetween(branch, "nodeA", "nodeB")
        case Cst.Node("gossip", List(Cst.Node("list", names))) =>
          val peers = names.collect { case Cst.Leaf(n) => Gossip.Peer(n, nodeOf(n)) }
          Gossip.converge(peers, authorities).map { reorgs =>
            log += s"gossip converged over ${peers.map(_.name).mkString(",")} (${reorgs.length} reorgs)" }
        case Cst.Node("port", List(Cst.Leaf(host))) =>
          portModules.values.headOption.toRight("no rosetta module registered for port steps").flatMap { m =>
            val port: Option[cairn.core.PortV2] = host match
              case "scala"   => Some(cairn.core.Ports2.ScalaPort2)
              case "lean"    => Some(cairn.core.Ports2.LeanPort2)
              case "haskell" => Some(cairn.core.Ports2.HaskellPort2)
              case "rust"    => Some(cairn.core.Ports2.RustPort2)
              case _          => None
            port.toRight(s"unknown port host '$host'").flatMap { p =>
              cairn.core.PortV2.verified(p, m).flatMap { out =>
                if host == "scala" then
                  val scalaCli = sys.env.getOrElse("PATH", "").split(":")
                    .map(java.nio.file.Paths.get(_, "scala-cli")).find(Files.isExecutable(_))
                  scalaCli match
                    case None => Right(log += s"port $host verified (host toolchain absent, fixpoint only)")
                    case Some(cli) =>
                      val f = Files.createTempDirectory(workDir, "port").resolve(out.fileName)
                      Files.writeString(f, out.text)
                      cairn.systemhandler.Process.run(
                        cairn.systeminterface.Process.Request.Run(
                          List(cli.toString, "run", "--server=false", f.toString)),
                        processCtx
                      ) match
                        case Right(r) if r.ok && r.combined.contains("ALL TESTS PASS") =>
                          Right(log += s"port $host tests pass in host")
                        case Right(r) => Left(s"port $host host run failed:\n${r.combined}")
                        case Left(e)  => Left(s"port $host host run failed: $e")
                else Right(log += s"port $host verified (byte fixpoint)")
              }
            }
          }.map(_ => ())
        case Cst.Node("query", List(Cst.Leaf(qsrc), Cst.Leaf(expected))) =>
          for
            q <- Query.parse(qsrc)
            res <- Query.run(q, module)
            _ <- if res.hits.length == expected.toInt then Right(())
                 else Left(s"query '$qsrc' returned ${res.hits.length} hits, expected $expected")
          yield log += s"query ok: $qsrc => ${res.hits.length}"
        case Cst.Node("expectfail", List(Cst.Leaf(substring), inner)) =>
          runStep(inner) match
            case Left(err) if err.contains(substring) =>
              log += s"expected failure: ...${substring}..."; Right(())
            case Left(err) => Left(s"failed with wrong message: $err (wanted ...$substring...)")
            case Right(_)  => Left(s"step succeeded but was expected to fail with ...$substring...")
        case other => Left(s"unknown transcript step: ${other.render}")

    val result = steps.foldLeft[Either[String, Unit]](Right(())) { (acc, step) =>
      acc.flatMap(_ => runStep(step)) }
    result.map(_ => Report(
      name,
      log.result(),
      workDir.toAbsolutePath.normalize,
      nodes.toList.map((n, node) => (n, node.root.toAbsolutePath.normalize))))

/** Generic CLI (S6, M40, M42, M43, M44): hash / put / get / canon over a disk
  * CAS, transcripts, provenance walking, capability manifests, REPL, LSP,
  * and the semantic-repository surface (`repo branches` / `repo demo`).
  * Language packs come from the caller AND from `.cairn` files (M42: adding a
  * language requires no recompilation).
  */
object Cli:
  def loadLanguages(dir: Path, packLoader: PackLoader): Map[String, ComposedLanguage] =
    packLoader.loadClosed(dir)

  def main(
      args: List[String],
      packsIn: Map[String, ComposedLanguage],
      portModules: Map[String, cairn.core.RosettaModule2] = Map.empty,
      packLoader: PackLoader,
      ledgerCtx: EffectContext,
      processCtx: EffectContext,
      lspCtx: EffectContext,
  ): Either[String, String] =
    /** Durable store root. Override with env `CAIRN_HOME`.
      * Default: `./.cas`. Each transcript writes a fresh run under
      * `$CAIRN_HOME/runs/<timestamp>/{nodeA,nodeB,…}` and prints those paths.
      * `ui` opens the latest run's `nodeA` (via `$CAIRN_HOME/LATEST`).
      */
    val home = Path.of(sys.env.getOrElse("CAIRN_HOME", ".cas")).toAbsolutePath.normalize
    val casDir = home
    def cas = DiskCas(casDir)
    def defaultUiRoot: Path =
      val latestFile = home.resolve("LATEST")
      val fromLatest =
        if Files.isRegularFile(latestFile) then
          val nodeA = Path.of(Files.readString(latestFile).trim).resolve("nodeA")
          if Files.exists(nodeA.resolve("chain")) then Some(nodeA) else None
        else None
      fromLatest.getOrElse {
        val nodeA = home.resolve("nodeA")
        if Files.exists(nodeA.resolve("chain")) then nodeA
        else if Files.exists(home.resolve("chain")) then home
        else nodeA
      }
    val packs = packsIn ++ packLoader.loadClosed() ++ loadLanguages(Path.of("languages"), packLoader)
    args match
      case List("home") =>
        val latest = Option.when(Files.isRegularFile(home.resolve("LATEST")))(
          Files.readString(home.resolve("LATEST")).trim)
        Right(
          s"""CAIRN_HOME=$home
             |env=${sys.env.getOrElse("CAIRN_HOME", "<unset> (default ./.cas)")}
             |latest-run=${latest.getOrElse("<none>")}
             |ui-default=$defaultUiRoot
             |ui-has-chain=${Files.exists(defaultUiRoot.resolve("chain"))}
             |""".stripMargin.trim)
      case List("hash", file) =>
        Right(Digest.ofBytes(Files.readAllBytes(Path.of(file))).hex)
      case List("put", file) =>
        val bs = Files.readAllBytes(Path.of(file))
        Right(cas.putBytes(bs).hex)
      case List("get", hex) =>
        Digest.parse(hex).flatMap(d => cas.getBytes(d)).map(bs => new String(bs, "UTF-8"))
      case List("canon", file) =>
        val bs = Files.readAllBytes(Path.of(file))
        Canon.decode(bs).map(c => Digest.of(c).hex)
      case List("transcript", file) =>
        val src = Files.readString(Path.of(file))
        val runId = java.time.LocalDateTime.now()
          .format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))
        val runDir = home.resolve("runs").resolve(runId).toAbsolutePath.normalize
        Files.createDirectories(runDir)
        Transcript.run(src, packs, runDir, portModules, packLoader, ledgerCtx, processCtx).map { r =>
          // Remember latest publisher for bare `ui`
          val latest = home.resolve("LATEST")
          Files.writeString(latest, runDir.toString + "\n")
          r.render
        }
      case List("why", hex) =>
        Digest.parse(hex).map { d =>
          val hops = cairn.ledger.Provenance.why(casDir, d)
          if hops.isEmpty then s"no provenance recorded for ${d.short}"
          else hops.map(h => s"${"  " * h.depth}${h.record.output.short} <- ${h.record.tool}(${h.record.inputs.map(_.short).mkString(", ")})").mkString("\n") }
      case List("capabilities", langName) =>
        packs.get(langName).toRight(s"unknown language '$langName'")
          .flatMap { l =>
            val surfDigests = packLoader.surfacesFor(langName).map((n, s) => n -> s.digest)
            Capabilities.build(l, Map.empty, surfDigests)
          }.map(_.render)
      case List("languages") =>
        Right(packs.toList.sortBy(_._1).map((n, l) => s"$n ${l.digest.hex}").mkString("\n"))
      case "repo" :: rest =>
        // Semantic repository surface: Branches + SemanticRepository spine.
        val refs = home.resolve("refs")
        val branches = cairn.systemhandler.Branches(cas, refs)
        rest match
          case List("branches") | Nil =>
            val names = branches.list()
            if names.isEmpty then Right("(no local branch refs under CAIRN_HOME/refs)")
            else Right(names.map { n =>
              val m = branches.load(n)
              s"$n head=${m.head.map(_.valueHash.short).getOrElse("-")} history=${m.history.length}"
            }.mkString("\n"))
          case List("demo") =>
            // Minimal e2e story so the spine is reachable outside tests.
            packs.get("stlc").toRight("stlc language pack required for repo demo").flatMap { lang =>
              Delta.deltaOf(lang).left.map(_.map(_.render).mkString("; ")).flatMap { d =>
                def parse(src: String) = Parser.parse(d.grammar, src)
                val m0 = Module(List("a" -> Cst.Node("true", Nil), "b" -> Cst.Node("false", Nil)))
                for
                  cA <- parse("{ replace a = false ; add fromA = true ; }")
                  cB <- parse("{ replace b = true ; add fromB = false ; }")
                  _ = branches.commitModule("demo-base", m0)
                  tipA <- SemanticRepository.tipAfter(lang, m0, cA)
                  tipB <- SemanticRepository.tipAfter(lang, m0, cB)
                  _ = branches.commitModule("demo-a", tipA.tip)
                  _ = branches.commitModule("demo-b", tipB.tip)
                  outcome <- branches.merge(lang, "demo-main", m0, cA, cB)
                yield outcome match
                  case Left(conflict) =>
                    s"conflict: ${conflict.render}"
                  case Right(manifest) =>
                    val defs = branches.headModule("demo-main")
                      .map(m => m.defs.map(_._1).sorted.mkString(","))
                      .getOrElse("?")
                    s"accepted demo-main head=${manifest.head.map(_.valueHash.short).getOrElse("-")} defs=$defs"
              }
            }
          case other =>
            Left(s"usage: cairn repo [branches|demo]  (got: repo ${other.mkString(" ")})")
      case List("repl", langName) =>
        packs.get(langName).toRight(s"unknown language '$langName'").map { l =>
          val repl = Repl(l)
          val in = scala.io.Source.stdin.getLines()
          val out = StringBuilder()
          var done = false
          while in.hasNext && !done do
            val line = in.next()
            if line.trim == ":quit" then done = true
            else out ++= repl.eval(line) + "\n"
          out.result() }
      case List("lsp", langName) =>
        packs.get(langName).toRight(s"unknown language '$langName'").map { l =>
          Lsp.serve(LspConfig(l), System.in, System.out, lspCtx); "lsp session ended" }
      case "ui" :: rest =>
        val portOpt = rest.lastOption.filter(s => s.forall(_.isDigit) && s.nonEmpty).map(_.toInt)
        val rootArg = rest match
          case Nil => None
          case ps if portOpt.isDefined && ps.length >= 2 => Some(Path.of(ps.dropRight(1).mkString("/")))
          case ps if portOpt.isDefined => None
          case ps => Some(Path.of(ps.mkString("/")))
        val root = rootArg.map(_.toAbsolutePath.normalize).getOrElse(defaultUiRoot)
        val port = portOpt.getOrElse(8765)
        Files.createDirectories(root)
        val bound = BrowserServer.serve(root, packs, ledgerCtx, port)
        System.out.println(s"Cairn Explorer at http://127.0.0.1:$bound")
        System.out.println(s"CAIRN_HOME=$home")
        System.out.println(s"serving root=$root")
        if !Files.exists(root.resolve("chain")) then
          System.out.println("NOTE: empty node (no chain file). Seed then reopen ui:")
          System.out.println("""  sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"""")
          System.out.println(s"  (writes $home/nodeA)")
        System.out.println("Press Enter to stop.")
        scala.io.StdIn.readLine()
        Right("ui stopped")
      case _ =>
        Left("usage: cairn [home|hash|put|get|canon|transcript|why|capabilities|languages|repo|repl|lsp|ui] <arg>")
