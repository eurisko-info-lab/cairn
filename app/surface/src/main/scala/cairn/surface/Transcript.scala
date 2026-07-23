package cairn.surface

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{
  BftCeremony, BftFinality, BftReplica, CasEffects, DiskCas, EffectContext, Filesystem, Gossip,
  GossipDaemon, HttpGossip, HttpNode, HttpSync, Keypair, Keystore, Node, PeerRegistry, Provenance,
  Sync}
import cairn.systeminterface.Filesystem as Fs
import cairn.core.TreeEngine
import cairn.runtime.PackLoader
import java.nio.file.Path

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
  *   deferred "REASON" ;               honest coverage stub (no Cairn equivalent yet)
  *   porcelain THEME ;                 run Plumbing.charbTheme (promoted Charb name)
  *   fork-from CHILD trunk ;           plant domain branch on ledger trunk
  *   fork-from CHILD of PARENT ;       plant domain branch under primary
  *   refer CHILD to OTHER ;            soft domain reference
  */
object Transcript:
  /** Typed step result — success / expected failure / deferral are not
    * string-prefix conventions that CI can accidentally game.
    */
  enum StepOutcome:
    case Executed(evidence: Canon)
    case ExpectedFailure(error: Canon)
    case Deferred(reason: String, sourceCapability: String)

    def summary: String = this match
      case Executed(Canon.CStr(s)) => s
      case Executed(Canon.CTag("porcelain", Canon.CStr(theme))) => s"porcelain $theme"
      case Executed(other) => other.toString
      case ExpectedFailure(Canon.CStr(s)) => s"expected failure: ...$s..."
      case ExpectedFailure(e) => s"expected failure: $e"
      case Deferred(reason, _) => s"deferred: $reason"

  val grammar: GrammarSpec = GrammarSpec(
    name = "cairn-transcript",
    tokens = TokenSpec(
      keywords = List("transcript", "lang", "roundtrip", "eval", "expect",
        "delta", "claim", "publish", "fetch", "node", "on", "from", "to",
        "gossip", "port", "expect-tests-pass", "query", "expectfail", "load-language",
        "deferred", "porcelain", "fork-from", "trunk", "of", "refer"),
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
        ConstructorSpec("deferred", List(Elem.Tok("deferred"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("porcelain", List(Elem.Tok("porcelain"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("forkTrunk", List(
          Elem.Tok("fork-from"), Elem.NameLeaf, Elem.Tok("trunk"), Elem.Tok(";"))),
        ConstructorSpec("forkOf", List(
          Elem.Tok("fork-from"), Elem.NameLeaf, Elem.Tok("of"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("referTo", List(
          Elem.Tok("refer"), Elem.NameLeaf, Elem.Tok("to"), Elem.NameLeaf, Elem.Tok(";"))),
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
      PrintRule("deferred", List(
        PrintSeg.Lit("deferred"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("porcelain", List(
        PrintSeg.Lit("porcelain"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("forkTrunk", List(
        PrintSeg.Lit("fork-from"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("trunk"), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("forkOf", List(
        PrintSeg.Lit("fork-from"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("of"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("referTo", List(
        PrintSeg.Lit("refer"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("to"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("expectfail", List(
        PrintSeg.Lit("expectfail"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Field(1)))),
    top = "transcript")

  /** Node name, absolute root, and whether a chain file exists (FS-gated probe). */
  final case class Report(
      name: String,
      steps: List[StepOutcome],
      workDir: Path,
      nodes: List[(String, Path, Boolean)] = Nil,
  ):
    def render: String =
      val body = (s"transcript '$name':" :: steps.map(s => "  ✓ " + s.summary)).mkString("\n")
      val nodeLines =
        if nodes.isEmpty then Nil
        else
          "" :: "blockchain nodes:" :: nodes.sortBy(_._1).map { (n, p, hasChain) =>
            val abs = p.toAbsolutePath.normalize
            val tip = if hasChain then " (has chain)" else ""
            s"  $n = $abs$tip"
          }
      val browse =
        nodes.find(_._1 == "nodeA").orElse(nodes.headOption) match
          case Some((_, p, _)) =>
            val abs = p.toAbsolutePath.normalize
            List(
              "",
              s"explorer: sbt \"examples/runMain cairn.examples.Main ui $abs\"",
              s"         → http://127.0.0.1:8765  (serving $abs)")
          case None =>
            List("", s"workDir: ${workDir.toAbsolutePath.normalize}")
      (body :: nodeLines ++ browse).mkString("\n")

    def summaries: List[String] = steps.map(_.summary)

  private[surface] def fsAbs(p: Path): Fs.Path = Fs.Path(p.toAbsolutePath.normalize.toString)

  private[surface] def fsErr(e: Fs.Error): String = e match
    case Fs.Error.NotFound(p) => s"not found: ${p.value}"
    case Fs.Error.Io(m)       => m

  private[surface] def fsRun(req: Fs.Request, ctx: EffectContext): Either[String, Fs.Response] =
    Filesystem.run(req, ctx).left.map(fsErr)

  private[surface] def fsExists(p: Path, ctx: EffectContext): Either[String, Boolean] =
    fsRun(Fs.Request.Exists(fsAbs(p)), ctx).flatMap {
      case Fs.Response.Bool(b) => Right(b)
      case other               => Left(s"unexpected fs response: $other")
    }

  private[surface] def fsMkdirs(p: Path, ctx: EffectContext): Either[String, Unit] =
    fsRun(Fs.Request.Mkdirs(fsAbs(p)), ctx).flatMap {
      case Fs.Response.Ok => Right(())
      case other          => Left(s"unexpected fs response: $other")
    }

  private[surface] def fsRead(p: Path, ctx: EffectContext): Either[String, String] =
    fsRun(Fs.Request.Read(fsAbs(p)), ctx).flatMap {
      case Fs.Response.Text(s) => Right(s)
      case other               => Left(s"unexpected fs response: $other")
    }

  private[surface] def fsReadBytes(p: Path, ctx: EffectContext): Either[String, Array[Byte]] =
    fsRun(Fs.Request.ReadBytes(fsAbs(p)), ctx).flatMap {
      case Fs.Response.Bytes(bs) => Right(bs)
      case other                 => Left(s"unexpected fs response: $other")
    }

  private[surface] def fsWrite(p: Path, content: String, ctx: EffectContext): Either[String, Unit] =
    fsRun(Fs.Request.Write(fsAbs(p), content), ctx).flatMap {
      case Fs.Response.Ok => Right(())
      case other          => Left(s"unexpected fs response: $other")
    }

  private[surface] def fsIsRegularFile(p: Path, ctx: EffectContext): Either[String, Boolean] =
    fsRun(Fs.Request.IsRegularFile(fsAbs(p)), ctx).flatMap {
      case Fs.Response.Bool(b) => Right(b)
      case other               => Left(s"unexpected fs response: $other")
    }

  /** Interpret a transcript against a working directory. `packs` is the
    * language registry (domain packs stay out of the surface layer — they are
    * injected by callers, §4.11). EffectContexts and packLoader are explicit — no
    * ambient AuthorityGate / PackAccess. Run/node directories use [[fsCtx]]
    * ([[EffectContext.forFilesystem]] at the CLI composition root).
    */
  def run(
      src: String,
      packs: Map[String, ComposedLanguage],
      workDir: Path,
      portModules: Map[String, cairn.core.RosettaModule2] = Map.empty,
      packLoader: PackLoader,
      ledgerCtx: EffectContext,
      processCtx: EffectContext,
      fsCtx: EffectContext,
  ): Either[String, Report] =
    Parser.parse(grammar, src).flatMap {
      case Cst.Node("transcript", List(Cst.Leaf(name), Cst.Node("list", steps))) =>
        runSteps(name, steps, packs, workDir, portModules, packLoader, ledgerCtx, processCtx, fsCtx)
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
      fsCtx: EffectContext,
  ): Either[String, Report] =
    var packs = packsIn
    var lang: Option[ComposedLanguage] = None
    var module = Module(Nil)
    val authority = Keypair.dev("dev-authority")
    def authorities = Map(authority.name -> authority.publicBytes)
    val log = List.newBuilder[StepOutcome]
    def ok(msg: String): Unit = log += StepOutcome.Executed(Canon.CStr(msg))
    def okCanon(c: Canon): Unit = log += StepOutcome.Executed(c)
    val nodes = scala.collection.mutable.LinkedHashMap[String, Node]()
    lazy val domainBranches =
      val cas = DiskCas(workDir.resolve("domain-cas"))
      cairn.systemhandler.Branches(cas, workDir.resolve("domain-refs"), EffectContext.forBranches())
    def ensureNode(n: String): Either[String, Node] =
      nodes.get(n) match
        case Some(node) => Right(node)
        case None =>
          val path = workDir.resolve(n).toAbsolutePath.normalize
          fsMkdirs(path, fsCtx).map { _ =>
            ok(s"node $n at $path")
            val node = Node(path, ledgerCtx)
            nodes(n) = node
            node
          }

    def need: Either[String, ComposedLanguage] = lang.toRight("no language selected (use `lang NAME ;` first)")
    def parseIn(l: ComposedLanguage, s: String): Either[String, Cst] = Parser.parse(l.grammar, s)

    def casPut(node: Node, art: Artifact): Either[String, Unit] =
      CasEffects.put(node.cas, art, node.ctx).left.map {
        case cairn.systeminterface.Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
        case cairn.systeminterface.Cas.Error.Io(m)      => m
      }.map(_ => ())

    def publishTo(node: Node, branch: String): Either[String, Unit] =
      need.flatMap { l =>
        for
          _ <- l.fragments.foldLeft[Either[String, Unit]](Right(()))((acc, f) =>
            acc.flatMap(_ => casPut(node, f.artifact)))
          _ <- casPut(node, l.artifact)
          _ <- casPut(node, module.artifact)
          txs =
            List(authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes))) ++
            l.fragments.map(f => authority.signTx(Tx.PublishArtifact(f.artifact.key))) ++
            List(
              authority.signTx(Tx.PublishArtifact(l.artifact.key)),
              authority.signTx(Tx.PublishArtifact(module.artifact.key)),
              authority.signTx(Tx.SetBranchHead(branch, module.artifact.key)))
          b <- node.append(authority, authorities, txs)
        yield
          ok(s"published $branch at block ${b.digest.short} root ${b.stateRoot.short}")
          ok(s"blockchain node: ${node.root.toAbsolutePath.normalize}")
      }

    def fetchBetween(branch: String, fromN: String, toN: String): Either[String, Unit] =
      for
        from <- ensureNode(fromN)
        to <- ensureNode(toN)
        _ <- Sync.pull(from, to, authorities)
        st <- to.state(authorities)
        head <- st.heads.get(branch).toRight(s"branch '$branch' not on ledger")
        art <- CasEffects.get(to.cas, head.valueHash, to.ctx).left.map {
          case cairn.systeminterface.Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
          case cairn.systeminterface.Cas.Error.Io(m)      => m
        }
      yield ok(s"fetched $branch head ${art.digest.short} on $toN")

    def runStep(step: Cst): Either[String, Unit] =
      step match
        case Cst.Node("lang", List(Cst.Leaf(n))) =>
          packs.get(n).toRight(s"unknown language pack '$n' (registered: ${packs.keys.mkString(", ")})")
            .map { l => lang = Some(l); module = Module(Nil); ok(s"lang $n (${l.digest.short})") }
        case Cst.Node("loadLang", List(Cst.Leaf(file))) =>
          // Transcript-embedded paths predate the container/content/app move
          // and still say "languages/..."; resolve those under content/.
          val p = Path.of(if file.startsWith("languages/") then s"content/$file" else file)
          (for
            exists <- fsExists(p, fsCtx)
            _ <- Either.cond(exists, (), s"no such language file: $file")
            text <- fsRead(p, fsCtx)
            l <- Meta.parseLanguageAst(text).flatMap { (name, fs) =>
              val dir = Option(p.toAbsolutePath.normalize.getParent).getOrElse(Path.of("."))
              val packsHere = packLoader.loadRaw(dir) + (name -> fs)
              val surfaces = packLoader.loadSurfaces(dir)
              packLoader.close(name, packsHere, surfaces).left.map(_.map(_.render).mkString("\n"))
            }
          yield l).map { l =>
            packs = packs + (l.name -> l)
            ok(s"loaded language ${l.name} (${l.digest.short}) from $file") }
        case Cst.Node("nodeD", List(Cst.Leaf(n))) =>
          ensureNode(n).map(_ => ())
        case Cst.Node("roundtrip", List(Cst.Leaf(s))) =>
          for
            l <- need
            t <- parseIn(l, s)
            _ <- RoundTrip.check(l.grammar, t)
          yield ok(s"roundtrip ok: $s")
        case Cst.Node("eval", List(Cst.Leaf(src), Cst.Leaf(expected))) =>
          for
            l <- need
            t <- parseIn(l, src)
            e <- parseIn(l, expected)
            v <- TreeEngine.normalize(l, t)
            _ <- if v == e then Right(()) else Left(s"eval mismatch: $src ~> ${v.render}, expected ${e.render}")
          yield ok(s"eval $src => $expected")
        case Cst.Node("delta", List(Cst.Leaf(src))) =>
          for
            l <- need
            dl <- Delta.deltaOf(l).left.map(_.map(_.render).mkString("; "))
            ch <- Parser.parse(dl.grammar, src)
            res <- Delta.apply(l, module, ch)
          yield
            module = res._1
            ok(s"delta applied: module ${res._2.base.short} -> ${res._2.result.short}")
        case Cst.Node("claim", List(Cst.Leaf(cn), Cst.Leaf(input), Cst.Leaf(expected))) =>
          for
            l <- need
            i <- parseIn(l, input)
            e <- parseIn(l, expected)
            claim = cairn.proof.Claim(cn, Cst.node("claimEval", i, e), module.digest)
            suite = cairn.proof.TestSuite(s"$cn-tests", module.digest,
              List(cairn.proof.TestCase(cn, i, e)))
            cert <- cairn.proof.Certify.byTests(claim, suite, t => TreeEngine.normalize(l, t))
          yield ok(s"claim $cn certified (${cert.artifact.digest.short})")
        case Cst.Node("publishOn", List(Cst.Leaf(branch), Cst.Leaf(nodeName))) =>
          ensureNode(nodeName).flatMap(publishTo(_, branch))
        case Cst.Node("publish", List(Cst.Leaf(branch))) =>
          ensureNode("nodeA").flatMap(publishTo(_, branch))
        case Cst.Node("fetchBetween", List(Cst.Leaf(branch), Cst.Leaf(f), Cst.Leaf(t))) =>
          fetchBetween(branch, f, t)
        case Cst.Node("fetch", List(Cst.Leaf(branch))) =>
          fetchBetween(branch, "nodeA", "nodeB")
        case Cst.Node("gossip", List(Cst.Node("list", names))) =>
          val peerNames = names.collect { case Cst.Leaf(n) => n }
          peerNames.foldLeft[Either[String, List[Gossip.Peer]]](Right(Nil)) { (acc, n) =>
            acc.flatMap(ps => ensureNode(n).map(node => ps :+ Gossip.Peer(n, node)))
          }.flatMap { peers =>
            Gossip.converge(peers, authorities).map { reorgs =>
              ok(s"gossip converged over ${peers.map(_.name).mkString(",")} (${reorgs.length} reorgs)") }
          }
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
                  val scalaCli = sys.env.getOrElse("PATH", "").split(":").iterator
                    .map(dir => Path.of(dir, "scala-cli"))
                    .find { cand =>
                      fsRun(Fs.Request.IsExecutable(fsAbs(cand)), fsCtx).exists {
                        case Fs.Response.Bool(true) => true
                        case _                      => false
                      }
                    }
                  scalaCli match
                    case None => Right(ok(s"port $host verified (host toolchain absent, fixpoint only)"))
                    case Some(cli) =>
                      val portDir = workDir.resolve(s"port-${java.util.UUID.randomUUID()}")
                      val f = portDir.resolve(out.fileName)
                      fsMkdirs(portDir, fsCtx).flatMap(_ => fsWrite(f, out.text, fsCtx)).flatMap { _ =>
                        cairn.systemhandler.Process.run(
                          cairn.systeminterface.Process.Request.Run(
                            List(cli.toString, "run", "--server=false", f.toString)),
                          processCtx
                        ) match
                          case Right(r) if r.ok && r.combined.contains("ALL TESTS PASS") =>
                            Right(ok(s"port $host tests pass in host"))
                          case Right(r) => Left(s"port $host host run failed:\n${r.combined}")
                          case Left(e)  => Left(s"port $host host run failed: $e")
                      }
                else Right(ok(s"port $host verified (byte fixpoint)"))
              }
            }
          }.map(_ => ())
        case Cst.Node("query", List(Cst.Leaf(qsrc), Cst.Leaf(expected))) =>
          for
            q <- Query.parse(qsrc)
            res <- Query.run(q, module)
            _ <- if res.hits.length == expected.toInt then Right(())
                 else Left(s"query '$qsrc' returned ${res.hits.length} hits, expected $expected")
          yield ok(s"query ok: $qsrc => ${res.hits.length}")
        case Cst.Node("expectfail", List(Cst.Leaf(substring), inner)) =>
          runStep(inner) match
            case Left(err) if err.contains(substring) =>
              log += StepOutcome.ExpectedFailure(Canon.CStr(substring)); Right(())
            case Left(err) => Left(s"failed with wrong message: $err (wanted ...$substring...)")
            case Right(_)  => Left(s"step succeeded but was expected to fail with ...$substring...")
        case Cst.Node("deferred", List(Cst.Leaf(reason))) =>
          log += StepOutcome.Deferred(reason, "transcript"); Right(())
        case Cst.Node("porcelain", List(Cst.Leaf(theme))) =>
          val home = workDir
          val e = Porcelain.env(home, packLoader, ledgerCtx)
          Plumbing.charbTheme(theme, e).map { out =>
            okCanon(Canon.CTag("porcelain", Canon.CStr(theme)))
            out.linesIterator.foreach(line => ok(s"  $line"))
          }
        case Cst.Node("forkTrunk", List(Cst.Leaf(child))) =>
          domainBranches.forkFrom(child, None).map { m =>
            ok(s"fork-from $child trunk (primary=${m.primaryAncestor.getOrElse("∅")})")
          }
        case Cst.Node("forkOf", List(Cst.Leaf(child), Cst.Leaf(parent))) =>
          domainBranches.forkFrom(child, Some(parent)).map { m =>
            ok(s"fork-from $child of $parent (primary=${m.primaryAncestor.getOrElse("∅")})")
          }
        case Cst.Node("referTo", List(Cst.Leaf(child), Cst.Leaf(other))) =>
          domainBranches.referTo(child, other).map { m =>
            ok(s"refer $child to $other (refs=${m.references.mkString(",")})")
          }
        case other => Left(s"unknown transcript step: ${other.render}")

    val result = steps.foldLeft[Either[String, Unit]](Right(())) { (acc, step) =>
      acc.flatMap(_ => runStep(step)) }
    result.flatMap { _ =>
      nodes.toList.foldLeft[Either[String, List[(String, Path, Boolean)]]](Right(Nil)) {
        case (acc, (n, node)) =>
          acc.flatMap { xs =>
            val abs = node.root.toAbsolutePath.normalize
            fsExists(abs.resolve("chain"), fsCtx).map(has => xs :+ (n, abs, has))
          }
      }.map(ns => Report(name, log.result(), workDir.toAbsolutePath.normalize, ns))
    }

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
      fsCtx: EffectContext,
  ): Either[String, String] =
    /** Durable store root. Override with env `CAIRN_HOME`.
      * Default: `./.cas`. Each transcript writes a fresh run under
      * `$CAIRN_HOME/runs/<timestamp>/{nodeA,nodeB,…}` and prints those paths.
      * `ui` opens the latest run's `nodeA` (via `$CAIRN_HOME/LATEST`).
      * Home/run/ui path I/O goes through [[Filesystem]] on [[fsCtx]].
      */
    val home = Path.of(sys.env.getOrElse("CAIRN_HOME", ".cas")).toAbsolutePath.normalize
    val casDir = home
    def cas = DiskCas(casDir)
    def defaultUiRoot: Either[String, Path] =
      val latestFile = home.resolve("LATEST")
      for
        isLatest <- Transcript.fsIsRegularFile(latestFile, fsCtx)
        fromLatest <-
          if isLatest then
            Transcript.fsRead(latestFile, fsCtx).flatMap { text =>
              val nodeA = Path.of(text.trim).resolve("nodeA")
              Transcript.fsExists(nodeA.resolve("chain"), fsCtx).map(ok => Option.when(ok)(nodeA))
            }
          else Right(None)
        fallback <- fromLatest match
          case Some(p) => Right(p)
          case None =>
            val nodeA = home.resolve("nodeA")
            for
              hasNodeA <- Transcript.fsExists(nodeA.resolve("chain"), fsCtx)
              hasHome <- if hasNodeA then Right(false)
                         else Transcript.fsExists(home.resolve("chain"), fsCtx)
            yield
              if hasNodeA then nodeA
              else if hasHome then home
              else nodeA
      yield fallback
    val packs = packsIn ++ packLoader.loadClosed() ++ loadLanguages(Path.of("content/languages"), packLoader)
    args match
      case List("home") =>
        for
          isLatest <- Transcript.fsIsRegularFile(home.resolve("LATEST"), fsCtx)
          latest <-
            if isLatest then Transcript.fsRead(home.resolve("LATEST"), fsCtx).map(s => Some(s.trim))
            else Right(None)
          uiRoot <- defaultUiRoot
          hasChain <- Transcript.fsExists(uiRoot.resolve("chain"), fsCtx)
        yield
          s"""CAIRN_HOME=$home
             |env=${sys.env.getOrElse("CAIRN_HOME", "<unset> (default ./.cas)")}
             |latest-run=${latest.getOrElse("<none>")}
             |ui-default=$uiRoot
             |ui-has-chain=$hasChain
             |""".stripMargin.trim
      case List("hash", file) =>
        Transcript.fsReadBytes(Path.of(file), fsCtx).map(bs => Digest.ofBytes(bs).hex)
      case List("put", file) =>
        for
          bs <- Transcript.fsReadBytes(Path.of(file), fsCtx)
          d <- CasEffects.putBytes(cas, bs, ledgerCtx).left.map {
            case cairn.systeminterface.Cas.Error.Missing(d) => s"blob ${d.short} not in CAS"
            case cairn.systeminterface.Cas.Error.Io(m)      => m
          }
        yield d.hex
      case List("get", hex) =>
        Digest.parse(hex).flatMap { d =>
          CasEffects.getBytes(cas, d, ledgerCtx).left.map {
            case cairn.systeminterface.Cas.Error.Missing(_) => s"blob ${d.short} not in CAS"
            case cairn.systeminterface.Cas.Error.Io(m)      => m
          }
        }.map(bs => new String(bs, "UTF-8"))
      case List("canon", file) =>
        for
          bs <- Transcript.fsReadBytes(Path.of(file), fsCtx)
          c <- Canon.decode(bs)
        yield Digest.of(c).hex
      case List("transcript", file) =>
        val runId = java.time.LocalDateTime.now()
          .format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))
        val runDir = home.resolve("runs").resolve(runId).toAbsolutePath.normalize
        for
          src <- Transcript.fsRead(Path.of(file), fsCtx)
          _ <- Transcript.fsMkdirs(runDir, fsCtx)
          r <- Transcript.run(src, packs, runDir, portModules, packLoader, ledgerCtx, processCtx, fsCtx)
          _ <- Transcript.fsWrite(home.resolve("LATEST"), runDir.toString + "\n", fsCtx)
        yield r.render
      case List("why", hex) =>
        Digest.parse(hex).flatMap { d =>
          Provenance.why(casDir, d, ledgerCtx).map { hops =>
            if hops.isEmpty then s"no provenance recorded for ${d.short}"
            else hops.map(h => s"${"  " * h.depth}${h.record.output.short} <- ${h.record.tool}(${h.record.inputs.map(_.short).mkString(", ")})").mkString("\n")
          }
        }
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
        val branches = cairn.systemhandler.Branches(cas, refs, EffectContext.forBranches())
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
                  _ = branches.importModule("demo-base", m0)
                  tipA <- SemanticRepository.tipAfter(lang, m0, cA)
                  tipB <- SemanticRepository.tipAfter(lang, m0, cB)
                  _ = branches.commitTip("demo-a", tipA)
                  _ = branches.commitTip("demo-b", tipB)
                  outcome <- branches.mergeBranches(lang, "demo-main", "demo-a", "demo-b")
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
        val port = portOpt.getOrElse(8765)
        for
          resolved <- rootArg.map(p => Right(p.toAbsolutePath.normalize)).getOrElse(defaultUiRoot)
          _ <- Transcript.fsMkdirs(resolved, fsCtx)
          hasChain <- Transcript.fsExists(resolved.resolve("chain"), fsCtx)
          bound <- BrowserServer.serve(resolved, packs, ledgerCtx, fsCtx, port)
        yield
          System.out.println(s"Cairn Explorer at http://127.0.0.1:$bound")
          System.out.println(s"CAIRN_HOME=$home")
          System.out.println(s"serving root=$resolved")
          if !hasChain then
            System.out.println("NOTE: empty node (no chain file). Seed then reopen ui:")
            System.out.println("""  sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"""")
            System.out.println(s"  (writes $home/nodeA)")
          System.out.println("Press Enter to stop.")
          scala.io.StdIn.readLine()
          "ui stopped"
      case List("serve") =>
        serveHttp(home, 0, ledgerCtx, withBft = false)
      case List("serve", portStr) if portStr.forall(_.isDigit) =>
        serveHttp(home, portStr.toInt, ledgerCtx, withBft = false)
      case List("serve", "replica", name) =>
        serveHttp(home, 0, ledgerCtx, withBft = true, replicaName = name)
      case List("serve", "replica", name, portStr) if portStr.forall(_.isDigit) =>
        serveHttp(home, portStr.toInt, ledgerCtx, withBft = true, replicaName = name)
      case List("pull", baseUrl) =>
        pullHttp(home, baseUrl, ledgerCtx)
      case List("fetch-hash", baseUrl, hex) =>
        Digest.parse(hex).flatMap { d =>
          val node = Node(home.resolve("nodeA"), ledgerCtx)
          HttpSync.fetchByHash(baseUrl, node, d).map(got => s"fetched ${got.hex}")
        }
      case List("peer", "list") =>
        PeerRegistry.load(home).map { d =>
          if d.peers.isEmpty then "(no peers)"
          else d.peers.map(p => s"${p.name}\t${p.role.name}\t${p.baseUrl}").mkString("\n")
        }
      case List("peer", "add", name, url) =>
        peerAdd(home, name, url, PeerRegistry.Role.Gossip)
      case List("peer", "add", name, url, role) =>
        PeerRegistry.Role.parse(role).flatMap(r => peerAdd(home, name, url, r))
      case List("peer", "remove", name) =>
        PeerRegistry.remove(home, name).map(d => s"peers=${d.peers.size} removed $name")
      case List("peer", "discover", url) =>
        HttpGossip.discover(home, List(url)).map { d =>
          s"discovered ${d.peers.size} peers from $url"
        }
      case List("gossip", "once") =>
        gossipOnce(home, ledgerCtx)
      case List("gossip", "run", nStr) if nStr.forall(_.isDigit) =>
        val n = nStr.toInt
        val root = home.resolve("nodeA").toAbsolutePath.normalize
        java.nio.file.Files.createDirectories(root)
        val node = Node(root, ledgerCtx)
        defaultAuthorities(home).map { auth =>
          val daemon = GossipDaemon("nodeA", node, home, auth, intervalMs = 50)
          val reports = (1 to n).map(_ => daemon.tick())
          daemon.stop()
          val reorgs = reports.map(_.reorgs.size).sum
          s"gossip ticks=$n reorgs=$reorgs lastErrors=${reports.lastOption.map(_.errors.size).getOrElse(0)}"
        }
      case List("bft", "agree", hex) =>
        Digest.parse(hex).flatMap { d =>
          bftAgree(home, d, ledgerCtx)
        }
      case List("bft", "agree", "local", hex) =>
        Digest.parse(hex).flatMap { d =>
          bftAgreeLocal(home, d, ledgerCtx)
        }
      case "bft" :: "replica-set" :: "init" :: names if names.nonEmpty =>
        bftReplicaSetInit(home, names)
      case List("bft", "replica-set", "keygen", name) =>
        keystoreSecret.flatMap { sec =>
          BftCeremony.keygen(home, name, sec).map { kp =>
            s"keygen ${kp.name} pk=${Digest.of(Canon.CBytes(kp.publicBytes)).short}"
          }
        }
      case List("bft", "replica-set", "export-pubkey", name) =>
        keystoreSecret.flatMap { sec =>
          val out = BftCeremony.pubkeyPath(home, name)
          BftCeremony.exportPubkey(home, name, out, sec).map(p => s"exported pubkey $name -> $p")
        }
      case List("bft", "replica-set", "export-pubkey", name, outStr) =>
        keystoreSecret.flatMap { sec =>
          BftCeremony.exportPubkey(home, name, Path.of(outStr), sec)
            .map(p => s"exported pubkey $name -> $p")
        }
      case List("bft", "replica-set", "import-pubkey", pathStr) =>
        BftCeremony.importPubkey(home, Path.of(pathStr)).map { (id, dest) =>
          s"imported pubkey $id -> $dest"
        }
      case "bft" :: "replica-set" :: "assemble" :: args =>
        parseAssembleArgs(args).flatMap { (ids, activation, replaces) =>
          BftCeremony.assemble(home, ids, activation, replaces).map { d =>
            s"draft assembled body=${Digest.of(d.bodyCanon).short} n=${d.n} " +
              s"activation=${d.activationHeight} replaces=${d.replaces.map(_.short).getOrElse("none")}"
          }
        }
      case List("bft", "replica-set", "export-draft", outStr) =>
        BftCeremony.exportDraft(home, Path.of(outStr)).map(p => s"exported draft -> $p")
      case List("bft", "replica-set", "import-draft", pathStr) =>
        BftCeremony.importDraft(home, Path.of(pathStr)).map { d =>
          s"imported draft body=${Digest.of(d.bodyCanon).short} n=${d.n}"
        }
      case List("bft", "replica-set", "seal", name) =>
        keystoreSecret.flatMap { sec =>
          BftCeremony.sealMember(home, name, sec).map(p => s"sealed member $name -> $p")
        }
      case List("bft", "replica-set", "import-seal", pathStr) =>
        BftCeremony.importSeal(home, Path.of(pathStr)).map { (id, dest) =>
          s"imported seal $id -> $dest"
        }
      case List("bft", "replica-set", "approve", name) =>
        keystoreSecret.flatMap { sec =>
          BftCeremony.approve(home, name, sec).map(p => s"approval $name -> $p")
        }
      case List("bft", "replica-set", "import-approval", pathStr) =>
        BftCeremony.importApproval(home, Path.of(pathStr)).map { (id, dest) =>
          s"imported approval $id -> $dest"
        }
      case List("bft", "replica-set", "finalize") =>
        BftCeremony.commit(home).map { m =>
          s"finalized replica-set ${m.digest.short} n=${m.n} ids=${m.ids.mkString(",")}"
        }
      case List("bft", "replica-set", "export", outStr) =>
        BftCeremony.exportBundle(home, Path.of(outStr)).map(p => s"exported bundle -> $p")
      case List("bft", "replica-set", "install", pathStr) =>
        BftCeremony.installBundle(home, Path.of(pathStr)).map { m =>
          s"installed replica-set ${m.digest.short} n=${m.n}"
        }
      case List("bft", "replica-set", "status") =>
        BftCeremony.status(home)
      case List("smoke", "distribution") =>
        smokeDistribution(ledgerCtx)
      case porcelainCmd :: rest
          if Set("chain", "auth", "branch", "domain", "compose", "catalog",
            "workflow", "recover", "replay", "tx", "light", "porcelain").contains(porcelainCmd) =>
        Porcelain.dispatch(porcelainCmd :: rest, home, packLoader, ledgerCtx)
      case _ =>
        Left(
          "usage: cairn [home|hash|put|get|canon|transcript|why|capabilities|languages|repo|" +
            "serve|pull|fetch-hash|peer|gossip|bft|smoke|" +
            "chain|auth|branch|domain|compose|catalog|workflow|recover|replay|tx|light|porcelain|" +
            "repl|lsp|ui] <arg>")

  private def defaultAuthorities(home: Path): Either[String, Map[String, Vector[Byte]]] =
    keystoreLoadOrCreate(home, "dev-authority").map(kp => Map(kp.name -> kp.publicBytes))

  /** Require CAIRN_KEYSTORE_SECRET or lab plaintext; never silently invent keys over failures. */
  private def keystoreLoadOrCreate(home: Path, name: String): Either[String, Keypair] =
    keystoreSecret.flatMap(sec => Keystore.loadOrCreate(home, name, sec))

  private def keystoreSecret: Either[String, Option[Array[Byte]]] =
    Keystore.envSecret match
      case Some(sec) => Right(Some(sec))
      case None if Keystore.allowPlaintext => Right(None)
      case None =>
        Left(
          "keystore: set CAIRN_KEYSTORE_SECRET (or CAIRN_KEYSTORE_PLAINTEXT=1 for lab)")

  /** Parse `assemble [--activation N] [--replaces HEX] id…`. */
  private def parseAssembleArgs(
      args: List[String],
  ): Either[String, (List[String], Long, Option[Digest])] =
    def loop(
        rest: List[String],
        activation: Long,
        replaces: Option[Digest],
        ids: List[String],
    ): Either[String, (List[String], Long, Option[Digest])] =
      rest match
        case Nil =>
          if ids.isEmpty then Left("usage: bft replica-set assemble [--activation N] [--replaces HEX] <id>…")
          else Right((ids, activation, replaces))
        case "--activation" :: n :: tail if n.forall(c => c.isDigit) =>
          loop(tail, n.toLong, replaces, ids)
        case "--replaces" :: hex :: tail =>
          Digest.parse(hex).flatMap(d => loop(tail, activation, Some(d), ids))
        case id :: tail if !id.startsWith("--") =>
          loop(tail, activation, replaces, ids :+ id)
        case bad :: _ =>
          Left(s"ceremony: bad assemble argument '$bad'")
    loop(args, 0L, None, Nil)

  private def serveHttp(
      home: Path,
      port: Int,
      ledgerCtx: EffectContext,
      withBft: Boolean,
      replicaName: String = "local",
  ): Either[String, String] =
    val root = home.resolve("nodeA").toAbsolutePath.normalize
    java.nio.file.Files.createDirectories(root)
    val node = Node(root, ledgerCtx)
    for
      ledgerAuth <- defaultAuthorities(home)
      bftOpt <-
        if !withBft then Right(None)
        else
          for
            hist <- BftFinality.loadReplicaSetHistory(home)
            tipHeight =
              val digs = node.chainDigests
              if digs.isEmpty then 0L else (digs.length - 1).toLong
            manifest <- hist.activeAt(tipHeight)
            kp <- keystoreLoadOrCreate(home, replicaName)
            bft <- BftReplica.certified(
              kp, manifest,
              node = Some(node), ledgerAuth = ledgerAuth,
              certStore = Some(home.resolve("bft-certs.canon")),
              stateStore = Some(home.resolve("bft-state.canon")),
              home = Some(home))
          yield Some(bft)
      http = HttpNode(node, ledgerAuth, peersRoot = Some(home), bft = bftOpt)
      bound = http.start(port)
      selfUrl = s"http://127.0.0.1:$bound"
      role = if withBft then PeerRegistry.Role.Replica else PeerRegistry.Role.Gossip
      pk = bftOpt.map(_.keypair.publicBytes)
      _ <- bftOpt match
        case Some(bft) =>
          PeerRegistry.addBound(home, bft.keypair, selfUrl, role)
        case None =>
          PeerRegistry.add(home, replicaName, selfUrl, role, publicKey = pk)
    yield
      System.out.println(s"Cairn HTTP node at $selfUrl")
      System.out.println(s"  GET /chain  /heads  /blob/<digest>  /peers")
      if withBft then
        System.out.println(s"  POST /bft/msg  POST /bft/propose  GET /bft/certs  (replica=$replicaName)")
      else
        System.out.println(s"  (gossip only — use `serve replica <name>` to enable BFT)")
      System.out.println(s"serving $root (CAIRN_HOME=$home)")
      System.out.println("Press Enter to stop.")
      scala.io.StdIn.readLine()
      http.stop()
      s"serve stopped (was :$bound)"

  /** Plant a peer; if a local keystore key matches `name`, bind the URL with a seal. */
  private def peerAdd(
      home: Path,
      name: String,
      url: String,
      role: PeerRegistry.Role,
  ): Either[String, String] =
    keystoreSecret match
      case Right(sec) =>
        Keystore.load(home, name, sec) match
          case Right(kp) =>
            PeerRegistry.addBound(home, kp, url, role).map { d =>
              s"peers=${d.peers.size} added $name (${role.name}, bound)"
            }
          case Left(_) =>
            if role == PeerRegistry.Role.Replica then
              Left(s"peer add: replica '$name' requires a local keystore key to sign the URL")
            else
              PeerRegistry.add(home, name, url, role).map(d =>
                s"peers=${d.peers.size} added $name (${role.name})")
      case Left(_) if role == PeerRegistry.Role.Replica =>
        Left("peer add: replica role requires CAIRN_KEYSTORE_SECRET and a local key")
      case Left(_) =>
        PeerRegistry.add(home, name, url, role).map(d =>
          s"peers=${d.peers.size} added $name (${role.name})")

  /** Network agreement: ask the designated primary to propose (signed by a local replica key). */
  private def bftAgree(home: Path, block: Digest, ledgerCtx: EffectContext): Either[String, String] =
    val root = home.resolve("nodeA").toAbsolutePath.normalize
    java.nio.file.Files.createDirectories(root)
    val node = Node(root, ledgerCtx)
    for
      ledgerAuth <- defaultAuthorities(home)
      proved <- BftFinality.requireSealedBlock(node, ledgerAuth, block)
      height = proved._2
      manifest <- BftFinality.loadActiveReplicaSet(home, height)
      dir <- PeerRegistry.load(home)
      urls <- PeerRegistry.resolveReplicaUrls(dir, manifest)
      initiator <- localReplicaSigner(home, manifest.ids)
      genesis <- BftFinality.chainId(node)
      cert <- BftFinality.agreeNetworkRemote(
        urls, block, initiator, chainId = genesis, replicaSet = manifest.replicaSetDigest)
      hist <- BftFinality.loadReplicaSetHistory(home)
      _ <- BftFinality.FinalityCertificate.verifyAgainstHistory(cert, hist, node, ledgerAuth)
      _ <- BftFinality.advanceCheckpoint(home, cert)
    yield s"bft network finality ${cert.digest.short} for block ${block.short} commits=${cert.commits.size}"

  /** First local keystore identity that is a member of the replica set. */
  private def localReplicaSigner(home: Path, ids: List[String]): Either[String, Keypair] =
    keystoreSecret.flatMap { sec =>
      ids.foldLeft[Either[String, Keypair]](Left("bft: no local replica key in keystore")) { (acc, id) =>
        acc match
          case Right(kp) => Right(kp)
          case Left(_)   => Keystore.load(home, id, sec)
      }.left.map(_ =>
        s"bft: need a local keystore key for one of ${ids.mkString(",")}")
    }

  /** Create local keypairs for each name and write a sealed replica-set.canon. */
  private def bftReplicaSetInit(home: Path, names: List[String]): Either[String, String] =
    for
      kps <- names.foldLeft[Either[String, List[Keypair]]](Right(Nil)) { (acc, n) =>
        acc.flatMap(ks => keystoreLoadOrCreate(home, n).map(ks :+ _))
      }
      sealedM <- BftFinality.sealReplicaSet(kps)
      _ <- BftFinality.saveReplicaSet(BftFinality.defaultReplicaSetPath(home), sealedM)
    yield s"replica-set ${sealedM.digest.short} n=${sealedM.n} ids=${sealedM.ids.mkString(",")}"

  /** In-process lab agreement bound to a local sealed block (not network). */
  private def bftAgreeLocal(home: Path, block: Digest, ledgerCtx: EffectContext): Either[String, String] =
    val root = home.resolve("nodeA").toAbsolutePath.normalize
    java.nio.file.Files.createDirectories(root)
    val node = Node(root, ledgerCtx)
    val names = List("r0", "r1", "r2", "r3")
    for
      ledgerAuth <- defaultAuthorities(home)
      replicas <- names.foldLeft[Either[String, List[Keypair]]](Right(Nil)) { (acc, n) =>
        acc.flatMap(ks => keystoreLoadOrCreate(home, n).map(ks :+ _))
      }
      cert <- BftFinality.agreeForSealedBlock(node, ledgerAuth, replicas, block)
      manifest <- BftFinality.sealReplicaSet(replicas)
      _ <- BftFinality.FinalityCertificate.verifyAgainstChain(cert, manifest, node, ledgerAuth)
    yield s"bft local finality ${cert.digest.short} for block ${block.short} commits=${cert.commits.size}"

  private def pullHttp(home: Path, baseUrl: String, ledgerCtx: EffectContext): Either[String, String] =
    val root = home.resolve("nodeA").toAbsolutePath.normalize
    java.nio.file.Files.createDirectories(root)
    val node = Node(root, ledgerCtx)
    for
      auth <- defaultAuthorities(home)
      checkpoint <- BftFinality.recoverAndLoadCheckpoint(home, node, auth)
      r <- HttpSync.pull(baseUrl, node, auth, checkpoint, Some(home))
    yield s"pull ok: blocks=${r.fetchedBlocks} blobs=${r.fetchedBlobs} alreadyHad=${r.alreadyHad}"

  private def gossipOnce(home: Path, ledgerCtx: EffectContext): Either[String, String] =
    val root = home.resolve("nodeA").toAbsolutePath.normalize
    java.nio.file.Files.createDirectories(root)
    val node = Node(root, ledgerCtx)
    for
      auth <- defaultAuthorities(home)
      checkpoint <- BftFinality.recoverAndLoadCheckpoint(home, node, auth)
    yield
      val report = HttpGossip.round(
        "nodeA", node,
        PeerRegistry.load(home).toOption.toList.flatMap(_.gossipPeers),
        auth, checkpoint, Some(home))
      s"gossip once: pulled=${report.pulled.mkString(",")}" +
        s" reorgs=${report.reorgs.size} errors=${report.errors.size}"

  /** Packaged-executable smoke: two-node HTTP gossip + four-node BFT certificate. */
  private def smokeDistribution(ledgerCtx: EffectContext): Either[String, String] =
    val auth = Keypair.dev("auth")
    val ledgerAuth = Map(auth.name -> auth.publicBytes)
    // --- two-node gossip ---
    val aRoot = java.nio.file.Files.createTempDirectory("cairn-smoke-a")
    val bRoot = java.nio.file.Files.createTempDirectory("cairn-smoke-b")
    val peersRoot = java.nio.file.Files.createTempDirectory("cairn-smoke-p")
    val a = Node(aRoot, ledgerCtx)
    val b = Node(bRoot, ledgerCtx)
    a.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes)))) match
      case Left(e) => Left(e)
      case Right(_) =>
        val httpA = HttpNode(a, ledgerAuth, peersRoot = Some(peersRoot))
        val portA = httpA.start()
        try
          PeerRegistry.add(peersRoot, "a", s"http://127.0.0.1:$portA") match
            case Left(e) => Left(e)
            case Right(_) =>
              val report = HttpGossip.round("b", b,
                PeerRegistry.load(peersRoot).fold(_ => Nil, _.gossipPeers), ledgerAuth)
              if report.errors.nonEmpty then Left(s"gossip errors: ${report.errors}")
              else if b.chainDigests != a.chainDigests then
                Left(s"gossip diverge: a=${a.chainDigests} b=${b.chainDigests}")
              else
                // --- four-node BFT ---
                smokeBft(ledgerCtx, auth, ledgerAuth).map { certShort =>
                  s"smoke distribution ok: gossip-pulled=a bft-cert=$certShort"
                }
        finally httpA.stop()

  private def smokeBft(
      ledgerCtx: EffectContext,
      auth: Keypair,
      ledgerAuth: Map[String, Vector[Byte]],
  ): Either[String, String] =
    val replicas = List("r0", "r1", "r2", "r3").map(Keypair.dev)
    BftFinality.sealReplicaSet(replicas).flatMap { manifest =>
      val ids = manifest.ids
      val homes = ids.map(id => id -> java.nio.file.Files.createTempDirectory(s"cairn-smoke-$id")).toMap
      val seeded: Either[String, Map[String, Node]] =
        ids.foldLeft[Either[String, Map[String, Node]]](Right(Map.empty)) { (acc, id) =>
          acc.flatMap { m =>
            val n = Node(homes(id).resolve("node"), ledgerCtx)
            n.append(auth, ledgerAuth, List(auth.signTx(Tx.RegisterIdentity(auth.name, auth.publicBytes))))
              .map(_ => m + (id -> n))
          }
        }
      seeded.flatMap { nodes =>
        val block = nodes("r0").chainDigests.head
        val https = scala.collection.mutable.ListBuffer.empty[HttpNode]
        try
          val portsE = ids.foldLeft[Either[String, Map[String, Int]]](Right(Map.empty)) { (acc, id) =>
            acc.flatMap { ports =>
              val peersRoot = homes(id)
              BftReplica.certified(
                replicas.find(_.name == id).get, manifest,
                node = Some(nodes(id)), ledgerAuth = ledgerAuth).map { bft =>
                val http = HttpNode(nodes(id), ledgerAuth, peersRoot = Some(peersRoot), bft = Some(bft))
                https += http
                ports + (id -> http.start())
              }
            }
          }
          for
            ports <- portsE
            _ <- ids.foldLeft[Either[String, Unit]](Right(())) { (acc, id) =>
              acc.flatMap { _ =>
                ids.foldLeft[Either[String, Unit]](Right(())) { (acc2, peer) =>
                  acc2.flatMap { _ =>
                    val kp = replicas.find(_.name == peer).get
                    PeerRegistry.addBound(
                      homes(id), kp, s"http://127.0.0.1:${ports(peer)}",
                      PeerRegistry.Role.Replica).map(_ => ())
                  }
                }
              }
            }
            urls = ids.map(id => id -> s"http://127.0.0.1:${ports(id)}").toMap
            cert <- BftFinality.agreeNetworkRemote(
              urls, block, replicas.head,
              chainId = block, replicaSet = manifest.replicaSetDigest,
              polls = 64, pollSleepMs = 30)
            _ <- BftFinality.FinalityCertificate.verifyAgainstChain(
              cert, manifest, nodes("r0"), ledgerAuth)
          yield s"bft ok ${cert.digest.short}"
        finally https.foreach(_.stop())
      }
    }
