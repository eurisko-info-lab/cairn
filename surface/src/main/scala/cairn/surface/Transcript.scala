package cairn.surface

import cairn.kernel.*
import cairn.workbench.*
import cairn.compute.TreeEngine
import cairn.ledger.*
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
        "delta", "claim", "publish", "fetch"),
      puncts = List("{", "}", ";"),
      lineComment = Some("--")),
    categories = List(
      CategorySpec("transcript", List(
        ConstructorSpec("transcript", List(
          Elem.Tok("transcript"), Elem.NameLeaf, Elem.Tok("{"),
          Elem.Star(Elem.Cat("step")), Elem.Tok("}"))))),
      CategorySpec("step", List(
        ConstructorSpec("lang", List(Elem.Tok("lang"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("roundtrip", List(Elem.Tok("roundtrip"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("eval", List(Elem.Tok("eval"), Elem.StrLeaf, Elem.Tok("expect"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("delta", List(Elem.Tok("delta"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("claim", List(Elem.Tok("claim"), Elem.NameLeaf, Elem.StrLeaf, Elem.Tok("expect"), Elem.StrLeaf, Elem.Tok(";"))),
        ConstructorSpec("publish", List(Elem.Tok("publish"), Elem.NameLeaf, Elem.Tok(";"))),
        ConstructorSpec("fetch", List(Elem.Tok("fetch"), Elem.NameLeaf, Elem.Tok(";")))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("transcript", List(
        PrintSeg.Lit("transcript"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("{"), PrintSeg.Newline, PrintSeg.IndentIn,
        PrintSeg.SepFields(1, "\n"), PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
      PrintRule("lang", List(PrintSeg.Lit("lang"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("roundtrip", List(PrintSeg.Lit("roundtrip"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("eval", List(
        PrintSeg.Lit("eval"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space,
        PrintSeg.Lit("expect"), PrintSeg.Space, PrintSeg.StrField(1), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("delta", List(PrintSeg.Lit("delta"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("claim", List(
        PrintSeg.Lit("claim"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.StrField(1),
        PrintSeg.Space, PrintSeg.Lit("expect"), PrintSeg.Space, PrintSeg.StrField(2), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("publish", List(PrintSeg.Lit("publish"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";"))),
      PrintRule("fetch", List(PrintSeg.Lit("fetch"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(";")))),
    top = "transcript")

  final case class Report(name: String, steps: List[String]):
    def render: String = (s"transcript '$name':" :: steps.map("  ✓ " + _)).mkString("\n")

  /** Interpret a transcript against a working directory. `packs` is the
    * language registry (domain packs stay out of the surface layer — they are
    * injected by callers, §4.11).
    */
  def run(src: String, packs: Map[String, ComposedLanguage], workDir: Path): Either[String, Report] =
    Parser.parse(grammar, src).flatMap {
      case Cst.Node("transcript", List(Cst.Leaf(name), Cst.Node("list", steps))) =>
        runSteps(name, steps, packs, workDir)
      case other => Left(s"not a transcript: ${other.render}")
    }

  private def runSteps(name: String, steps: List[Cst],
                       packs: Map[String, ComposedLanguage], workDir: Path): Either[String, Report] =
    var lang: Option[ComposedLanguage] = None
    var module = Module(Nil)
    val authority = Keypair.dev("dev-authority")
    def authorities = Map(authority.name -> authority.publicBytes)
    val log = List.newBuilder[String]

    def need: Either[String, ComposedLanguage] = lang.toRight("no language selected (use `lang NAME ;` first)")
    def parseIn(l: ComposedLanguage, s: String): Either[String, Cst] = Parser.parse(l.grammar, s)

    val result = steps.foldLeft[Either[String, Unit]](Right(())) { (acc, step) =>
      acc.flatMap { _ =>
        step match
          case Cst.Node("lang", List(Cst.Leaf(n))) =>
            packs.get(n).toRight(s"unknown language pack '$n' (registered: ${packs.keys.mkString(", ")})")
              .map { l => lang = Some(l); module = Module(Nil); log += s"lang $n (${l.digest.short})" }
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
          case Cst.Node("publish", List(Cst.Leaf(branch))) =>
            need.flatMap { l =>
              val node = Node(workDir.resolve("nodeA"))
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
                .map(b => log += s"published $branch at block ${b.digest.short} root ${b.stateRoot.short}")
            }
          case Cst.Node("fetch", List(Cst.Leaf(branch))) =>
            val a = Node(workDir.resolve("nodeA"))
            val b = Node(workDir.resolve("nodeB"))
            for
              _ <- Sync.pull(a, b, authorities)
              st <- b.state(authorities)
              head <- st.heads.get(branch).toRight(s"branch '$branch' not on ledger")
              art <- b.cas.get(head)
            yield log += s"fetched $branch head ${art.digest.short} on second node"
          case other => Left(s"unknown transcript step: ${other.render}")
      }
    }
    result.map(_ => Report(name, log.result()))

/** Generic CLI (S6): hash / put / get / canon over a disk CAS, plus
  * transcript running with an injected pack registry.
  */
object Cli:
  def main(args: List[String], packs: Map[String, ComposedLanguage]): Either[String, String] =
    val casDir = Path.of(sys.env.getOrElse("CAIRN_HOME", ".cas"))
    def cas = DiskCas(casDir)
    args match
      case List("hash", file) =>
        Right(Digest.ofBytes(Files.readAllBytes(Path.of(file))).hex)
      case List("put", file) =>
        val bs = Files.readAllBytes(Path.of(file))
        Right(cas.putBytes(bs).hex)
      case List("get", hex) =>
        Digest.parse(hex).flatMap(d => cas.getBytes(d)).map(bs => new String(bs, "UTF-8"))
      case List("canon", file) =>
        // normalize a stored artifact: decode + re-encode canonical bytes
        val bs = Files.readAllBytes(Path.of(file))
        Canon.decode(bs).map(c => Digest.of(c).hex)
      case List("transcript", file) =>
        val src = Files.readString(Path.of(file))
        val work = Files.createTempDirectory("cairn-transcript")
        Transcript.run(src, packs, work).map(_.render)
      case _ =>
        Left("usage: cairn [hash|put|get|canon|transcript] <arg>")
