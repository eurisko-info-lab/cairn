package cairn.examples.riemann

import cairn.kernel.*
import cairn.workbench.*
import cairn.proof.Claim
import java.nio.file.{Files, Path}

/** Riemann Hypothesis pack — an open mathematical claim, not a parity item.
  *
  * Object language: [[languages/riemann.cairn]] (`fragment analytic provides
  * riemann`, standalone — no `requires`). Cairn's kernel is a decidable
  * syntactic term checker (§2b/L2); it has no business deciding continuous
  * complex analysis, so RH is formalized as a `cairn.proof.Claim` — proof-free
  * by construction (§2 Claim vs proof) — and projected to Lean referencing
  * mathlib's real `riemannZeta`. This pack never builds a `Theorem` or
  * `Certificate` for it: that absence is the honest statement "this is open."
  */
object Riemann:
  lazy val fragments: List[Fragment] = PackLoader.requireOwn("riemann")

  /** Standalone: composes on its own (no `requires` to satisfy). */
  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("riemann", fragments)

  lazy val language: ComposedLanguage = PackLoader.requireClosed("riemann")

  private def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)

  /** "For all s in C, (zeta(s) = 0 and 0 < Re(s) < 1) implies Re(s) = 1/2" —
    * the standard critical-strip formalization of the Riemann Hypothesis.
    */
  val riemannHypothesis: Cst =
    n("forallC", Cst.Leaf("s"),
      n("rimplies",
        n("rand",
          n("isZero", n("zetaOf", n("cvar", Cst.Leaf("s")))),
          n("rand",
            n("rlt", n("rzero"), n("reOf", n("cvar", Cst.Leaf("s")))),
            n("rlt", n("reOf", n("cvar", Cst.Leaf("s"))), n("rone")))),
        n("req", n("reOf", n("cvar", Cst.Leaf("s"))), n("rhalf"))))

  /** Proof-free claim (§2 Claim vs proof). Deliberately never wrapped in a
    * `Theorem` or `Certificate` anywhere in this pack — RH is an open
    * problem; Cairn formalizes the statement and stops there.
    */
  val riemannHypothesisClaim: Claim =
    Claim("riemann_hypothesis", riemannHypothesis, subject = language.digest)

  /** Small, dedicated Lean-subset projection (mirrors `rosetta.Rosetta.LeanPort`'s
    * discipline: grammar-as-data, round-trip verified before printing) — Lean is
    * the only host with real analytic-continuation machinery, so this is the
    * only projection target (no Scala/Haskell/Rust ports for an open conjecture).
    */
  object LeanPort:
    val grammar: GrammarSpec = GrammarSpec(
      name = "riemann-lean-subset",
      tokens = TokenSpec(
        keywords = List("def", "Prop", "riemannZeta", "re"),
        puncts = List("(", ")", ":", ":=", ".", "=", "<", "∧", "→", "∀", ",", "/"),
        lineComment = Some("--")),
      categories = List(
        CategorySpec("file", List(ConstructorSpec("file", List(Elem.Star(Elem.Cat("decl")))))),
        CategorySpec("decl", List(
          ConstructorSpec("claimDef", List(
            Elem.Tok("def"), Elem.NameLeaf, Elem.Tok(":"), Elem.Tok("Prop"), Elem.Tok(":="),
            Elem.Cat("prop"))))),
        CategorySpec("expr", List(
          ConstructorSpec("evar", List(Elem.NameLeaf)),
          ConstructorSpec("zetaCall", List(
            Elem.Tok("riemannZeta"), Elem.Tok("("), Elem.Cat("expr"), Elem.Tok(")"))))),
        CategorySpec("rexpr", List(
          ConstructorSpec("reCall", List(
            Elem.Tok("("), Elem.Cat("expr"), Elem.Tok(")"), Elem.Tok("."), Elem.Tok("re"))),
          // longer alternative first — PEG ordered choice would otherwise let
          // numLit's bare NumLeaf commit on "1" and strand the "/ 2" that follows
          ConstructorSpec("divLit", List(Elem.NumLeaf, Elem.Tok("/"), Elem.NumLeaf)),
          ConstructorSpec("numLit", List(Elem.NumLeaf)))),
        CategorySpec("prop", List(
          ConstructorSpec("eqZero", List(Elem.Cat("expr"), Elem.Tok("="), Elem.Cat("rexpr"))),
          ConstructorSpec("eqR", List(Elem.Cat("rexpr"), Elem.Tok("="), Elem.Cat("rexpr"))),
          ConstructorSpec("ltR", List(Elem.Cat("rexpr"), Elem.Tok("<"), Elem.Cat("rexpr"))),
          ConstructorSpec("andP", List(
            Elem.Tok("("), Elem.Cat("prop"), Elem.Tok("∧"), Elem.Cat("prop"), Elem.Tok(")"))),
          ConstructorSpec("implyP", List(
            Elem.Tok("("), Elem.Cat("prop"), Elem.Tok("→"), Elem.Cat("prop"), Elem.Tok(")"))),
          ConstructorSpec("forallP", List(
            Elem.Tok("∀"), Elem.NameLeaf, Elem.Tok(":"), Elem.Tok("ℂ"), Elem.Tok(","), Elem.Cat("prop")))))),
      precCategories = Nil,
      printRules = List(
        PrintRule("file", List(PrintSeg.SepFields(0, "\n"))),
        PrintRule("claimDef", List(
          PrintSeg.Lit("def"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
          PrintSeg.Space, PrintSeg.Lit("Prop"), PrintSeg.Space, PrintSeg.Lit(":="), PrintSeg.Space,
          PrintSeg.Field(1))),
        PrintRule("evar", List(PrintSeg.Field(0))),
        PrintRule("zetaCall", List(
          PrintSeg.Lit("riemannZeta"), PrintSeg.Space, PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Lit(")"))),
        PrintRule("reCall", List(
          PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Lit(")"), PrintSeg.Lit("."), PrintSeg.Lit("re"))),
        PrintRule("numLit", List(PrintSeg.Field(0))),
        PrintRule("divLit", List(
          PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("/"), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("eqZero", List(PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("eqR", List(PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("ltR", List(PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("<"), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("andP", List(
          PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("∧"), PrintSeg.Space,
          PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("implyP", List(
          PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("→"), PrintSeg.Space,
          PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("forallP", List(
          PrintSeg.Lit("∀"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
          PrintSeg.Space, PrintSeg.Lit("ℂ"), PrintSeg.Lit(","), PrintSeg.Space, PrintSeg.Field(1)))),
      top = "file")

    private def toExpr(t: Cst): Cst = t match
      case Cst.Node("cvar", List(Cst.Leaf(x))) => Cst.node("evar", Cst.Leaf(x))
      case Cst.Node("zetaOf", List(inner))     => Cst.node("zetaCall", toExpr(inner))
      case other => throw CodecError(s"not a riemann term: ${other.render}")

    private def toRExpr(r: Cst): Cst = r match
      case Cst.Node("reOf", List(t)) => Cst.node("reCall", toExpr(t))
      case Cst.Node("rzero", _)      => Cst.node("numLit", Cst.Leaf("0"))
      case Cst.Node("rone", _)       => Cst.node("numLit", Cst.Leaf("1"))
      case Cst.Node("rhalf", _)      => Cst.node("divLit", Cst.Leaf("1"), Cst.Leaf("2"))
      case other => throw CodecError(s"not a riemann real expr: ${other.render}")

    private def toProp(p: Cst): Cst = p match
      case Cst.Node("isZero", List(t))      => Cst.node("eqZero", toExpr(t), Cst.node("numLit", Cst.Leaf("0")))
      case Cst.Node("req", List(a, b))      => Cst.node("eqR", toRExpr(a), toRExpr(b))
      case Cst.Node("rlt", List(a, b))      => Cst.node("ltR", toRExpr(a), toRExpr(b))
      case Cst.Node("rand", List(a, b))     => Cst.node("andP", toProp(a), toProp(b))
      case Cst.Node("rimplies", List(a, b)) => Cst.node("implyP", toProp(a), toProp(b))
      case Cst.Node("forallC", List(Cst.Leaf(x), body)) => Cst.node("forallP", Cst.Leaf(x), toProp(body))
      case other => throw CodecError(s"not a riemann prop: ${other.render}")

    /** Round-trip-verified Lean text. `def ... : Prop`, never `theorem ...
      * := by sorry` — a bare `def` asserts nothing, which is the honest
      * rendering of a proof-free Claim (a `sorry`-theorem would falsely
      * claim "provable, proof deferred").
      */
    def emit(claim: Claim): Either[String, String] =
      val declCst = Cst.node("claimDef", Cst.Leaf(claim.name), toProp(claim.statement))
      val fileCst = Cst.node("file", Cst.Node("list", List(declCst)))
      for
        _ <- RoundTrip.check(grammar, fileCst)
        text <- Printer.print(grammar, fileCst)
      yield
        s"""-- generated by cairn examples riemann pack (artifact ${claim.artifact.digest.short})
           |-- CLAIM (unproven, open problem): this formalizes the STATEMENT of the
           |-- Riemann Hypothesis only. It is a `def : Prop`, not a `theorem` — Cairn
           |-- never asserts this holds (see CAIRN-PROMPT.md §2, "Claim vs proof").
           |-- No proof term, no `sorry`, no certificate exists anywhere in this pack.
           |-- Not built against real mathlib here (no network / multi-GB dependency in
           |-- this repo's tests) — provided as a mathlib-referencing artifact for a
           |-- separately-provisioned `lake build` to elaborate.
           |import Mathlib.NumberTheory.LSeries.RiemannZeta
           |import Mathlib.Analysis.SpecialFunctions.Complex.Circle
           |
           |namespace Riemann
           |
           |$text
           |
           |end Riemann
           |""".stripMargin

  /** Obligations manifest entry (mirrors `rosetta.Scaffold.obligationsManifest`'s
    * shape, keyed on `Claim`/status instead of Rosetta `theorems`): `kind =
    * "claim"` and `status = "open"` are the machine-readable form of the same
    * honesty the generated Lean text states in its banner comment.
    */
  def obligationsManifest(claim: Claim, leanFile: String): Either[String, String] =
    val entry = Cst.node("entry",
      Cst.node("claim", Cst.Leaf(claim.name)),
      Cst.node("kind", Cst.Leaf("claim")),
      Cst.node("status", Cst.Leaf("open")),
      Cst.node("host", Cst.Leaf("lean")),
      Cst.node("file", Cst.Leaf(leanFile)),
      Cst.node("artifact", Cst.Leaf(claim.artifact.digest.hex)))
    JsonSurface.encode(Cst.node("obligations", Cst.Node("list", List(entry))))

  def writeArtifacts(dir: Path): Either[String, (Path, Path)] =
    for
      leanText <- LeanPort.emit(riemannHypothesisClaim)
      manifest <- obligationsManifest(riemannHypothesisClaim, "Riemann.lean")
    yield
      Files.createDirectories(dir)
      val leanF = dir.resolve("Riemann.lean")
      val manF = dir.resolve("obligations.json")
      Files.writeString(leanF, leanText)
      Files.writeString(manF, manifest)
      (leanF, manF)
