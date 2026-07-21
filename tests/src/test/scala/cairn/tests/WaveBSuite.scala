package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** Wave B acceptance (M6–M12). */
class WaveBSuite extends munit.FunSuite:

  // ---- M6: layout combinators ----

  /** Juxtaposition-application STLC surface: `f x y` instead of `((f x) y)`. */
  val juxtaGrammar: GrammarSpec = GrammarSpec(
    name = "stlc-juxta",
    tokens = TokenSpec(List("fun", "true", "false", "Bool"), List("(", ")", ":", ".", "->"), Some("--")),
    categories = List(
      CategorySpec("term", List(
        ConstructorSpec("appRun", List(Elem.Run("atom"))))),
      CategorySpec("atom", List(
        ConstructorSpec("lam", List(
          Elem.Tok("fun"), Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("type"), Elem.Tok("."), Elem.Cat("term"))),
        ConstructorSpec("true", List(Elem.Tok("true"))),
        ConstructorSpec("false", List(Elem.Tok("false"))),
        ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("term"), Elem.Tok(")"))),
        ConstructorSpec("var", List(Elem.NameLeaf)))),
      CategorySpec("typeAtom", List(
        ConstructorSpec("tyBool", List(Elem.Tok("Bool"))),
        ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("type"), Elem.Tok(")")))))),
    precCategories = List(PrecCategory("type", "typeAtom", List(InfixOp("->", "arrow", 1, true)))),
    printRules = List(
      PrintRule("appRun", List(PrintSeg.SepFields(0, " "))),
      PrintRule("lam", List(
        PrintSeg.Lit("fun"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
        PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit("."), PrintSeg.Space,
        PrintSeg.Lit("("), PrintSeg.Field(2), PrintSeg.Lit(")"))),
      PrintRule("true", List(PrintSeg.Lit("true"))),
      PrintRule("false", List(PrintSeg.Lit("false"))),
      PrintRule("var", List(PrintSeg.Field(0))),
      PrintRule("tyBool", List(PrintSeg.Lit("Bool")))),
    top = "term")

  test("M6: juxtaposition application parses n-ary and round-trips"):
    val parsed = Parser.parse(juxtaGrammar, "f x true").toOption.get
    parsed match
      case Cst.Node("appRun", List(Cst.Node("list", items))) => assertEquals(items.length, 3)
      case other => fail(s"unexpected: ${other.render}")
    RoundTrip.check(juxtaGrammar, parsed).fold(e => fail(e), identity)

  test("M6: run stops at line boundary unless indented past floor"):
    val src = "f x\ntrue"
    // `true` starts a NEW top-level... but top is a single term: parse should
    // stop the run at the newline (col 1 not past floor) and then fail on trailing input
    assert(Parser.parse(juxtaGrammar, src).isLeft)
    // indented continuation joins the run
    val parsed = Parser.parse(juxtaGrammar, "f x\n   true").toOption.get
    parsed match
      case Cst.Node("appRun", List(Cst.Node("list", items))) => assertEquals(items.length, 3)
      case other => fail(s"unexpected: ${other.render}")

  /** Offside-rule block demo: `let` bindings at a shared column. */
  val blockGrammar: GrammarSpec = GrammarSpec(
    name = "block-demo",
    tokens = TokenSpec(List("let", "in"), List("="), Some("--")),
    categories = List(
      CategorySpec("letE", List(
        ConstructorSpec("let", List(Elem.Tok("let"), Elem.Block("binding"), Elem.Tok("in"), Elem.NameLeaf)))),
      CategorySpec("binding", List(
        ConstructorSpec("bind", List(Elem.NameLeaf, Elem.Tok("="), Elem.NameLeaf))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("let", List(
        PrintSeg.Lit("let"), PrintSeg.Newline, PrintSeg.IndentIn, PrintSeg.SepFields(0, "\n"),
        PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("in"), PrintSeg.Space, PrintSeg.Field(1))),
      PrintRule("bind", List(PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1)))),
    top = "letE")

  test("M6: offside block collects items at the first item's column"):
    val src =
      """let
        |  a = x
        |  b = y
        |in a""".stripMargin
    val parsed = Parser.parse(blockGrammar, src).toOption.get
    parsed match
      case Cst.Node("let", List(Cst.Node("list", bindings), Cst.Leaf("a"))) =>
        assertEquals(bindings.length, 2)
      case other => fail(s"unexpected: ${other.render}")
    RoundTrip.check(blockGrammar, parsed).fold(e => fail(e), identity)
    // `in` at column 1 (not the binding column) correctly terminates the block

  test("M6: restOfLine captures verbatim free-form content"):
    val g = GrammarSpec("rol", TokenSpec(List("note"), Nil, None, restOfLineMarkers = List("note:")),
      List(CategorySpec("doc", List(
        ConstructorSpec("doc", List(Elem.Star(Elem.Cat("line")))))),
        CategorySpec("line", List(
          ConstructorSpec("note", List(Elem.RestOfLine))))),
      Nil,
      List(PrintRule("doc", List(PrintSeg.SepFields(0, "\n"))),
        PrintRule("note", List(PrintSeg.Lit("note:"), PrintSeg.Field(0)))),
      "doc")
    val src = "note: free form!! *anything* here\nnote: second"
    val parsed = Parser.parse(g, src).toOption.get
    parsed match
      case Cst.Node("doc", List(Cst.Node("list", List(
        Cst.Node("note", List(Cst.Leaf(a))), Cst.Node("note", List(Cst.Leaf(b))))))) =>
        assertEquals(a, " free form!! *anything* here")
        assertEquals(b, " second")
      case other => fail(s"unexpected: ${other.render}")
    RoundTrip.check(g, parsed).fold(e => fail(e), identity)

  // ---- M7: trivia preservation ----

  test("M7: comment-bearing source reproduces byte-for-byte"):
    val src = "-- leading comment\nfun x : Bool .  x  -- trailing\n"
    val out = Parser.parseFull(Stlc.language.grammar, src).toOption.get
    assertEquals(Concrete.printExact(src, out), src)

  test("M7: splice replaces one subtree, preserves all other bytes"):
    val src = "-- keep me\nif true then x else  y -- and me\n"
    val g = Stlc.language.grammar
    val out = Parser.parseFull(g, src).toOption.get
    // replace the `x` (then-branch) with `false`
    val thenBranch = out.cst match
      case Cst.Node("if", List(_, t, _)) => t
      case other => fail(s"unexpected: ${other.render}")
    val spliced = Concrete.splice(g, src, out, thenBranch, Stlc.fls).toOption.get
    assertEquals(spliced, "-- keep me\nif true then false else  y -- and me\n")
    // and the splice still parses
    assert(Parser.parse(g, spliced).isRight)

  // ---- M8: diagnostics ----

  test("M8: rich errors carry source excerpt, caret, and expected set"):
    val err = Parser.parse(Stlc.language.grammar, "fun x : Bool .").swap.toOption.get
    assert(err.contains("expected"), err)
    assert(err.contains("^"), err)
    assert(err.linesIterator.size >= 3, err)

  test("M8: spans recorded for every node"):
    val src = "if true then x else y"
    val out = Parser.parseFull(Stlc.language.grammar, src).toOption.get
    val span = out.spans.get(out.cst)
    assertEquals(span, Some((0, out.tokens.length - 1)))

  // ---- M9: grammar lints ----

  test("M9: prefix-shadowing detected"):
    val bad = GrammarSpec("bad", TokenSpec(Nil, List("(", ")"), None),
      List(CategorySpec("e", List(
        ConstructorSpec("short", List(Elem.Tok("("), Elem.NameLeaf, Elem.Tok(")"))),
        ConstructorSpec("long", List(Elem.Tok("("), Elem.NameLeaf, Elem.Tok(")"), Elem.NameLeaf))))),
      Nil, List(PrintRule("short", List(PrintSeg.Field(0))), PrintRule("long", List(PrintSeg.Field(0), PrintSeg.Field(1)))), "e")
    assert(GrammarLint.errors(bad).exists(_.msg.contains("shadowed by earlier prefix")))

  test("M9: duplicate alternative detected"):
    val bad = GrammarSpec("bad", TokenSpec(Nil, Nil, None),
      List(CategorySpec("e", List(
        ConstructorSpec("a", List(Elem.NameLeaf)),
        ConstructorSpec("b", List(Elem.NameLeaf))))),
      Nil, List(PrintRule("a", List(PrintSeg.Field(0))), PrintRule("b", List(PrintSeg.Field(0)))), "e")
    assert(GrammarLint.errors(bad).exists(_.msg.contains("unreachable")))

  test("M9: print/parse field arity disagreement detected"):
    val bad = GrammarSpec("bad", TokenSpec(Nil, Nil, None),
      List(CategorySpec("e", List(ConstructorSpec("a", List(Elem.NameLeaf))))),
      Nil, List(PrintRule("a", List(PrintSeg.Field(0), PrintSeg.Field(3)))), "e")
    assert(GrammarLint.errors(bad).exists(_.msg.contains("field 3")))

  test("M9: unknown category reference detected"):
    val bad = GrammarSpec("bad", TokenSpec(Nil, Nil, None),
      List(CategorySpec("e", List(ConstructorSpec("a", List(Elem.Cat("ghost")))))),
      Nil, List(PrintRule("a", List(PrintSeg.Field(0)))), "e")
    assert(GrammarLint.errors(bad).exists(_.msg.contains("unknown category 'ghost'")))

  test("M9: all shipped grammars pass lint clean; compose enforces it"):
    for g <- List(Stlc.language.grammar, Delta.deltaOf(Stlc.language).toOption.get.grammar,
                  Meta.grammar, JsonSurface.grammar) do
      assertEquals(GrammarLint.errors(g), Nil, s"lint failures in ${g.name}")
    // a fragment pair whose merged grammar lints dirty must fail composition
    val evil = Stlc.base.copy(name = "zz-evil", provides = List("evil"), grammar = GrammarPart(
      categories = List(CategorySpec("term", List(
        ConstructorSpec("varDup", List(Elem.NameLeaf))))),
      printRules = List(PrintRule("varDup", List(PrintSeg.Field(0), PrintSeg.Field(9))))))
    Compose.compose("bad", Stlc.fragments :+ evil) match
      case Left(errs) => assert(errs.exists(_.path.startsWith("grammar/lint/")), errs.map(_.render).mkString("\n"))
      case Right(_)   => fail("lint error not enforced by compose")

  // ---- M10: packrat + incremental ----

  test("M10: pathological backtracking becomes linear with memoization"):
    // grammar where naive PEG re-parses the same category exponentially:
    // e := (a e) | (b e) | n — deep nesting with failing first alternatives
    val g = GrammarSpec("path", TokenSpec(List("a", "b", "n"), List("(", ")"), None),
      List(CategorySpec("e", List(
        ConstructorSpec("ea", List(Elem.Tok("("), Elem.Tok("a"), Elem.Cat("e"), Elem.Tok(")"))),
        ConstructorSpec("eb", List(Elem.Tok("("), Elem.Tok("b"), Elem.Cat("e"), Elem.Tok(")"))),
        ConstructorSpec("en", List(Elem.Tok("n")))))),
      Nil, List(
        PrintRule("ea", List(PrintSeg.Lit("(a "), PrintSeg.Field(0), PrintSeg.Lit(")"))),
        PrintRule("eb", List(PrintSeg.Lit("(b "), PrintSeg.Field(0), PrintSeg.Lit(")"))),
        PrintRule("en", List(PrintSeg.Lit("n")))), "e")
    val depth = 60
    val src = "(b " * depth + "n" + ")" * depth
    val out = Parser.parseFull(g, src).toOption.get
    // without memo, ea-then-eb retries double the work at every level (2^60);
    // with memo the parse finishes and step count stays linear-ish
    assert(out.steps < depth * 20, s"steps = ${out.steps}")

  test("M10: incremental reparse of a 1-char edit does bounded fresh work"):
    val ip = IncrementalParser(Stlc.language.grammar)
    val defs = (1 to 40).map(i => s"fun x$i : Bool . x$i").mkString("(", " ", ")")
    // build a big nested application term
    val src = (1 to 40).map(i => s"fun v$i : Bool . ").mkString + "true"
    val r1 = ip.parse(src).toOption.get
    val r2 = ip.parse(src.replace("true", "false")).toOption.get
    assertEquals(r2.out.cst != r1.out.cst, true)
    assert(r2.freshSteps <= r1.freshSteps, s"fresh ${r2.freshSteps} vs full ${r1.freshSteps}")

  // ---- M11: error recovery ----

  test("M11: 3 seeded errors yield 3 diagnostics and a parseable remainder"):
    val lang = Stlc.language
    val dl = Delta.deltaOf(lang).toOption.get
    val g = dl.grammar.copy(syncTokens = List(";"))
    val src = "{ add good1 = true ; add bad1 == true ; add good2 = false ; replace ; add bad3 true true true ; add good3 = true ; }"
    val out = Parser.parseRecovering(g, src).toOption.get
    assertEquals(out.diagnostics.length, 3)
    // good items survived around the errors
    val items = out.cst match
      case Cst.Node(t, List(Cst.Node("list", xs))) => xs
      case other => fail(s"unexpected: ${other.render}")
    assertEquals(items.count { case Cst.Node("$error", _) => true; case _ => false }, 3)
    assertEquals(items.count { case Cst.Node(t, _) if t.startsWith("add:") => true; case _ => false }, 3)

  // ---- M12: non-text surfaces ----

  test("M12: JSON surface round-trips any term"):
    for t <- List(Stlc.idBool, Stlc.churchTrue, Stlc.node3if,
                  Stlc.app1(Stlc.churchTrue, Stlc.v("free"))) do
      val json = JsonSurface.encode(t).toOption.get
      assertEquals(JsonSurface.decode(json), Right(t))
    // string escaping survives
    val tricky = Cst.node("var", Cst.Leaf("we\"ird\\name\nline"))
    assertEquals(JsonSurface.decode(JsonSurface.encode(tricky).toOption.get), Right(tricky))

  test("M12: surface registry exposes text/json/canon per language"):
    val surfaces = Surfaces.forLanguage(Stlc.language)
    assertEquals(surfaces.keySet, Set("text", "json", "canon"))
    for (name, s) <- surfaces do
      val enc = s.encode(Stlc.idBool).toOption.get
      assertEquals(s.decode(enc), Right(Stlc.idBool), s"surface $name failed")
