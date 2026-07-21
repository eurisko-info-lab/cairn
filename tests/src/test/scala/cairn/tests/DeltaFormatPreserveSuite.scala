package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** Format-preserving grammar-as-lens `put` + ΔL apply.
  *
  * [[RoundTrip.put]] / [[Concrete.put]] edit one spanned subtree and keep every
  * byte outside the span. Without `put`, a whole-tree [[Printer.print]] after
  * the same structural edit drops comments/spacing — the contrast test below
  * fails if `put` regresses to canonical reprint. Module-level ΔL uses the same
  * primitive via [[Delta.applyPreservingFormat]] (`replace`/`edit`/`add`;
  * `remove`/`rename` explicitly unsupported — see Delta.scala).
  */
class DeltaFormatPreserveSuite extends munit.FunSuite:
  private val dl = Delta.deltaOf(Stlc.language).fold(e => fail(e.map(_.render).mkString), identity)
  private val mg = ModuleSurface.grammar(Stlc.language)
  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  /** The property that actually matters: format-preservation must never change
    * what the module MEANS, only its bytes. Compare against plain Delta.apply.
    */
  private def assertSameMeaning(originalSource: String, change: Cst, preservedResult: String): Unit =
    val origModule = ModuleSurface.toModule(Parser.parse(mg, originalSource).toOption.get).toOption.get
    val viaApply = Delta.apply(Stlc.language, origModule, change).toOption.get._1
    val viaPreserved = ModuleSurface.toModule(Parser.parse(mg, preservedResult).toOption.get).toOption.get
    assertEquals(viaPreserved.sorted.digest, viaApply.sorted.digest)

  test("RoundTrip.put preserves trivia outside the edited span; print does not"):
    val g = Stlc.language.grammar
    val src = "-- keep me\nif true then x else  y -- and me\n"
    val out = Parser.parseFull(g, src).fold(e => fail(e), identity)
    val thenBranch = out.cst match
      case Cst.Node("if", List(_, t, _)) => t
      case other => fail(s"unexpected: ${other.render}")
    val viaPut = RoundTrip.put(g, src, out, thenBranch, Stlc.fls).fold(e => fail(e), identity)
    assertEquals(viaPut, "-- keep me\nif true then false else  y -- and me\n")
    // Same structural edit via whole-tree print loses the surrounding trivia —
    // that is exactly why put exists.
    val edited = out.cst match
      case Cst.Node("if", List(c, _, e)) => Cst.node("if", c, Stlc.fls, e)
      case other => fail(s"unexpected: ${other.render}")
    val viaPrint = Printer.print(g, edited).fold(e => fail(e), identity)
    assert(!viaPrint.contains("-- keep me"), viaPrint)
    assert(!viaPrint.contains("-- and me"), viaPrint)

  test("replace preserves comments/formatting on every other def, only touches the target"):
    val src = "-- header comment\na = true ;\n-- b's own comment\nb = false ;\n"
    val change = parseChange("{ replace a = false ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assert(result.contains("-- header comment"), result)
    assert(result.contains("-- b's own comment\nb = false ;"), result)
    assert(result.contains("a = false ;"), result)
    assert(Parser.parse(mg, result).isRight, result)
    assertSameMeaning(src, change, result)

  test("edit-at-path touches only the targeted subterm, sibling text untouched"):
    val src = "-- c comment\nc = (f x) ; -- inline\n"
    val change = parseChange("{ edit c at [ 1 ] = y ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assertEquals(result, "-- c comment\nc = (f y) ; -- inline\n")
    assertSameMeaning(src, change, result)

  test("add appends after the original bytes, preserving them verbatim as a prefix"):
    val src = "-- a comment\na = true ;\n"
    val change = parseChange("{ add b = false ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assert(result.startsWith("-- a comment\na = true ;"), result)
    assert(result.contains("b = false ;"), result)
    assert(Parser.parse(mg, result).isRight, result)
    assertSameMeaning(src, change, result)

  test("add into an empty module"):
    val src = "  \n"
    val change = parseChange("{ add a = true ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assert(result.contains("a = true ;"), result)
    assert(Parser.parse(mg, result).isRight, result)
    assertSameMeaning(src, change, result)

  test("replace of an undefined name fails"):
    val src = "a = true ;\n"
    val change = parseChange("{ replace z = false ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change)
    assert(result.swap.exists(_.contains("not defined")), result.toString)

  test("add of an already-defined name fails"):
    val src = "a = true ;\n"
    val change = parseChange("{ add a = false ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change)
    assert(result.swap.exists(_.contains("already defined")), result.toString)

  test("remove deletes the def's own leading comment, leaves the next def's comment untouched"):
    val src = "-- header comment\na = true ;\n-- b's own comment\nb = false ;\n-- footer comment\nc = true ;\n"
    val change = parseChange("{ remove b ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assertEquals(result, "-- header comment\na = true ;\n-- footer comment\nc = true ;\n")
    assertSameMeaning(src, change, result)

  test("remove between blank-line-separated defs leaves a single blank line, not double"):
    val src = "a = true ;\n\nb = false ;\n\nc = true ;\n"
    val change = parseChange("{ remove b ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assertEquals(result, "a = true ;\n\nc = true ;\n")
    assertSameMeaning(src, change, result)

  test("remove the first def consumes its own leading trivia back to file start"):
    val src = "-- a's comment\na = true ;\nb = false ;\n"
    val change = parseChange("{ remove a ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    // "a"'s own leading comment is gone; the separator newline before "b" is
    // b's leading trivia, not a's — untouched, same rule as the middle-def
    // case (see the test above), so it survives as a leading blank line here.
    assertEquals(result, "\nb = false ;\n")
    assertSameMeaning(src, change, result)

  test("remove the last def leaves trailing trivia (EOF's leading) untouched"):
    val src = "a = true ;\n-- b's comment\nb = false ;\n"
    val change = parseChange("{ remove b ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assertEquals(result, "a = true ;\n")
    assertSameMeaning(src, change, result)

  test("remove of a name still referenced by another def fails (delegated to apply's validation)"):
    val src = "id = fun x : Bool . x ;\nuse = (id true) ;\n"
    val change = parseChange("{ remove id ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change)
    assert(result.swap.exists(_.contains("still referenced")), result.toString)

  test("remove of an undefined name fails"):
    val src = "a = true ;\n"
    val change = parseChange("{ remove z ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change)
    assert(result.isLeft, result.toString)

  test("rename touches the def's own name and every footprint reference, nothing else"):
    val src = "-- id's own comment\nid = fun x : Bool . x ;\n" +
      "-- use1 comment\nuse1 = (id true) ;\n-- use2 comment\nuse2 = (id false) ;\n"
    val change = parseChange("{ rename id to ident footprint [ use1 , use2 ] ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assert(result.contains("-- id's own comment\nident = fun x : Bool . x ;"), result)
    assert(result.contains("-- use1 comment\nuse1 = (ident true) ;"), result)
    assert(result.contains("-- use2 comment\nuse2 = (ident false) ;"), result)
    assert(!result.contains("(id true)") && !result.contains("(id false)"), result)
    assert(Parser.parse(mg, result).isRight, result)
    assertSameMeaning(src, change, result)

  test("rename with an empty footprint touches only the def's own name"):
    val src = "-- lone comment\na = true ;\n-- other comment\nb = false ;\n"
    val change = parseChange("{ rename a to renamed footprint [ ] ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change).fold(e => fail(e), identity)
    assertEquals(result, "-- lone comment\nrenamed = true ;\n-- other comment\nb = false ;\n")
    assertSameMeaning(src, change, result)

  test("rename of an undefined name fails"):
    val src = "a = true ;\n"
    val change = parseChange("{ rename z to y footprint [ ] ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change)
    assert(result.isLeft, result.toString)

  test("rename to an already-defined name fails (delegated to apply's validation)"):
    val src = "a = true ;\nb = false ;\n"
    val change = parseChange("{ rename a to b footprint [ ] ; }")
    val result = Delta.applyPreservingFormat(Stlc.language, mg, src, change)
    assert(result.isLeft, result.toString)
