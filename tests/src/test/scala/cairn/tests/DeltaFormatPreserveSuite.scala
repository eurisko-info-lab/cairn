package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.examples.stlc.Stlc

/** Format-preserving ΔL apply (grammar-as-lens, part b): `Delta.applyPreservingFormat`
  * splices only the bytes an edit touches, built on the existing, independently
  * tested `Concrete.splice` (M7). Covers `replace`/`edit`/`add`; `remove`/`rename`
  * are explicitly unsupported (see Delta.scala doc comment) and checked as such.
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

  test("remove and rename are explicitly unsupported, not silently canonicalized"):
    val src = "a = true ;\n"
    val rm = parseChange("{ remove a ; }")
    val rn = parseChange("{ rename a to b footprint [ ] ; }")
    assert(Delta.applyPreservingFormat(Stlc.language, mg, src, rm)
      .swap.exists(_.contains("not yet supported for 'remove'")))
    assert(Delta.applyPreservingFormat(Stlc.language, mg, src, rn)
      .swap.exists(_.contains("not yet supported for 'rename'")))
