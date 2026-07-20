package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.examples.stlc.Stlc

/** Delta.applyTyped: typed Rejection reasons for the one gate that's ΔL
  * apply, scoped rather than a kernel-wide Either[String,_] migration.
  * `apply`'s public string contract is unchanged — Rejection.render must
  * reproduce it exactly (checked directly below), since apply is now a
  * one-line wrapper around applyTyped rather than a separate implementation.
  */
class DeltaApplyTypedSuite extends munit.FunSuite:
  private val dl = Delta.deltaOf(Stlc.language).fold(e => fail(e.map(_.render).mkString), identity)
  private def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  test("applyTyped and apply agree exactly: apply's string is Rejection.render"):
    val base = Module(List("a" -> Cst.node("true")))
    val bad = parseChange("{ add a = false ; }")
    val typedErr = Delta.applyTyped(Stlc.language, base, bad).swap.toOption.get
    val stringErr = Delta.apply(Stlc.language, base, bad).swap.toOption.get
    assertEquals(typedErr.render, stringErr)

  test("AlreadyDefined(add) vs AlreadyDefined(rename-target) render differently, matching apply's historical text"):
    val base = Module(List("a" -> Cst.node("true"), "b" -> Cst.node("false")))
    val addTaken = Delta.applyTyped(Stlc.language, base, parseChange("{ add a = true ; }"))
    assertEquals(addTaken, Left(Delta.Rejection.AlreadyDefined("add", "a")))
    assertEquals(Delta.Rejection.AlreadyDefined("add", "a").render, "ΔL add: 'a' already defined (use replace)")

    val renameTaken = Delta.applyTyped(Stlc.language, base, parseChange("{ rename a to b footprint [ ] ; }"))
    assertEquals(renameTaken, Left(Delta.Rejection.AlreadyDefined("rename-target", "b")))
    assertEquals(Delta.Rejection.AlreadyDefined("rename-target", "b").render, "ΔL rename: target 'b' already defined")

  test("NotDefined carries the op so replace/remove/edit/rename are distinguishable, not just stringly"):
    val base = Module(List("a" -> Cst.node("true")))
    assertEquals(Delta.applyTyped(Stlc.language, base, parseChange("{ replace z = true ; }")),
      Left(Delta.Rejection.NotDefined("replace", "z")))
    assertEquals(Delta.applyTyped(Stlc.language, base, parseChange("{ remove z ; }")),
      Left(Delta.Rejection.NotDefined("remove", "z")))
    assertEquals(Delta.applyTyped(Stlc.language, base, parseChange("{ rename z to y footprint [ ] ; }")),
      Left(Delta.Rejection.NotDefined("rename", "z")))

  test("StillReferenced carries the referencing names as a real Set, not a pre-joined string"):
    val base = Module(List(
      "x" -> Cst.node("true"),
      "user" -> Cst.node("var", Cst.Leaf("x"))))
    val result = Delta.applyTyped(Stlc.language, base, parseChange("{ remove x ; }"))
    assertEquals(result, Left(Delta.Rejection.StillReferenced("x", Set("user"))))

  test("FootprintMismatch carries declared vs actual as real Sets"):
    val base = Module(List(
      "x" -> Cst.node("true"),
      "user" -> Cst.node("var", Cst.Leaf("x"))))
    val result = Delta.applyTyped(Stlc.language, base, parseChange("{ rename x to y footprint [ ] ; }"))
    assertEquals(result, Left(Delta.Rejection.FootprintMismatch("x", Set.empty, Set("user"))))

  test("Malformed covers both a bad change term and a bad changeset shape"):
    val base = Module(Nil)
    val notAChange = Cst.node("bogus", Cst.Leaf("x"))
    assert(Delta.applyTyped(Stlc.language, base, notAChange).swap.exists {
      case Delta.Rejection.Malformed(d) => d.startsWith("not a ΔL changeset")
      case _ => false
    })
