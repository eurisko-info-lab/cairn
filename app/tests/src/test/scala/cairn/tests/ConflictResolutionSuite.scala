package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

class ConflictResolutionSuite extends munit.FunSuite:
  private val language = Stlc.language
  private val delta = Delta.deltaOf(language).toOption.get
  private val resolutionLanguage = ConflictDelta.deltaOf(language).toOption.get
  private val base = Module(List("a" -> Stlc.tru))

  private def change(source: String): Cst =
    Parser.parse(delta.grammar, source).fold(e => fail(e), identity)

  private def program(source: String): Cst =
    Parser.parse(resolutionLanguage.grammar, source).fold(e => fail(e), identity)

  private def conflict(left: Cst, right: Cst): Merge.Conflict =
    Merge.threeWay(language, base, left, right) match
      case Left(value) => value
      case Right(_)    => fail("expected conflict")

  private def hex(term: Cst): String =
    java.util.HexFormat.of().formatHex(Canon.encode(Cst.toCanon(term)))

  test("ΔConflict is a free parsed language with every resolution primitive"):
    val left = change("{ replace a = false ; }")
    val c = conflict(left, change("{ edit a at [] = fun x : Bool . x ; }"))
    val location = c.overlap.toList.sortBy(_.render).head.render
    val source =
      s"""{ compose-resolution "${hex(left)}"; replace-at "$location" with "${hex(Stlc.fls)}"; defer "$location"; split "$location"; accept-left; accept-right; }"""
    assert(Parser.parse(resolutionLanguage.grammar, source).isRight)

  test("accept-left produces an ordinary model- and domain-validated change with both causes"):
    val left = change("{ replace a = false ; }")
    val right = change("{ edit a at [] = fun x : Bool . x ; }")
    val c = conflict(left, right)
    var gateRuns = 0
    val gate = ModuleGate.host("resolution-domain") { module =>
      gateRuns += 1
      Either.cond(module.get("a").contains(Stlc.fls), (), "wrong side")
    }
    val policy = AcceptancePolicy.gated(gate)
    val constitution = AcceptanceConstitution.fromPolicy(policy, ChangeModel.default.digest)
    val resolved = ConflictDelta.resolve(
      language, base, c, left, right, program("{ accept-left; }"),
      ChangeModel.default, gate, constitution).fold(e => fail(e), identity)
    assertEquals(resolved.result.get("a"), Some(Stlc.fls))
    assertEquals(resolved.validatedChange.result, resolved.result.digest)
    assertEquals(resolved.unresolved, Nil)
    assertEquals(resolved.causalChanges.toSet, Set(c.changeA, c.changeB))
    assertEquals(gateRuns, 1)

  test("Studio conflict mode is a projection over the ordinary ΔConflict interpreter"):
    val left = change("{ replace a = false ; }")
    val right = change("{ edit a at [] = fun x : Bool . x ; }")
    val c = conflict(left, right)
    val workspace = StudioConflictWorkspace(language, base, c, left, right,
      AcceptanceConstitution.open(ChangeModel.default.digest))
    assertEquals(workspace.locations.toSet, c.overlap)
    val resolution = workspace.resolve(program("{ accept-left; }")).fold(e => fail(e), identity)
    assertEquals(resolution.result.get("a"), Some(Stlc.fls))
    assertEquals(resolution.causalChanges.toSet, Set(c.changeA, c.changeB))
    assertEquals(resolution.unresolved, Nil)

  test("defer and split remain explicit and a failing domain gate rejects resolution"):
    val left = change("{ replace a = false ; }")
    val right = change("{ edit a at [] = fun x : Bool . x ; }")
    val c = conflict(left, right)
    val refs = c.overlap.toList.sortBy(_.render)
    val operations = refs.zipWithIndex.map { (location, index) =>
      val op = if index == 0 then "defer" else "split"
      s"$op \"${location.render}\";"
    }.mkString("{ ", " ", " }")
    val partial = ConflictDelta.resolve(
      language, base, c, left, right, program(operations), ChangeModel.default,
      ModuleGate.passthrough, AcceptanceConstitution.open(ChangeModel.default.digest))
      .fold(e => fail(e), identity)
    assertEquals(partial.unresolved.map(_.location).toSet, c.overlap)
    assert(partial.unresolved.forall(_.disposition != ConflictDelta.Disposition.Pending))

    val rejecting = ModuleGate.host("never")(_ => Left("domain rejected"))
    val constitution = AcceptanceConstitution.fromPolicy(AcceptancePolicy.gated(rejecting), ChangeModel.default.digest)
    assert(ConflictDelta.resolve(
      language, base, c, left, right, program("{ accept-left; }"), ChangeModel.default,
      rejecting, constitution).left.exists(_.contains("domain rejected")))
