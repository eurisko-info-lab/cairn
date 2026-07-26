package cairn.tests

import cairn.kernel.*
import cairn.core.*

/** Canon round-trip coverage for every [[ChangeModel]] type. `ChangeOpDef`/
  * `ChangeModel` deliberately exclude `printSegs` from canon (purely
  * presentational surface syntax, same reason `Fragment.grammar` is excluded
  * from `Fragment.canon` — see `fromCanon` there, which always comes back
  * with an empty placeholder rather than the original grammar). So those two
  * types are checked at the canon level (`fromCanon(x.canon).canon ==
  * x.canon`) rather than full case-class equality; every other type here has
  * no excluded fields and is checked with full `==` round-trips.
  */
class ChangeModelCanonSuite extends munit.FunSuite:

  test("ChangeParamKind: canon round-trip, all cases"):
    for k <- List(ChangeParamKind.NameK, ChangeParamKind.TermK, ChangeParamKind.PathK, ChangeParamKind.FootprintK) do
      assertEquals(ChangeParamKind.fromCanon(k.canon), k)

  test("ChangeParam: canon round-trip, with and without precedingToken"):
    val a = ChangeParam("name", ChangeParamKind.NameK)
    val b = ChangeParam("term", ChangeParamKind.TermK, Some("="))
    assertEquals(ChangeParam.fromCanon(a.canon), a)
    assertEquals(ChangeParam.fromCanon(b.canon), b)

  test("ValueRef: canon round-trip, Param and Bound"):
    val a = ValueRef.Param(0)
    val b = ValueRef.Bound("x")
    assertEquals(ValueRef.fromCanon(a.canon), a)
    assertEquals(ValueRef.fromCanon(b.canon), b)

  test("BoolExpr: canon round-trip, all cases including nesting"):
    val exprs = List(
      BoolExpr.IsDefined(ValueRef.Param(0)),
      BoolExpr.Not(BoolExpr.IsDefined(ValueRef.Param(1))),
      BoolExpr.NamesEmpty(ValueRef.Bound("refs")),
      BoolExpr.NamesEqual(ValueRef.Param(2), ValueRef.Bound("actual")))
    for e <- exprs do assertEquals(BoolExpr.fromCanon(e.canon), e)

  test("SortRef: canon round-trip, both cases"):
    assertEquals(SortRef.fromCanon(SortRef.TopSort.canon), SortRef.TopSort)
    assertEquals(SortRef.fromCanon(SortRef.FromBound("sp").canon), SortRef.FromBound("sp"))

  test("ChangeQuery: canon round-trip, all cases"):
    val qs = List(
      ChangeQuery.ReferencingNames(ValueRef.Param(0)),
      ChangeQuery.ReadTerm(ValueRef.Param(0)),
      ChangeQuery.ResolveSemanticPath(ValueRef.Param(0), ValueRef.Param(1)))
    for q <- qs do assertEquals(ChangeQuery.fromCanon(q.canon), q)

  test("Mutation: canon round-trip, all cases"):
    val ms = List(
      Mutation.InsertDef(ValueRef.Param(0), ValueRef.Param(1)),
      Mutation.ReplaceDef(ValueRef.Param(0), ValueRef.Param(1)),
      Mutation.DeleteDef(ValueRef.Param(0)),
      Mutation.ReplaceSubtreeAt(ValueRef.Param(0), ValueRef.Param(1), ValueRef.Param(2)),
      Mutation.RenameOccurrences(ValueRef.Param(0), ValueRef.Param(1), ValueRef.Bound("actual")))
    for m <- ms do assertEquals(Mutation.fromCanon(m.canon), m)

  test("RejectionSpec: canon round-trip, all cases"):
    val rs = List(
      RejectionSpec.AlreadyDefinedR("add", ValueRef.Param(0)),
      RejectionSpec.NotDefinedR("replace", ValueRef.Param(0)),
      RejectionSpec.StillReferencedR(ValueRef.Param(0), ValueRef.Bound("refs")),
      RejectionSpec.FootprintMismatchR(ValueRef.Param(0), ValueRef.Param(2), ValueRef.Bound("actual")))
    for r <- rs do assertEquals(RejectionSpec.fromCanon(r.canon), r)

  test("ChangeStep: canon round-trip, all cases"):
    val steps = List(
      ChangeStep.Check(BoolExpr.IsDefined(ValueRef.Param(0)), RejectionSpec.NotDefinedR("add", ValueRef.Param(0))),
      ChangeStep.CheckTerm(ValueRef.Param(1), SortRef.TopSort, ValueRef.Param(0)),
      ChangeStep.Bind("refs", ChangeQuery.ReferencingNames(ValueRef.Param(0))),
      ChangeStep.Mutate(Mutation.DeleteDef(ValueRef.Param(0))))
    for s <- steps do assertEquals(ChangeStep.fromCanon(s.canon), s)

  test("FootprintExpr: canon round-trip, all cases including Union nesting"):
    val fs = List(
      FootprintExpr.NameOf(0),
      FootprintExpr.NamesOf(2),
      FootprintExpr.Union(List(FootprintExpr.NameOf(0), FootprintExpr.NameOf(1), FootprintExpr.NamesOf(2))))
    for f <- fs do assertEquals(FootprintExpr.fromCanon(f.canon), f)

  test("InverseArg: canon round-trip, all cases"):
    val args = List(InverseArg.Copy(0), InverseArg.LookupOld(0), InverseArg.SubtreeAtOld(0, 1))
    for a <- args do assertEquals(InverseArg.fromCanon(a.canon), a)

  test("InverseExpr: canon round-trip"):
    val inv = InverseExpr("remove", List(InverseArg.Copy(0)))
    assertEquals(InverseExpr.fromCanon(inv.canon), inv)

  test("ChangeOpDef: canon-level round-trip for every ChangeModel.default operation (printSegs excluded)"):
    for op <- ChangeModel.default.operations do
      assertEquals(ChangeOpDef.fromCanon(op.canon).canon, op.canon)

  test("ChangeModel: canon-level round-trip for ChangeModel.default, order-independent"):
    val m = ChangeModel.default
    assertEquals(ChangeModel.fromCanon(m.canon).canon, m.canon)
    // Construction order must not affect the digest.
    val shuffled = ChangeModel(m.operations.reverse)
    assertEquals(shuffled.digest, m.digest)

  test("ChangeModel: has a stable artifact digest"):
    assertEquals(ChangeModel.default.digest, ChangeModel.default.digest)
