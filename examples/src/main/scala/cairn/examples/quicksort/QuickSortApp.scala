package cairn.examples.quicksort

import cairn.kernel.Cst
import cairn.rosetta.*

/** Rosetta sample app depth — on par with ROSETTA `QuickSortOrdEffects`
  * public entrypoints (`sortNatWithTrace` / `runSample`). Builds on the same
  * Ord-constrained quicksort artifact vocabulary; Nat ADT + traced sort +
  * sample `[3,1,2,1]` are first-class Rosetta declarations.
  *
  * Kept as a sibling of [[QuickSort2]] so existing golden digests stay stable.
  */
object QuickSortApp:
  private def call(f: String, args: Cst*): Cst = Cst.node("rcall", (Cst.Leaf(f) +: args)*)
  private def v(x: String): Cst = Cst.node("rvar", Cst.Leaf(x))
  private def n(i: Int): Cst = Cst.node("rnum", Cst.Leaf(i.toString))

  val sampleList: Cst =
    call("cons", n(3), call("cons", n(1), call("cons", n(2), call("cons", n(1), Cst.node("rnil")))))

  val quicksortBody: Cst = Cst.node("rif",
    call("isEmpty", v("xs")),
    Cst.node("rnil"),
    call("append",
      call("quicksort", call("filterLt", call("hd", v("xs")), call("tl", v("xs")))),
      call("cons", call("hd", v("xs")),
        call("quicksort", call("filterGe", call("hd", v("xs")), call("tl", v("xs")))))))

  val module: RosettaModule2 = RosettaModule2(
    name = "quicksortApp",
    datas = List(
      RDataV2("Peano", Nil, List(("Z", Nil), ("S", List(RTy.RVar("Peano"))))),
      RDataV2("Verdict", Nil, List(("Pass", Nil), ("Fail", Nil)))),
    effects = List(REffectV2("counter", List("tick"))),
    defs = List(
      RDefV2(
        name = "quicksort",
        typeParams = List("a"),
        constraints = List(("a", "Ord")),
        params = List("xs" -> RTy.RList(RTy.RVar("a"))),
        ret = RTy.RList(RTy.RVar("a")),
        body = quicksortBody),
      RDefV2(
        name = "sortNatWithTrace",
        typeParams = Nil,
        constraints = Nil,
        params = List("xs" -> RTy.RList(RTy.RInt)),
        ret = RTy.RList(RTy.RInt),
        body = Cst.node("rseq", call("tick"), call("quicksort", v("xs"))),
        effect = Some("counter")),
      RDefV2(
        name = "runSample",
        typeParams = Nil,
        constraints = Nil,
        params = Nil,
        ret = RTy.RList(RTy.RInt),
        body = call("quicksort", sampleList))),
    theorems = List(
      RTheorem("quicksort_sorted",
        Cst.node("rforall", Cst.Leaf("xs"), Cst.node("rsorted", call("quicksort", v("xs"))))),
      RTheorem("quicksort_perm",
        Cst.node("rforall", Cst.Leaf("xs"), Cst.node("rperm", v("xs"), call("quicksort", v("xs"))))),
      RTheorem("runSample_sorted",
        Cst.node("rforall", Cst.Leaf("xs"), Cst.node("rsorted", call("runSample"))))))
