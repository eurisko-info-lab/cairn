package cairn.examples.quicksort

import cairn.kernel.Cst
import cairn.rosetta.*

/** QuickSortOrdEffects (M30/M34): ONE semantic artifact — generic quicksort
  * over any `Ord a`, plus a counter-effect variant — with sortedness and
  * permutation obligations, projected to four hosts.
  */
object QuickSort2:
  private def call(f: String, args: Cst*): Cst = Cst.node("rcall", (Cst.Leaf(f) +: args)*)
  private def v(x: String): Cst = Cst.node("rvar", Cst.Leaf(x))

  val quicksortBody: Cst = Cst.node("rif",
    call("isEmpty", v("xs")),
    Cst.node("rnil"),
    call("append",
      call("quicksort", call("filterLt", call("hd", v("xs")), call("tl", v("xs")))),
      call("cons", call("hd", v("xs")),
        call("quicksort", call("filterGe", call("hd", v("xs")), call("tl", v("xs")))))))

  val module: RosettaModule2 = RosettaModule2(
    name = "quicksort2",
    datas = List(RDataV2("Verdict", Nil, List(("Pass", Nil), ("Fail", Nil)))),
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
        name = "countingQuicksort",
        typeParams = Nil,
        constraints = Nil,
        params = List("xs" -> RTy.RList(RTy.RInt)),
        ret = RTy.RList(RTy.RInt),
        body = Cst.node("rseq", call("tick"), call("quicksort", v("xs"))),
        effect = Some("counter"))),
    theorems = List(
      RTheorem("quicksort_sorted",
        Cst.node("rforall", Cst.Leaf("xs"), Cst.node("rsorted", call("quicksort", v("xs"))))),
      RTheorem("quicksort_perm",
        Cst.node("rforall", Cst.Leaf("xs"), Cst.node("rperm", v("xs"), call("quicksort", v("xs")))))))
