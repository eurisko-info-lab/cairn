package cairn.examples.quicksort

import cairn.kernel.Cst
import cairn.core.*

/** RosettaQuickSort pack (S34, §5): one semantic artifact, claims about
  * sortedness and permutation, projected to ≥2 ports (Scala tested, Lean
  * skeleton with theorem obligations).
  */
object QuickSort:
  private def call(f: String, args: Cst*): Cst = Cst.node("rcall", (Cst.Leaf(f) +: args)*)
  private def v(x: String): Cst = Cst.node("rvar", Cst.Leaf(x))

  /** quicksort xs =
    *   if isEmpty xs then [] else
    *     append (quicksort (filterLt (hd xs) (tl xs)))
    *            (cons (hd xs) (quicksort (filterGe (hd xs) (tl xs))))
    */
  val module: RosettaModule = RosettaModule(
    name = "quicksort",
    defs = List(RDef(
      name = "quicksort",
      params = List("xs" -> RType.RListInt),
      ret = RType.RListInt,
      body = Cst.node("rif",
        call("isEmpty", v("xs")),
        Cst.node("rnil"),
        call("append",
          call("quicksort", call("filterLt", call("hd", v("xs")), call("tl", v("xs")))),
          call("cons", call("hd", v("xs")),
            call("quicksort", call("filterGe", call("hd", v("xs")), call("tl", v("xs"))))))))),
    theorems = List(
      RTheorem("quicksort_sorted",
        Cst.node("rforall", Cst.Leaf("xs"), Cst.node("rsorted", call("quicksort", v("xs"))))),
      RTheorem("quicksort_perm",
        Cst.node("rforall", Cst.Leaf("xs"), Cst.node("rperm", v("xs"), call("quicksort", v("xs")))))))
