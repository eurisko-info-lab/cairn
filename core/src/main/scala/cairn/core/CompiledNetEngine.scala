package cairn.core

/** Compiled rule dispatch for interaction nets — the net-engine counterpart of
  * [[CompiledTreeEngine]]. [[NetEngine.ruleFor]] linearly scans `lang.rules`
  * (twice, for both orderings of the active pair) on every single step; this
  * precomputes a `(leftKind, rightKind) -> rule` index ONCE per [[NetLanguage]]
  * and replaces that scan with a map lookup. Pure dispatch optimization: same
  * rewrite semantics, same [[NetEngine.applyPair]] — [[NetEngine]] remains the
  * certifying reference; [[WaveESuite]] asserts agreement.
  */
final class CompiledNetEngine(lang: NetLanguage):
  /** `getOrElseUpdate` preserves [[NetEngine.ruleFor]]'s exact precedence:
    * every rule's forward (left, right) orientation is indexed before ANY
    * rule's backward (right, left) orientation is considered, so a forward
    * match always wins over a same-keyed backward one, and the first rule
    * in declaration order wins a same-keyed forward/forward tie — identical
    * to the original's "try forward across all rules, then fall back".
    */
  private val byKindPair: Map[(String, String), (NetRule, Boolean)] =
    val m = scala.collection.mutable.LinkedHashMap.empty[(String, String), (NetRule, Boolean)]
    for r <- lang.rules do m.getOrElseUpdate((r.left, r.right), (r, false))
    for r <- lang.rules if r.left != r.right do m.getOrElseUpdate((r.right, r.left), (r, true))
    m.toMap

  var visits = 0L // instrumentation for benchmarks, mirrors CompiledTreeEngine

  def ruleFor(net: Net, pair: (Int, Int)): Option[(NetRule, Int, Int)] =
    visits += 1
    val (la, ra) = pair
    val lk = net.agents(la).kind
    val rk = net.agents(ra).kind
    byKindPair.get((lk, rk)).map {
      case (rule, swapped) => if swapped then (rule, ra, la) else (rule, la, ra)
    }

  def step(net: Net): Either[String, Option[Net]] =
    NetEngine.activePairs(net).flatMap(ruleFor(net, _)).headOption match
      case None                     => Right(None)
      case Some((rule, l, r)) => NetEngine.applyPair(net, rule, l, r).map(Some(_))

  def normalize(net: Net, fuel: Int = 10_000): Either[String, Net] =
    if fuel <= 0 then Left("out of fuel reducing net")
    else step(net).flatMap {
      case Some(n2) => normalize(n2, fuel - 1)
      case None     => Right(net)
    }
