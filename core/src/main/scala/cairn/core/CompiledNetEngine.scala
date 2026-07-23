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
  // Reject a malformed or ambiguous rule table at construction, rather than
  // letting it be silently indexed below — an ambiguous table (two rules
  // claiming the same unordered kind pair) would otherwise be granted
  // accidental declaration-order semantics by getOrElseUpdate rather than
  // being caught as the validation error it is. See NetLanguageChecker's
  // doc for the full list of what "malformed" covers (unknown kinds,
  // out-of-range ports, non-linear rewrites).
  NetLanguageChecker.validate(lang).left.foreach { errs =>
    throw RuntimeException(
      s"CompiledNetEngine: invalid NetLanguage '${lang.name}': ${errs.map(_.render).mkString("; ")}")
  }

  /** `getOrElseUpdate` preserves [[NetEngine.ruleFor]]'s exact precedence:
    * every rule's forward (left, right) orientation is indexed before ANY
    * rule's backward (right, left) orientation is considered. With the
    * validation above in place, no two DIFFERENT rules ever share an
    * unordered kind pair, so `getOrElseUpdate` here can never actually
    * overwrite-skip a competing rule — the only remaining writes are a
    * single rule's own forward and (if `left != right`) backward entries,
    * which target different keys.
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
