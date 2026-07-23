package cairn.core

/** Structural validation of a `NetLanguage`'s rule TABLE — distinct from
  * [[NetEngine.wellFormed]], which validates a NET against an already-trusted
  * language. Nothing previously checked the rule table itself before
  * [[CompiledNetEngine]] indexed it: an ambiguous table (two different rules
  * declaring the same unordered kind pair) is faithfully preserved by
  * `CompiledNetEngine`'s `getOrElseUpdate` — first-declared-wins — but that's
  * accidental declaration-order semantics, not a validated property, and a
  * rule referencing an undeclared kind, an out-of-range `RulePort`, or a
  * non-linear connection list would previously only fail (or silently
  * misbehave) deep inside [[NetEngine.applyPair]] at REDUCTION time, not at
  * definition time.
  */
object NetLanguageChecker:
  enum RuleError:
    case AmbiguousPair(left: String, right: String, rules: List[String])
    case UnknownKind(rule: String, side: String, kind: String)
    case NewAgentIndexOutOfRange(rule: String, index: Int, count: Int)
    case NewAgentPortOutOfRange(rule: String, index: Int, kind: String, port: Int, arity: Int)
    case ExtPortOutOfRange(rule: String, side: Int, aux: Int, kind: String, arity: Int)
    case NotLinear(rule: String, port: String, uses: Int)

    def render: String = this match
      case AmbiguousPair(left, right, rules) =>
        s"rules ${rules.mkString(", ")} all match the kind pair ($left, $right) — ambiguous dispatch"
      case UnknownKind(rule, side, kind) =>
        s"rule '$rule': $side agent kind '$kind' is not declared"
      case NewAgentIndexOutOfRange(rule, index, count) =>
        s"rule '$rule': New($index, _) references newAgents index $index, but only $count are created"
      case NewAgentPortOutOfRange(rule, index, kind, port, arity) =>
        s"rule '$rule': New($index, $port) — created agent $index has kind '$kind' (arity $arity), port $port out of range"
      case ExtPortOutOfRange(rule, side, aux, kind, arity) =>
        s"rule '$rule': Ext($side, $aux) — ${if side == 0 then "left" else "right"} agent kind '$kind' (arity $arity), aux port $aux out of range"
      case NotLinear(rule, port, uses) =>
        s"rule '$rule': port $port used $uses time(s) in connections (must be exactly 1 — linearity violation)"

  /** Every port a rule's rewrite must account for exactly once: each created
    * agent's own ports (0..arity), plus each side's EXTERNAL aux ports
    * (1..arity, `Ext`'s own numbering — see [[RulePort.Ext]]/[[NetEngine.applyPairFrom]]'s
    * `resolve`, which maps `Ext(side, aux)` to the consumed agent's `aux + 1`).
    */
  private def allPortsOf(lang: NetLanguage, r: NetRule): (List[RuleError], Option[List[String]]) =
    val errs = List.newBuilder[RuleError]
    val leftKind = lang.kindOf(r.left)
    val rightKind = lang.kindOf(r.right)
    if leftKind.isEmpty then errs += RuleError.UnknownKind(r.name, "left", r.left)
    if rightKind.isEmpty then errs += RuleError.UnknownKind(r.name, "right", r.right)
    val newKinds: List[Option[AgentKind]] = r.newAgents.map {
      case "@left"  => leftKind
      case "@right" => rightKind
      case k        => lang.kindOf(k)
    }
    for (spec, ok) <- r.newAgents.zip(newKinds) if ok.isEmpty && spec != "@left" && spec != "@right" do
      errs += RuleError.UnknownKind(r.name, s"newAgents(${r.newAgents.indexOf(spec)})", spec)
    if leftKind.isEmpty || rightKind.isEmpty || newKinds.exists(_.isEmpty) then (errs.result(), None)
    else
      val newPorts = newKinds.zipWithIndex.flatMap((k, i) => (0 to k.get.arity).map(p => s"new($i,$p)"))
      val extPorts = (1 to leftKind.get.arity).map(p => s"ext(0,$p)") ++
        (1 to rightKind.get.arity).map(p => s"ext(1,$p)")
      (errs.result(), Some(newPorts ++ extPorts))

  private def portName(lang: NetLanguage, r: NetRule, rp: RulePort): Either[RuleError, String] = rp match
    case RulePort.New(i, p) =>
      if i < 0 || i >= r.newAgents.length then Left(RuleError.NewAgentIndexOutOfRange(r.name, i, r.newAgents.length))
      else
        val kind = r.newAgents(i) match
          case "@left"  => lang.kindOf(r.left)
          case "@right" => lang.kindOf(r.right)
          case k        => lang.kindOf(k)
        kind match
          case Some(k) if p >= 0 && p <= k.arity => Right(s"new($i,$p)")
          case Some(k) => Left(RuleError.NewAgentPortOutOfRange(r.name, i, k.name, p, k.arity))
          case None    => Right(s"new($i,$p)") // unknown-kind already reported by allPortsOf
    case RulePort.Ext(side, aux) =>
      val kind = if side == 0 then lang.kindOf(r.left) else lang.kindOf(r.right)
      kind match
        case Some(k) if aux >= 0 && aux < k.arity => Right(s"ext($side,${aux + 1})")
        case Some(k) => Left(RuleError.ExtPortOutOfRange(r.name, side, aux, k.name, k.arity))
        case None    => Right(s"ext($side,${aux + 1})")

  /** One rule's own internal well-formedness: known kinds, in-range ports,
    * and linearity (every port the rewrite touches appears in exactly one
    * connection).
    */
  def checkRule(lang: NetLanguage, r: NetRule): List[RuleError] =
    val (kindErrs, allPortsOpt) = allPortsOf(lang, r)
    allPortsOpt match
      case None => kindErrs
      case Some(allPorts) =>
        val errs = List.newBuilder[RuleError]
        errs ++= kindErrs
        val resolved = r.connections.flatMap((x, y) => List(portName(lang, r, x), portName(lang, r, y)))
        val (portErrs, names) = resolved.partitionMap(identity)
        errs ++= portErrs
        if portErrs.isEmpty then
          val counts = names.groupBy(identity).view.mapValues(_.size).toMap
          for port <- allPorts do
            counts.getOrElse(port, 0) match
              case 1 => ()
              case n => errs += RuleError.NotLinear(r.name, port, n)
        errs.result()

  /** The full table: every rule individually well-formed, AND no two
    * different rules claim the same unordered (kind, kind) active pair.
    */
  def check(lang: NetLanguage): List[RuleError] =
    val errs = List.newBuilder[RuleError]
    for r <- lang.rules do errs ++= checkRule(lang, r)
    def unordered(r: NetRule): (String, String) = if r.left <= r.right then (r.left, r.right) else (r.right, r.left)
    for (pair, rules) <- lang.rules.groupBy(unordered) if rules.sizeIs > 1 do
      errs += RuleError.AmbiguousPair(pair._1, pair._2, rules.map(_.name))
    errs.result()

  def validate(lang: NetLanguage): Either[List[RuleError], NetLanguage] =
    val errs = check(lang)
    if errs.isEmpty then Right(lang) else Left(errs)
