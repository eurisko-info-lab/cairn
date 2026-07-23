package cairn.core

/** Structural validation of a `RosettaModule2` before any `PortV2.emit` sees
  * it — the gate `PortV2.verified` needs and didn't have. None of these are
  * things a genuinely well-formed module can violate by accident, but
  * nothing previously stopped a malformed one from reaching a host emitter
  * and either crashing deep inside port-specific code or silently rendering
  * something subtly wrong (constraint truncation, effect-op collision).
  */
object RosettaChecker:
  enum ModuleError:
    case DuplicateData(name: String)
    case DuplicateEffect(name: String)
    case DuplicateDef(name: String)
    case DuplicateTypeParam(defName: String, param: String)
    case DuplicateParam(defName: String, param: String)
    case UnknownEffect(defName: String, effect: String)
    case AmbiguousEffectOp(defName: String, op: String, effects: List[String])
    case MultipleConstraints(defName: String, params: List[String])

    def render: String = this match
      case DuplicateData(name)   => s"duplicate data declaration '$name'"
      case DuplicateEffect(name) => s"duplicate effect declaration '$name'"
      case DuplicateDef(name)    => s"duplicate def '$name'"
      case DuplicateTypeParam(defName, param) =>
        s"def '$defName' declares type parameter '$param' more than once"
      case DuplicateParam(defName, param) =>
        s"def '$defName' declares parameter '$param' more than once"
      case UnknownEffect(defName, effect) =>
        s"def '$defName' references undeclared effect '$effect'"
      case AmbiguousEffectOp(defName, op, effects) =>
        s"def '$defName' composes effects ${effects.mkString(", ")}, which both declare operation '$op' — ambiguous dispatch"
      case MultipleConstraints(defName, params) =>
        s"def '$defName' declares multiple constraints (${params.mkString(", ")}) — only the first is rendered by 3 of 4 host ports"

  private def dupes[A](items: List[A]): List[A] =
    items.groupBy(identity).collect { case (k, vs) if vs.sizeIs > 1 => k }.toList

  def check(m: RosettaModule2): List[ModuleError] =
    val errs = List.newBuilder[ModuleError]
    for name <- dupes(m.datas.map(_.name)) do errs += ModuleError.DuplicateData(name)
    for name <- dupes(m.effects.map(_.name)) do errs += ModuleError.DuplicateEffect(name)
    for name <- dupes(m.defs.map(_.name)) do errs += ModuleError.DuplicateDef(name)

    val effectsByName = m.effects.map(e => e.name -> e).toMap
    for d <- m.defs do
      for tp <- dupes(d.typeParams) do errs += ModuleError.DuplicateTypeParam(d.name, tp)
      for p <- dupes(d.params.map(_._1)) do errs += ModuleError.DuplicateParam(d.name, p)
      if d.constraints.sizeIs > 1 then
        errs += ModuleError.MultipleConstraints(d.name, d.constraints.map(_._1))
      // Only check op-ambiguity across effects that are actually declared —
      // an unknown effect is reported once below, not doubled up here.
      val declaredEffects = d.effects.flatMap(en => effectsByName.get(en).map(en -> _))
      if declaredEffects.length == d.effects.length then
        val owners = declaredEffects.flatMap((en, e) => e.ops.map(_ -> en))
          .groupBy(_._1).view.mapValues(_.map(_._2)).toMap
        for (op, ens) <- owners if ens.sizeIs > 1 do
          errs += ModuleError.AmbiguousEffectOp(d.name, op, ens)
      for en <- d.effects if !effectsByName.contains(en) do
        errs += ModuleError.UnknownEffect(d.name, en)
    errs.result()

  def validate(m: RosettaModule2): Either[List[ModuleError], RosettaModule2] =
    val errs = check(m)
    if errs.isEmpty then Right(m) else Left(errs)
