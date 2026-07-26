package cairn.core

import cairn.kernel.*

enum SemanticLocation:
  case Binding(name: String)
  case WholeDefinition(name: String)
  case Subtree(name: String, path: SemanticPath)

  def definitionName: String = this match
    case Binding(n)          => n
    case WholeDefinition(n)  => n
    case Subtree(n, _)       => n

  def canon: Canon = this match
    case Binding(n) => Canon.CTag("binding", Canon.CStr(n))
    case WholeDefinition(n) => Canon.CTag("whole-definition", Canon.CStr(n))
    case Subtree(n, path) => Canon.CTag("subtree", Canon.cmap(
      "name" -> Canon.CStr(n), "path" -> path.canon))

  def render: String = this match
    case Binding(n) => s"binding:$n"
    case WholeDefinition(n) => s"definition:$n"
    case Subtree(n, path) => s"subtree:$n/${path.steps.map(SemanticLocation.stepRender).mkString("/")}"

object SemanticLocation:
  private def sameStep(a: SemanticPath.Step, b: SemanticPath.Step): Boolean = (a, b) match
    case (SemanticPath.Step.Field(ac, _, ap, af), SemanticPath.Step.Field(bc, _, bp, bf)) =>
      ac == bc && ((af, bf) match
        case (Some(x), Some(y)) => x == y
        case (None, None)       => ap == bp
        case _                  => false)
    case (SemanticPath.Step.Index(a), SemanticPath.Step.Index(b)) => a == b
    case (SemanticPath.Step.KeyedElement(as, af, av), SemanticPath.Step.KeyedElement(bs, bf, bv)) =>
      as == bs && af == bf && av == bv
    case _ => false

  private def prefix(a: List[SemanticPath.Step], b: List[SemanticPath.Step]): Boolean =
    a.length <= b.length && a.zip(b).forall(sameStep)

  def overlaps(a: SemanticLocation, b: SemanticLocation): Boolean =
    if a.definitionName != b.definitionName then false
    else (a, b) match
      case (Binding(_), Binding(_)) => true
      case (Binding(_), _) | (_, Binding(_)) => false
      case (WholeDefinition(_), _) | (_, WholeDefinition(_)) => true
      case (Subtree(_, ap), Subtree(_, bp)) => prefix(ap.steps, bp.steps) || prefix(bp.steps, ap.steps)

  private[core] def stepRender(step: SemanticPath.Step): String = step match
    case SemanticPath.Step.Field(ctor, _, position, fieldId) => fieldId.getOrElse(s"$ctor[$position]")
    case SemanticPath.Step.Index(position) => s"[$position]"
    case SemanticPath.Step.KeyedElement(sort, field, value) => s"$sort[$field=$value]"

enum AccessMode:
  case Read, Write

final case class SemanticAccess(mode: AccessMode, location: SemanticLocation)
final case class AccessTrace(accesses: List[SemanticAccess]):
  def ++(other: AccessTrace): AccessTrace = AccessTrace(accesses ++ other.accesses)

object AccessTrace:
  val empty: AccessTrace = AccessTrace(Nil)

  def conflicts(a: AccessTrace, b: AccessTrace): Set[SemanticLocation] =
    (for
      left <- a.accesses
      right <- b.accesses
      if left.mode == AccessMode.Write || right.mode == AccessMode.Write
      if SemanticLocation.overlaps(left.location, right.location)
      location <- List(left.location, right.location)
    yield location).toSet
