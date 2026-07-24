package cairn.runtime

import cairn.kernel.*
import cairn.core.Module

/** Generic outline → report CST projection.
  *
  * Domain packs supply [[Resolve]] (section number, title, field keys/text);
  * this object only walks the outline shape. SDS `SectionReport.toCst` is a
  * thin adapter — not a second projector.
  */
object OutlineProjector:

  final case class Resolve(
      sectionNumber: Cst => Option[Int],
      title: Int => String,
      fieldKeys: Cst => List[String],
      fieldText: (Module, String, String, String) => Option[String],
  )

  /** Section binding names from an `outline` term's sections field. */
  def sectionRefs(sectionsField: Cst): Either[String, List[String]] = sectionsField match
    case Cst.Node("none", _) => Right(Nil)
    case Cst.Node("some", List(Cst.Node("list", rs))) =>
      Right(rs.collect { case Cst.Leaf(r) => r })
    case Cst.Node("list", rs) => Right(rs.collect { case Cst.Leaf(r) => r })
    case other => Left(s"bad outline sections: ${other.render}")

  def outlineRefs(module: Module, outlineName: String): Either[String, (String, String, List[String])] =
    module.get(outlineName) match
      case Some(Cst.Node("outline", List(Cst.Leaf(name), Cst.Leaf(cas), sectionsField))) =>
        sectionRefs(sectionsField).map(refs => (name, cas, refs))
      case Some(other) => Left(s"'$outlineName' is not an outline: ${other.render}")
      case None => Left(s"no outline '$outlineName'")

  def toReport(
      module: Module,
      outlineName: String,
      lang: String,
      resolve: Resolve,
  ): Either[String, Cst] =
    outlineRefs(module, outlineName).map { (name, cas, refs) =>
      val blocks = refs.flatMap { ref =>
        module.get(ref).flatMap { sec =>
          resolve.sectionNumber(sec).map { n =>
            val keys = resolve.fieldKeys(sec)
            val lines = keys.flatMap { k =>
              resolve.fieldText(module, ref, k, lang).map { v =>
                Cst.node("fieldLine", Cst.Leaf(k), Cst.Leaf(v))
              }
            }
            Cst.node(
              "sectionBlock",
              Cst.Leaf(n.toString),
              Cst.Leaf(resolve.title(n)),
              Cst.Node("list", lines))
          }
        }
      }
      Cst.node("report", Cst.Leaf(name), Cst.Leaf(cas), Cst.Node("list", blocks))
    }
