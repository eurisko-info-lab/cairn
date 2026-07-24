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

  def toReport(
      module: Module,
      outlineName: String,
      lang: String,
      resolve: Resolve,
  ): Either[String, Cst] =
    module.get(outlineName) match
      case Some(Cst.Node("outline", List(Cst.Leaf(name), Cst.Leaf(cas), sectionsField))) =>
        val refs = sectionsField match
          case Cst.Node("none", _) => Nil
          case Cst.Node("some", List(Cst.Node("list", rs))) =>
            rs.collect { case Cst.Leaf(r) => r }
          case Cst.Node("list", rs) => rs.collect { case Cst.Leaf(r) => r }
          case other => return Left(s"bad outline sections: ${other.render}")
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
        Right(Cst.node("report", Cst.Leaf(name), Cst.Leaf(cas), Cst.Node("list", blocks)))
      case Some(other) => Left(s"'$outlineName' is not an outline: ${other.render}")
      case None => Left(s"no outline '$outlineName'")
