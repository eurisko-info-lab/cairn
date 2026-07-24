package cairn.core

import cairn.kernel.*

/** Derive ordered slot labels from grammar-as-data — not host key tables.
  *
  * Pattern: `Tok(label) StrLeaf` in a constructor production names an EN
  * string slot (SDS typed sections, similar surfaces). Domain packs must
  * not duplicate those labels in Scala maps.
  */
object SurfaceSlots:

  /** Labels of `Tok(lab) StrLeaf` pairs in ctor `tag`, in production order. */
  def labeledStrSlots(g: GrammarSpec, tag: String): List[String] =
    val elems = g.categories.flatMap(_.ctors).collectFirst {
      case ConstructorSpec(t, es) if t == tag => es
    }.getOrElse(Nil)
    labeledStrSlots(elems)

  def labeledStrSlots(elems: List[Elem]): List[String] =
    def go(es: List[Elem], acc: List[String]): List[String] = es match
      case Elem.Tok(lab) :: Elem.StrLeaf :: rest => go(rest, acc :+ lab)
      case _ :: rest => go(rest, acc)
      case Nil => acc
    go(elems, Nil)

  /** Constructor names ending with `suffix`, excluding `exclude`, in language
    * fragment declaration order (stable amalgamation order).
    */
  def ctorsEndingWith(
      language: ComposedLanguage,
      suffix: String,
      exclude: Set[String] = Set.empty,
  ): List[String] =
    language.fragments.flatMap(_.constructors).map(_.name)
      .filter(n => n.endsWith(suffix) && !exclude.contains(n))
      .distinct

  /** `tag` → ordered EN slot keys for every ctor that has labeled StrLeaf slots. */
  def allLabeledStrSlots(g: GrammarSpec): Map[String, List[String]] =
    g.categories.flatMap(_.ctors).flatMap { c =>
      val keys = labeledStrSlots(c.elems)
      if keys.isEmpty then None else Some(c.tag -> keys)
    }.toMap
