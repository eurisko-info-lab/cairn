package cairn.core

import cairn.kernel.*

/** Generic field-row collection from CST lists — domain packs supply tags/indices. */
object ModuleFieldResolve:

  /** Rows `(lang, value)` under `fields` whose tag is in `tags` and key leaf matches. */
  def keyedRows(
      fields: List[Cst],
      tags: Set[String],
      keyIdx: Int,
      langIdx: Int,
      valueIdx: Int,
      key: String,
  ): List[(String, String)] =
    fields.flatMap {
      case Cst.Node(tag, kids) if tags.contains(tag) =>
        (kids.lift(keyIdx), kids.lift(langIdx), kids.lift(valueIdx)) match
          case (Some(Cst.Leaf(k)), Some(Cst.Leaf(l)), Some(Cst.Leaf(v))) if k == key =>
            List((l, v))
          case _ => Nil
      case _ => Nil
    }

  /** Collect `(defName, lang, text)` from bindings whose ctor is in `ctors`
    * and whose name leaf equals `name`.
    */
  def namedBindings(
      m: Module,
      ctors: Set[String],
      nameIdx: Int,
      langIdx: Int,
      textIdx: Int,
      name: String,
  ): List[(String, String, String)] =
    m.defs.flatMap {
      case (defName, Cst.Node(tag, kids)) if ctors.contains(tag) =>
        (kids.lift(nameIdx), kids.lift(langIdx), kids.lift(textIdx)) match
          case (Some(Cst.Leaf(n)), Some(Cst.Leaf(l)), Some(Cst.Leaf(t))) if n == name =>
            List((defName, l, t))
          case _ => Nil
      case _ => Nil
    }.toList

  /** Collect `(lang, text)` — [[namedBindings]] without def names. */
  def namedLangText(
      m: Module,
      ctors: Set[String],
      nameIdx: Int,
      langIdx: Int,
      textIdx: Int,
      name: String,
  ): List[(String, String)] =
    namedBindings(m, ctors, nameIdx, langIdx, textIdx, name).map { case (_, l, t) => (l, t) }

  /** Collect `(lang, hash, tag)` marks for a ctor keyed by leading leaf paths. */
  def stateMarks(
      m: Module,
      ctor: String,
      matchLeaves: List[String],
      langIdx: Int,
      hashIdx: Int,
      tagIdx: Int,
  ): List[(String, String, String)] =
    m.defs.flatMap {
      case (_, Cst.Node(c, kids)) if c == ctor =>
        val prefix = matchLeaves.zipWithIndex.forall { (want, i) =>
          kids.lift(i).contains(Cst.Leaf(want))
        }
        if !prefix then Nil
        else
          (kids.lift(langIdx), kids.lift(hashIdx), kids.lift(tagIdx)) match
            case (Some(Cst.Leaf(lang)), Some(Cst.Leaf(hash)), Some(Cst.Leaf(tag))) =>
              List((lang, hash, tag))
            case _ => Nil
      case _ => Nil
    }.toList

  /** Unwrap an `opt sepby1` overlay shape (`none` / `some(list(...))`) — or a
    * bare `list(...)` — into its rows. Shared by every SDS locale-overlay
    * child (`fieldLocale`/`fieldLocaleRef` under sections, phrases, …).
    */
  def optListRows(overlays: Cst): List[Cst] = overlays match
    case Cst.Node("none", _) => Nil
    case Cst.Node("some", List(Cst.Node("list", xs))) => xs
    case Cst.Node("list", xs) => xs
    case _ => Nil

  /** Typed-ctor EN slots: zip `keys` against the leaf args preceding the
    * trailing locale-overlay child. Empty if arity doesn't match
    * (`kids` must be exactly `keys.length + 1` long).
    */
  def typedSlots(keys: List[String], kids: List[Cst]): Map[String, String] =
    if kids.length != keys.length + 1 then Map.empty
    else keys.zip(kids.init).collect { case (k, Cst.Leaf(v)) => k -> v }.toMap
