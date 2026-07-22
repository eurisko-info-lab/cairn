package cairn.core

import cairn.kernel.*

/** Structural validation of a term against a `ComposedLanguage`'s own
  * Fragment data (constructors, sorts, arities) — the gate `Delta.applyTyped`
  * needs and didn't have: a parsed-from-text term is well-formed by
  * construction (the parser can't produce an unknown constructor or wrong
  * arity), but a STRUCTURED edit (`cairn.editDefAtStructured`, submitting a
  * JSON-shaped `Cst` directly) has no such guarantee. `checkTerm` closes
  * that gap generically, for any registered language, the same way the
  * parser/printer/LSP already work for any language with no per-language code.
  */
object LanguageChecker:
  enum TermError:
    case UnknownConstructor(ctor: String)
    case ArityMismatch(ctor: String, expected: Int, actual: Int)
    case SortMismatch(ctor: String, expectedSort: String, actualSort: String)
    case UnexpectedLeaf(expectedSort: String, text: String)

    def render: String = this match
      case UnknownConstructor(ctor) => s"unknown constructor '$ctor'"
      case ArityMismatch(ctor, expected, actual) =>
        s"'$ctor' expects $expected argument(s), got $actual"
      case SortMismatch(ctor, expectedSort, actualSort) =>
        s"'$ctor' has sort '$actualSort', expected '$expectedSort'"
      case UnexpectedLeaf(expectedSort, text) =>
        s"expected a '$expectedSort' term, got bare leaf '$text'"

  /** Opaque proof a term passed [[checkTerm]] against some language+sort. */
  opaque type CheckedTerm = Cst
  object CheckedTerm:
    extension (c: CheckedTerm) def term: Cst = c

  /** Resolve a `PrecCategory` name to its underlying plain category, if
    * `name` names a precedence category; otherwise `name` unchanged.
    */
  private def catBase(language: ComposedLanguage, name: String): String =
    language.grammar.precCategories.find(_.name == name).map(_.base).getOrElse(name)

  /** `expectedSort` names either:
    *   - a SEMANTIC sort (`CtorDef.sort`, e.g. `"Term"`) — the normal case
    *     for any nested position, since `CtorDef.argSorts` entries are
    *     concrete sorts; or
    *   - a grammar CATEGORY (e.g. `"term"`, or Search's `"searchObj"`) — the
    *     case for a language's own top-level entry point
    *     (`ComposedLanguage.grammar.top`), which callers should pass as-is
    *     with no attempt to resolve it to a single semantic sort. A category
    *     can legitimately span MULTIPLE underlying sorts (Search's
    *     `"searchObj"` category admits both `"Fact"`- and `"Intent"`-sorted
    *     constructors) — membership in the category, not equality to one
    *     resolved sort, is the real invariant, so this checks tag membership
    *     and then recurses using the matched constructor's OWN sort.
    *
    * A `sort` that resolves to neither (not a declared `SortDef`, not a
    * grammar category) is either a genuine primitive/leaf sort (e.g.
    * `"Name"`, always a bare `Leaf`) or a "many/optional X" pseudo-sort named
    * by pluralization convention with no `SortDef` of its own (e.g. `ctor
    * mixture : SdsObj(Components)` in `languages/sds.cairn` — `"Components"`
    * is never declared, it's a naming convention paired with a
    * `sepby1`/`star`/`opt` grammar rule that wraps the real items in
    * `"list"`/`"some"`/`"none"`, already unwrapped below). There's no
    * reliable general depluralization to recover the element sort name, but
    * a `Node` here is still checkable on its own terms: look up its
    * constructor and validate arity/children against ITS declared sort,
    * rather than rejecting outright for not matching the pseudo-sort. This
    * same fallback also covers a `ComposedLanguage` built directly from
    * semantic fragments with no bound surface (an empty `grammar.top`, e.g.
    * for migration/digest purposes only) — there's no grammar to validate
    * against at all, so every def is checked purely on its own constructor's
    * declared shape.
    */
  def checkTerm(language: ComposedLanguage, expectedSort: String, term: Cst): Either[List[TermError], CheckedTerm] =
    def selfCheck(ctor: String, children: List[Cst]): List[TermError] =
      language.constructors.get(ctor) match
        case None => List(TermError.UnknownConstructor(ctor))
        case Some(cd) if cd.argSorts.length != children.length =>
          List(TermError.ArityMismatch(ctor, cd.argSorts.length, children.length))
        case Some(cd) =>
          cd.argSorts.zip(children).flatMap((argSort, child) => go(child, argSort))
    def go(t: Cst, sort: String): List[TermError] = t match
      // "list"/"some"/"none" are the grammar engine's OWN reserved wrapper
      // tags for Star/Opt/SepBy1 (Grammar.scala's parser emits them directly,
      // never via a declared CtorDef) — a CtorDef.argSorts position doesn't
      // distinguish "one X" from "zero-or-more X"/"optional X", it reuses the
      // same sort name X either way, so these wrappers unwrap transparently:
      // every contained item is checked against the SAME expected sort, not
      // looked up as a constructor of that name.
      case Cst.Node("list", items)  => items.flatMap(go(_, sort))
      case Cst.Node("some", List(x)) => go(x, sort)
      case Cst.Node("none", _)       => Nil
      case Cst.Leaf(x) if language.sorts.contains(sort) => List(TermError.UnexpectedLeaf(sort, x))
      case Cst.Node(ctor, children) if language.sorts.contains(sort) =>
        language.constructors.get(ctor) match
          case None => List(TermError.UnknownConstructor(ctor))
          case Some(cd) if cd.sort != sort => List(TermError.SortMismatch(ctor, sort, cd.sort))
          case Some(cd) => selfCheck(ctor, children)
      case Cst.Node(ctor, children) if language.grammar.categories.exists(_.name == catBase(language, sort)) =>
        val cat = language.grammar.categories.find(_.name == catBase(language, sort)).get
        language.constructors.get(ctor) match
          case None => List(TermError.UnknownConstructor(ctor))
          case Some(cd) if !cat.ctors.exists(_.tag == ctor) => List(TermError.SortMismatch(ctor, sort, cd.sort))
          case Some(_) => selfCheck(ctor, children)
      case Cst.Leaf(_) => Nil
      case Cst.Node(ctor, children) => selfCheck(ctor, children)
    val errs = go(term, expectedSort)
    if errs.isEmpty then Right(term) else Left(errs)

  /** Walk a child-index path from the language's grammar top category through
    * an EXISTING (already well-formed) term, to find the sort a REPLACEMENT
    * at that position must satisfy — `Delta`'s `edit` case's expected-sort
    * derivation. The existing term is trusted (already validated when it was
    * added); this walk only determines what sort fits at `path`. The
    * starting sort is `language.grammar.top` itself (a grammar category, not
    * a resolved semantic sort — see [[checkTerm]]'s doc for why these must
    * stay distinct); every subsequent hop uses a concrete `CtorDef.argSorts`
    * entry, which `checkTerm` already knows how to interpret regardless of
    * which of its three cases (real sort / category / pseudo-sort) it is.
    */
  def expectedSortAt(language: ComposedLanguage, root: Cst, path: List[Int]): Either[String, String] =
    def go(t: Cst, sort: String, remaining: List[Int]): Either[String, String] = remaining match
      case Nil => Right(sort)
      case i :: rest => t match
        // Same reserved wrappers as checkTerm: indexing into a list/some/none
        // node's children keeps the SAME sort — there's no CtorDef for these,
        // the grammar engine produces them directly for Star/Opt/SepBy1.
        case Cst.Node("list" | "some" | "none", children) if i >= 0 && i < children.length =>
          go(children(i), sort, rest)
        case Cst.Node(ctor, children) if i >= 0 && i < children.length =>
          language.constructors.get(ctor) match
            case Some(cd) if i < cd.argSorts.length => go(children(i), cd.argSorts(i), rest)
            case Some(cd) => Left(s"'$ctor' has no argSort for child index $i")
            case None     => Left(s"unknown constructor '$ctor' while walking path")
        case Cst.Node(ctor, children) => Left(s"path index $i out of range for '$ctor' (${children.length} children)")
        case Cst.Leaf(x) => Left(s"path descends into leaf '$x'")
    go(root, language.grammar.top, path)
