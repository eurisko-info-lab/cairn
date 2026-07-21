package cairn.kernel

/** Grammar-as-data vocabulary (S8). Grammars are artifacts: pure data
  * interpreted by ONE generic lexer/parser/printer in the workbench (§4.2).
  * No per-object-language parser or printer code exists anywhere in L0–L1.
  */
final case class TokenSpec(
    keywords: List[String],
    puncts: List[String], // matched longest-first
    lineComment: Option[String],
    identStartExtra: String = "_",
    identContExtra: String = "_'",
    /** M6: markers that capture the rest of the physical line verbatim as one token. */
    restOfLineMarkers: List[String] = Nil,
)

/** Parser combinators as data. */
enum Elem:
  case Tok(text: String)                 // match exact text, discard
  case TokField(text: String)            // match exact text, keep as leaf field
  case Cat(name: String)                 // recurse into category
  case Opt(e: Elem)                      // zero or one (missing => node "none")
  case Star(e: Elem)                     // zero or more
  case SepBy1(e: Elem, sep: String)      // one or more with separator token
  case NameLeaf                          // strict identifier (keywords excluded)
  case NumLeaf
  case StrLeaf
  // M6: layout vocabulary
  case AnyIdentLeaf                      // identifier OR keyword as an ordinary label
  case Block(itemCat: String)            // offside rule: items at the column of the first
  case Run(atomCat: String)              // juxtaposition: atoms on same line / indented past floor
  case Adjacent1(e: Elem)                // 1+ repetitions, stops at a blank-line gap
  case RestOfLine                        // one verbatim rest-of-line token

final case class ConstructorSpec(tag: String, elems: List[Elem])
final case class CategorySpec(name: String, ctors: List[ConstructorSpec]) // ordered PEG choice

/** Precedence climbing for bare infix surfaces (e.g. `a -> b -> c`). */
final case class InfixOp(text: String, tag: String, prec: Int, rightAssoc: Boolean)
final case class PrecCategory(name: String, base: String, ops: List[InfixOp])

/** Print side: dual view of the same constructor data. */
enum PrintSeg:
  case Lit(text: String)
  case Field(index: Int)                          // print child i
  case StrField(index: Int)                       // print child i as a quoted string literal
  case SepFields(from: Int, sep: String)          // children i.. joined by sep
  case Space
  case Newline
  case IndentIn
  case IndentOut

final case class PrintRule(tag: String, segs: List[PrintSeg])

/** Default print rules from syntax productions. Explicit `print` lines are
  * overrides that win over derivation (see [[complete]]). Infix ops stay
  * printer-special (no derived rule). Sugar tags (`$…`) are skipped.
  */
object PrintDerive:
  private val tightOpen  = Set("(", "[", "{")
  private val tightClose = Set(")", "]", "}", ";")

  private def tokText(e: Elem): Option[String] = e match
    case Elem.Tok(t)      => Some(t)
    case Elem.TokField(t) => Some(t)
    case _                => None

  private def isTightOpen(e: Elem): Boolean  = tokText(e).exists(tightOpen.contains)
  private def isTightClose(e: Elem): Boolean = tokText(e).exists(tightClose.contains)

  /** Field-producing elems (Tok is discarded at parse time). */
  private def elemSegs(e: Elem, fieldIdx: Int): (List[PrintSeg], Int) = e match
    case Elem.Tok(t)      => (List(PrintSeg.Lit(t)), fieldIdx)
    case Elem.TokField(t) => (List(PrintSeg.Lit(t)), fieldIdx + 1) // child kept; lit mirrors fixed text
    case Elem.StrLeaf     => (List(PrintSeg.StrField(fieldIdx)), fieldIdx + 1)
    case _                => (List(PrintSeg.Field(fieldIdx)), fieldIdx + 1)

  /** Invert a production into print segments with the default spacing policy:
    * space between adjacent elems, except no space after `(`/`[`/`{` and no
    * space before `)`/`]`/`}`/`;`.
    */
  def segs(elems: List[Elem]): List[PrintSeg] =
    val out = List.newBuilder[PrintSeg]
    var fieldIdx = 0
    var prev: Option[Elem] = None
    for e <- elems do
      val (ss, nextIdx) = elemSegs(e, fieldIdx)
      if ss.nonEmpty then
        val space =
          prev.isDefined && !isTightOpen(prev.get) && !isTightClose(e)
        if space then out += PrintSeg.Space
        out ++= ss
        prev = Some(e)
        fieldIdx = nextIdx
    out.result()

  def derive(
      categories: List[CategorySpec],
      precCategories: List[PrecCategory] = Nil,
  ): List[PrintRule] =
    val infixTags = precCategories.flatMap(_.ops).map(_.tag).toSet
    categories.flatMap(_.ctors).collect {
      case cs if !cs.tag.startsWith("$") && !infixTags.contains(cs.tag) =>
        PrintRule(cs.tag, segs(cs.elems))
    }

  /** Derived rules filled in; explicit overrides win on tag collision. */
  def complete(
      categories: List[CategorySpec],
      precCategories: List[PrecCategory],
      overrides: List[PrintRule],
  ): List[PrintRule] =
    val over = overrides.map(r => r.tag -> r).toMap
    val der = derive(categories, precCategories).map(r => r.tag -> r).toMap
    (der.keySet ++ over.keySet).toList.sorted.map(t => over.getOrElse(t, der(t)))

final case class GrammarSpec(
    name: String,
    tokens: TokenSpec,
    categories: List[CategorySpec],
    precCategories: List[PrecCategory],
    printRules: List[PrintRule],
    top: String,
    /** M11: tokens the recovering parser skips to after an error inside a Star. */
    syncTokens: List[String] = Nil,
):
  def category(n: String): Option[CategorySpec] = categories.find(_.name == n)
  def precCategory(n: String): Option[PrecCategory] = precCategories.find(_.name == n)
  def printRule(tag: String): Option[PrintRule] = printRules.find(_.tag == tag)

object GrammarSpec:
  import Canon.*

  private def elemToCanon(e: Elem): Canon = e match
    case Elem.Tok(t)       => CTag("tok", CStr(t))
    case Elem.TokField(t)  => CTag("tokField", CStr(t))
    case Elem.Cat(n)       => CTag("cat", CStr(n))
    case Elem.Opt(e)       => CTag("opt", elemToCanon(e))
    case Elem.Star(e)      => CTag("star", elemToCanon(e))
    case Elem.SepBy1(e, s) => CTag("sepBy1", Canon.cmap("e" -> elemToCanon(e), "sep" -> CStr(s)))
    case Elem.NameLeaf     => CTag("nameLeaf", CInt(0))
    case Elem.NumLeaf      => CTag("numLeaf", CInt(0))
    case Elem.StrLeaf      => CTag("strLeaf", CInt(0))
    case Elem.AnyIdentLeaf => CTag("anyIdentLeaf", CInt(0))
    case Elem.Block(c)     => CTag("block", CStr(c))
    case Elem.Run(c)       => CTag("run", CStr(c))
    case Elem.Adjacent1(e) => CTag("adjacent1", elemToCanon(e))
    case Elem.RestOfLine   => CTag("restOfLine", CInt(0))

  private def elemFromCanon(c: Canon): Elem = c match
    case CTag("tok", CStr(t))      => Elem.Tok(t)
    case CTag("tokField", CStr(t)) => Elem.TokField(t)
    case CTag("cat", CStr(n))      => Elem.Cat(n)
    case CTag("opt", e)            => Elem.Opt(elemFromCanon(e))
    case CTag("star", e)           => Elem.Star(elemFromCanon(e))
    case CTag("sepBy1", m)         => Elem.SepBy1(elemFromCanon(m.field("e")), m.field("sep").asStr)
    case CTag("nameLeaf", _)       => Elem.NameLeaf
    case CTag("numLeaf", _)        => Elem.NumLeaf
    case CTag("strLeaf", _)        => Elem.StrLeaf
    case CTag("anyIdentLeaf", _)   => Elem.AnyIdentLeaf
    case CTag("block", CStr(c))    => Elem.Block(c)
    case CTag("run", CStr(c))      => Elem.Run(c)
    case CTag("adjacent1", e)      => Elem.Adjacent1(elemFromCanon(e))
    case CTag("restOfLine", _)     => Elem.RestOfLine
    case other                     => throw CodecError(s"not an elem: $other")

  private def segToCanon(s: PrintSeg): Canon = s match
    case PrintSeg.Lit(t)           => CTag("lit", CStr(t))
    case PrintSeg.Field(i)         => CTag("field", CInt(i))
    case PrintSeg.StrField(i)      => CTag("strField", CInt(i))
    case PrintSeg.SepFields(f, s)  => CTag("sepFields", Canon.cmap("from" -> CInt(f), "sep" -> CStr(s)))
    case PrintSeg.Space            => CTag("space", CInt(0))
    case PrintSeg.Newline          => CTag("newline", CInt(0))
    case PrintSeg.IndentIn         => CTag("indentIn", CInt(0))
    case PrintSeg.IndentOut        => CTag("indentOut", CInt(0))

  private def segFromCanon(c: Canon): PrintSeg = c match
    case CTag("lit", CStr(t))   => PrintSeg.Lit(t)
    case CTag("field", CInt(i)) => PrintSeg.Field(i.toInt)
    case CTag("strField", CInt(i)) => PrintSeg.StrField(i.toInt)
    case CTag("sepFields", m)   => PrintSeg.SepFields(m.field("from").asInt.toInt, m.field("sep").asStr)
    case CTag("space", _)       => PrintSeg.Space
    case CTag("newline", _)     => PrintSeg.Newline
    case CTag("indentIn", _)    => PrintSeg.IndentIn
    case CTag("indentOut", _)   => PrintSeg.IndentOut
    case other                  => throw CodecError(s"not a print seg: $other")

  def toCanon(g: GrammarSpec): Canon = Canon.cmap(
    "name" -> CStr(g.name),
    "tokens" -> Canon.cmap(
      "keywords" -> Canon.cstrs(g.tokens.keywords),
      "puncts" -> Canon.cstrs(g.tokens.puncts),
      "lineComment" -> g.tokens.lineComment.fold(CTag("none", CInt(0)))(s => CTag("some", CStr(s))),
      "identStartExtra" -> CStr(g.tokens.identStartExtra),
      "identContExtra" -> CStr(g.tokens.identContExtra),
      "restOfLineMarkers" -> Canon.cstrs(g.tokens.restOfLineMarkers)),
    "categories" -> CList(g.categories.map(c => Canon.cmap(
      "name" -> CStr(c.name),
      "ctors" -> CList(c.ctors.map(k => Canon.cmap(
        "tag" -> CStr(k.tag), "elems" -> CList(k.elems.map(elemToCanon)))))))),
    "precCategories" -> CList(g.precCategories.map(p => Canon.cmap(
      "name" -> CStr(p.name), "base" -> CStr(p.base),
      "ops" -> CList(p.ops.map(o => Canon.cmap(
        "text" -> CStr(o.text), "tag" -> CStr(o.tag),
        "prec" -> CInt(o.prec), "rightAssoc" -> CInt(if o.rightAssoc then 1 else 0))))))),
    "printRules" -> CList(g.printRules.map(r => Canon.cmap(
      "tag" -> CStr(r.tag), "segs" -> CList(r.segs.map(segToCanon))))),
    "top" -> CStr(g.top),
    "syncTokens" -> Canon.cstrs(g.syncTokens))

  def fromCanon(c: Canon): GrammarSpec =
    val toks = c.field("tokens")
    GrammarSpec(
      name = c.field("name").asStr,
      tokens = TokenSpec(
        keywords = toks.field("keywords").asList.map(_.asStr),
        puncts = toks.field("puncts").asList.map(_.asStr),
        lineComment = toks.field("lineComment") match
          case CTag("some", CStr(s)) => Some(s)
          case _                     => None,
        identStartExtra = toks.field("identStartExtra").asStr,
        identContExtra = toks.field("identContExtra").asStr,
        restOfLineMarkers = toks.field("restOfLineMarkers").asList.map(_.asStr)),
      categories = c.field("categories").asList.map { cc =>
        CategorySpec(cc.field("name").asStr, cc.field("ctors").asList.map { k =>
          ConstructorSpec(k.field("tag").asStr, k.field("elems").asList.map(elemFromCanon)) }) },
      precCategories = c.field("precCategories").asList.map { pc =>
        PrecCategory(pc.field("name").asStr, pc.field("base").asStr,
          pc.field("ops").asList.map { o =>
            InfixOp(o.field("text").asStr, o.field("tag").asStr,
              o.field("prec").asInt.toInt, o.field("rightAssoc").asInt == 1L) }) },
      printRules = c.field("printRules").asList.map { r =>
        PrintRule(r.field("tag").asStr, r.field("segs").asList.map(segFromCanon)) },
      top = c.field("top").asStr,
      syncTokens = c.field("syncTokens").asList.map(_.asStr))

  def artifact(g: GrammarSpec): Artifact = Artifact(ArtifactKind.Grammar, toCanon(g))
