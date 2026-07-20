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

final case class GrammarSpec(
    name: String,
    tokens: TokenSpec,
    categories: List[CategorySpec],
    precCategories: List[PrecCategory],
    printRules: List[PrintRule],
    top: String,
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
      "identContExtra" -> CStr(g.tokens.identContExtra)),
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
    "top" -> CStr(g.top))

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
        identContExtra = toks.field("identContExtra").asStr),
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
      top = c.field("top").asStr)

  def artifact(g: GrammarSpec): Artifact = Artifact(ArtifactKind.Grammar, toCanon(g))
