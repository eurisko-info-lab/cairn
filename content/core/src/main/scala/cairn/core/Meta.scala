package cairn.core

import cairn.kernel.*

/** M41/M42: the FULL meta surface — Cairn's fragment IR as a Cairn language.
  *
  * Every component of a [[Fragment]] is expressible as text: interfaces,
  * sorts, constructors (with binders), variable ctor, grammar keywords/puncts,
  * syntax alternatives (the whole Elem vocabulary), print rules (the whole
  * PrintSeg vocabulary), infix tables, rewrite rules, judgments (premises,
  * conclusion, side conditions), and the top category.
  *
  * [[Meta.encode]] is the mechanical inverse of [[Meta.elaborateFragment]]:
  * `elaborate(parse(print(encode(f)))) == f` — so a fragment written in this
  * surface composes DIGEST-IDENTICALLY to the host-constructed value, and the
  * meta fragment can describe itself (the M42 bootstrap fixpoint).
  */
object Meta:
  private def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)
  private def leaf(s: String): Cst = Cst.Leaf(s)
  private def lst(items: List[Cst]): Cst = Cst.Node("list", items)
  private def opt(o: Option[Cst]): Cst = o.fold(n("none"))(c => n("some", c))
  private def optList(items: List[Cst]): Cst =
    if items.isEmpty then n("none") else n("some", lst(items))

  /** The meta surface's own definition — composition/fragment IR only. Grammar
    * productions (`syntax`/`print`/`infix`/…) live in [[grammarFragment]] —
    * `meta` composes with it (see [[language]]) to become self-hosting, the
    * same way any other fragment composes with a `requires`d interface.
    */
  val fragment: Fragment = Fragment(
    name = "meta",
    provides = List("meta"),
    requires = List("grammar"),
    sorts = List(SortDef("FragmentD", SortMode.Tree)),
    grammar = GrammarPart(
      keywords = List("language", "surface", "for", "fragment", "provides", "requires", "sort", "tree", "graph",
        "ctor", "binds", "in", "varctor", "rule", "judgment", "top", "where"),
      puncts = List("{", "}", "(", ")", ":", ";", ",", "=>", "|-", "$"),
      identContExtra = "-",
      categories = List(
        CategorySpec("file", List(
          ConstructorSpec("file", List(
            Elem.Tok("language"), Elem.AnyIdentLeaf, Elem.Tok("{"),
            Elem.Star(Elem.Cat("fragmentDecl")), Elem.Tok("}"))),
          ConstructorSpec("surfaceFile", List(
            Elem.Tok("surface"), Elem.AnyIdentLeaf, Elem.Tok("for"), Elem.AnyIdentLeaf, Elem.Tok("{"),
            Elem.Star(Elem.Cat("fragmentDecl")), Elem.Tok("}"))))),
        CategorySpec("fragmentDecl", List(
          ConstructorSpec("fragmentDecl", List(
            Elem.Tok("fragment"), Elem.AnyIdentLeaf,
            Elem.Opt(Elem.Cat("providesClause")),
            Elem.Opt(Elem.Cat("requiresClause")),
            Elem.Tok("{"), Elem.Star(Elem.Cat("item")), Elem.Tok("}"))))),
        CategorySpec("providesClause", List(
          ConstructorSpec("provides", List(Elem.Tok("provides"), Elem.SepBy1(Elem.AnyIdentLeaf, ","))))),
        CategorySpec("requiresClause", List(
          ConstructorSpec("requires", List(Elem.Tok("requires"), Elem.SepBy1(Elem.AnyIdentLeaf, ","))))),
        CategorySpec("item", List(
          ConstructorSpec("sortTree", List(Elem.Tok("sort"), Elem.AnyIdentLeaf, Elem.Tok("tree"), Elem.Tok(";"))),
          ConstructorSpec("sortGraph", List(Elem.Tok("sort"), Elem.AnyIdentLeaf, Elem.Tok("graph"), Elem.Tok(";"))),
          ConstructorSpec("ctorDecl", List(
            Elem.Tok("ctor"), Elem.AnyIdentLeaf, Elem.Tok(":"), Elem.AnyIdentLeaf,
            Elem.Opt(Elem.Cat("argList")), Elem.Star(Elem.Cat("bindsClause")), Elem.Tok(";"))),
          ConstructorSpec("varCtorDecl", List(Elem.Tok("varctor"), Elem.AnyIdentLeaf, Elem.Tok(";"))),
          ConstructorSpec("ruleDecl", List(
            Elem.Tok("rule"), Elem.AnyIdentLeaf, Elem.Tok(":"), Elem.Cat("pat"), Elem.Tok("=>"),
            Elem.Cat("pat"), Elem.Tok(";"))),
          ConstructorSpec("judgmentDecl", List(
            Elem.Tok("judgment"), Elem.AnyIdentLeaf, Elem.Tok("{"), Elem.Star(Elem.Cat("judgRule")), Elem.Tok("}"))),
          ConstructorSpec("topDecl", List(Elem.Tok("top"), Elem.AnyIdentLeaf, Elem.Tok(";"))))),
        CategorySpec("argList", List(
          ConstructorSpec("argList", List(Elem.Tok("("), Elem.SepBy1(Elem.AnyIdentLeaf, ","), Elem.Tok(")"))))),
        CategorySpec("bindsClause", List(
          ConstructorSpec("binds", List(
            Elem.Tok("binds"), Elem.NumLeaf, Elem.Tok("in"), Elem.SepBy1(Elem.NumLeaf, ","))))),
        CategorySpec("pat", List(
          ConstructorSpec("patMetaNode", List(
            Elem.Tok("$"), Elem.AnyIdentLeaf, Elem.Tok("("),
            Elem.Opt(Elem.SepBy1(Elem.Cat("pat"), ",")), Elem.Tok(")"))),
          ConstructorSpec("patMeta", List(Elem.Tok("$"), Elem.AnyIdentLeaf)),
          ConstructorSpec("patNode", List(
            Elem.AnyIdentLeaf, Elem.Tok("("),
            Elem.Opt(Elem.SepBy1(Elem.Cat("pat"), ",")), Elem.Tok(")"))),
          ConstructorSpec("patLeaf", List(Elem.AnyIdentLeaf)),
          ConstructorSpec("patStr", List(Elem.StrLeaf)))),
        CategorySpec("judgRule", List(
          ConstructorSpec("judgRule", List(
            Elem.Tok("rule"), Elem.AnyIdentLeaf, Elem.Tok(":"),
            Elem.Opt(Elem.SepBy1(Elem.Cat("pat"), ",")), Elem.Tok("|-"), Elem.Cat("pat"),
            Elem.Opt(Elem.Cat("whereClause")), Elem.Tok(";"))))),
        CategorySpec("whereClause", List(
          ConstructorSpec("whereC", List(Elem.Tok("where"), Elem.SepBy1(Elem.Cat("pat"), ",")))))),
      printRules = List(
        PrintRule("file", List(
          PrintSeg.Lit("language"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("{"),
          PrintSeg.Newline, PrintSeg.IndentIn, PrintSeg.SepFields(1, "\n"),
          PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
        PrintRule("surfaceFile", List(
          PrintSeg.Lit("surface"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("for"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit("{"),
          PrintSeg.Newline, PrintSeg.IndentIn, PrintSeg.SepFields(2, "\n"),
          PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
        PrintRule("fragmentDecl", List(
          PrintSeg.Lit("fragment"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Field(1), PrintSeg.Field(2),
          PrintSeg.Space, PrintSeg.Lit("{"), PrintSeg.Newline, PrintSeg.IndentIn,
          PrintSeg.SepFields(3, "\n"), PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
        PrintRule("provides", List(PrintSeg.Space, PrintSeg.Lit("provides"), PrintSeg.Space, PrintSeg.SepFields(0, ", "))),
        PrintRule("requires", List(PrintSeg.Space, PrintSeg.Lit("requires"), PrintSeg.Space, PrintSeg.SepFields(0, ", "))),
        PrintRule("sortTree", List(PrintSeg.Lit("sort"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("tree"), PrintSeg.Lit(";"))),
        PrintRule("sortGraph", List(PrintSeg.Lit("sort"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("graph"), PrintSeg.Lit(";"))),
        PrintRule("ctorDecl", List(
          PrintSeg.Lit("ctor"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
          PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Field(2), PrintSeg.SepFields(3, ""), PrintSeg.Lit(";"))),
        PrintRule("varCtorDecl", List(PrintSeg.Lit("varctor"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit(";"))),
        PrintRule("ruleDecl", List(
          PrintSeg.Lit("rule"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
          PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit("=>"), PrintSeg.Space,
          PrintSeg.Field(2), PrintSeg.Lit(";"))),
        PrintRule("judgmentDecl", List(
          PrintSeg.Lit("judgment"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("{"),
          PrintSeg.Newline, PrintSeg.IndentIn, PrintSeg.SepFields(1, "\n"),
          PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
        PrintRule("topDecl", List(PrintSeg.Lit("top"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit(";"))),
        PrintRule("argList", List(PrintSeg.Lit("("), PrintSeg.SepFields(0, ", "), PrintSeg.Lit(")"))),
        PrintRule("binds", List(
          PrintSeg.Space, PrintSeg.Lit("binds"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("in"), PrintSeg.Space, PrintSeg.SepFields(1, ", "))),
        PrintRule("patMetaNode", List(
          PrintSeg.Lit("$"), PrintSeg.Field(0), PrintSeg.Lit("("), PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("patMeta", List(PrintSeg.Lit("$"), PrintSeg.Field(0))),
        PrintRule("patNode", List(PrintSeg.Field(0), PrintSeg.Lit("("), PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("patLeaf", List(PrintSeg.Field(0))),
        PrintRule("patStr", List(PrintSeg.StrField(0))),
        PrintRule("judgRule", List(
          PrintSeg.Lit("rule"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
          PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit("|-"), PrintSeg.Space,
          PrintSeg.Field(2), PrintSeg.Field(3), PrintSeg.Lit(";"))),
        PrintRule("whereC", List(
          PrintSeg.Space, PrintSeg.Lit("where"), PrintSeg.Space, PrintSeg.SepFields(0, ", ")))),
      top = Some("file")))

  /** The grammar-authoring vocabulary — `syntax`/`print`/`infix`/`tok`/`cat`/…
    * (the same shape as [[Elem]]/[[PrintSeg]] in `kernel.Grammar`), split out
    * of the fused meta fragment so it's independently self-describing: this
    * is the "grammar-language" of the primordial bootstrap pair (CAIRN-
    * PROMPT.md §2b), and every `surfaces` directory's `.cairn` files across
    * every shipped language already use exactly this vocabulary to declare
    * custom syntax.
    * Extends [[fragment]]'s `item` category additively (same amalgamation
    * mechanism any two ordinary fragments use to jointly extend a shared
    * category, e.g. STLC's `term`) rather than requiring any new composition
    * machinery.
    */
  val grammarFragment: Fragment = Fragment(
    name = "grammar",
    provides = List("grammar"),
    requires = Nil,
    grammar = GrammarPart(
      keywords = List("keyword", "punct", "identcont", "syntax", "print",
        "infix", "over", "tag", "prec", "left", "right",
        "tok", "tokfield", "name", "anyident", "num", "str", "restofline", "cat", "opt",
        "star", "sepby1", "block", "run", "adjacent1",
        "lit", "sp", "nl", "indent", "dedent", "field", "strfield", "sep"),
      puncts = List("+=", ":", ";"),
      categories = List(
        CategorySpec("item", List(
          ConstructorSpec("keywordDecl", List(Elem.Tok("keyword"), Elem.AnyIdentLeaf, Elem.Tok(";"))),
          ConstructorSpec("punctDecl", List(Elem.Tok("punct"), Elem.StrLeaf, Elem.Tok(";"))),
          ConstructorSpec("identContDecl", List(Elem.Tok("identcont"), Elem.StrLeaf, Elem.Tok(";"))),
          ConstructorSpec("syntaxDecl", List(
            Elem.Tok("syntax"), Elem.AnyIdentLeaf, Elem.Tok("+="), Elem.AnyIdentLeaf, Elem.Tok(":"),
            Elem.Star(Elem.Cat("elem")), Elem.Tok(";"))),
          ConstructorSpec("printDecl", List(
            Elem.Tok("print"), Elem.AnyIdentLeaf, Elem.Tok(":"), Elem.Star(Elem.Cat("seg")), Elem.Tok(";"))),
          ConstructorSpec("infixDecl", List(
            Elem.Tok("infix"), Elem.AnyIdentLeaf, Elem.Tok("over"), Elem.AnyIdentLeaf, Elem.Tok(":"),
            Elem.StrLeaf, Elem.Tok("tag"), Elem.AnyIdentLeaf, Elem.Tok("prec"), Elem.NumLeaf,
            Elem.Cat("assoc"), Elem.Tok(";"))))),
        CategorySpec("assoc", List(
          ConstructorSpec("assocLeft", List(Elem.Tok("left"))),
          ConstructorSpec("assocRight", List(Elem.Tok("right"))))),
        CategorySpec("elem", List(
          ConstructorSpec("elemTok", List(Elem.Tok("tok"), Elem.StrLeaf)),
          ConstructorSpec("elemTokField", List(Elem.Tok("tokfield"), Elem.StrLeaf)),
          ConstructorSpec("elemName", List(Elem.Tok("name"))),
          ConstructorSpec("elemAnyIdent", List(Elem.Tok("anyident"))),
          ConstructorSpec("elemNum", List(Elem.Tok("num"))),
          ConstructorSpec("elemStr", List(Elem.Tok("str"))),
          ConstructorSpec("elemRest", List(Elem.Tok("restofline"))),
          ConstructorSpec("elemCat", List(Elem.Tok("cat"), Elem.AnyIdentLeaf)),
          ConstructorSpec("elemOpt", List(Elem.Tok("opt"), Elem.Cat("elem"))),
          ConstructorSpec("elemStar", List(Elem.Tok("star"), Elem.Cat("elem"))),
          ConstructorSpec("elemSepBy1", List(Elem.Tok("sepby1"), Elem.Cat("elem"), Elem.StrLeaf)),
          ConstructorSpec("elemBlock", List(Elem.Tok("block"), Elem.AnyIdentLeaf)),
          ConstructorSpec("elemRun", List(Elem.Tok("run"), Elem.AnyIdentLeaf)),
          ConstructorSpec("elemAdj", List(Elem.Tok("adjacent1"), Elem.Cat("elem"))))),
        CategorySpec("seg", List(
          ConstructorSpec("segLit", List(Elem.Tok("lit"), Elem.StrLeaf)),
          ConstructorSpec("segSp", List(Elem.Tok("sp"))),
          ConstructorSpec("segNl", List(Elem.Tok("nl"))),
          ConstructorSpec("segIn", List(Elem.Tok("indent"))),
          ConstructorSpec("segOut", List(Elem.Tok("dedent"))),
          ConstructorSpec("segField", List(Elem.Tok("field"), Elem.NumLeaf)),
          ConstructorSpec("segStrField", List(Elem.Tok("strfield"), Elem.NumLeaf)),
          ConstructorSpec("segSep", List(Elem.Tok("sep"), Elem.NumLeaf, Elem.StrLeaf))))),
      printRules = List(
        PrintRule("keywordDecl", List(PrintSeg.Lit("keyword"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit(";"))),
        PrintRule("punctDecl", List(PrintSeg.Lit("punct"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Lit(";"))),
        PrintRule("identContDecl", List(PrintSeg.Lit("identcont"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Lit(";"))),
        PrintRule("syntaxDecl", List(
          PrintSeg.Lit("syntax"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("+="),
          PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(":"), PrintSeg.Space,
          PrintSeg.SepFields(2, " "), PrintSeg.Lit(";"))),
        PrintRule("printDecl", List(
          PrintSeg.Lit("print"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
          PrintSeg.Space, PrintSeg.SepFields(1, " "), PrintSeg.Lit(";"))),
        PrintRule("infixDecl", List(
          PrintSeg.Lit("infix"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("over"),
          PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(":"), PrintSeg.Space,
          PrintSeg.StrField(2), PrintSeg.Space, PrintSeg.Lit("tag"), PrintSeg.Space, PrintSeg.Field(3),
          PrintSeg.Space, PrintSeg.Lit("prec"), PrintSeg.Space, PrintSeg.Field(4), PrintSeg.Space,
          PrintSeg.Field(5), PrintSeg.Lit(";"))),
        PrintRule("assocLeft", List(PrintSeg.Lit("left"))),
        PrintRule("assocRight", List(PrintSeg.Lit("right"))),
        PrintRule("elemTok", List(PrintSeg.Lit("tok"), PrintSeg.Space, PrintSeg.StrField(0))),
        PrintRule("elemTokField", List(PrintSeg.Lit("tokfield"), PrintSeg.Space, PrintSeg.StrField(0))),
        PrintRule("elemName", List(PrintSeg.Lit("name"))),
        PrintRule("elemAnyIdent", List(PrintSeg.Lit("anyident"))),
        PrintRule("elemNum", List(PrintSeg.Lit("num"))),
        PrintRule("elemStr", List(PrintSeg.Lit("str"))),
        PrintRule("elemRest", List(PrintSeg.Lit("restofline"))),
        PrintRule("elemCat", List(PrintSeg.Lit("cat"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("elemOpt", List(PrintSeg.Lit("opt"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("elemStar", List(PrintSeg.Lit("star"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("elemSepBy1", List(PrintSeg.Lit("sepby1"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.StrField(1))),
        PrintRule("elemBlock", List(PrintSeg.Lit("block"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("elemRun", List(PrintSeg.Lit("run"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("elemAdj", List(PrintSeg.Lit("adjacent1"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("segLit", List(PrintSeg.Lit("lit"), PrintSeg.Space, PrintSeg.StrField(0))),
        PrintRule("segSp", List(PrintSeg.Lit("sp"))),
        PrintRule("segNl", List(PrintSeg.Lit("nl"))),
        PrintRule("segIn", List(PrintSeg.Lit("indent"))),
        PrintRule("segOut", List(PrintSeg.Lit("dedent"))),
        PrintRule("segField", List(PrintSeg.Lit("field"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("segStrField", List(PrintSeg.Lit("strfield"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("segSep", List(PrintSeg.Lit("sep"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.StrField(1)))),
      top = None))

  lazy val language: ComposedLanguage =
    Compose.compose("meta", List(fragment, grammarFragment)).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")), identity)

  lazy val grammar: GrammarSpec = language.grammar

  /** `grammarFragment` composed alone — mirrors [[language]], for consumers
    * (tests, tooling) that want the standalone "grammar" pack's identity.
    */
  lazy val grammarLanguage: ComposedLanguage =
    Compose.compose("grammar", List(grammarFragment)).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")), identity)

  // ======================= elaboration (Cst -> Fragment) =======================

  private def names(c: Cst): List[String] = c match
    case Cst.Node("list", items) => items.collect { case Cst.Leaf(x) => x }
    case Cst.Node("some", List(inner)) => names(inner)
    case Cst.Node("none", _) => Nil
    case Cst.Leaf(x) => List(x)
    case _ => Nil

  private def optItems(c: Cst): List[Cst] = c match
    case Cst.Node("some", List(Cst.Node("list", items))) => items
    case Cst.Node("some", List(single)) => List(single)
    case _ => Nil

  def patToCst(p: Cst): Either[String, Cst] = p match
    case Cst.Node("patMeta", List(Cst.Leaf(x)))     => Right(leaf(s"$$$x"))
    case Cst.Node("patLeaf", List(Cst.Leaf(x)))     => Right(leaf(x))
    case Cst.Node("patStr", List(Cst.Leaf(x)))      => Right(leaf(x))
    case Cst.Node("patNode", List(Cst.Leaf(t), args)) =>
      seq(optItems(args).map(patToCst)).map(Cst.Node(t, _))
    case Cst.Node("patMetaNode", List(Cst.Leaf(t), args)) =>
      seq(optItems(args).map(patToCst)).map(Cst.Node(s"$$$t", _))
    case other => Left(s"not a pattern: ${other.render}")

  private def seq[A](xs: List[Either[String, A]]): Either[String, List[A]] =
    xs.foldLeft[Either[String, List[A]]](Right(Nil)) { (acc, x) =>
      for as <- acc; a <- x yield as :+ a }

  def elemToKernel(e: Cst): Either[String, Elem] = e match
    case Cst.Node("elemTok", List(Cst.Leaf(s)))      => Right(Elem.Tok(s))
    case Cst.Node("elemTokField", List(Cst.Leaf(s))) => Right(Elem.TokField(s))
    case Cst.Node("elemName", _)      => Right(Elem.NameLeaf)
    case Cst.Node("elemAnyIdent", _)  => Right(Elem.AnyIdentLeaf)
    case Cst.Node("elemNum", _)       => Right(Elem.NumLeaf)
    case Cst.Node("elemStr", _)       => Right(Elem.StrLeaf)
    case Cst.Node("elemRest", _)      => Right(Elem.RestOfLine)
    case Cst.Node("elemCat", List(Cst.Leaf(c)))   => Right(Elem.Cat(c))
    case Cst.Node("elemOpt", List(inner))         => elemToKernel(inner).map(Elem.Opt.apply)
    case Cst.Node("elemStar", List(inner))        => elemToKernel(inner).map(Elem.Star.apply)
    case Cst.Node("elemSepBy1", List(inner, Cst.Leaf(sep))) => elemToKernel(inner).map(Elem.SepBy1(_, sep))
    case Cst.Node("elemBlock", List(Cst.Leaf(c))) => Right(Elem.Block(c))
    case Cst.Node("elemRun", List(Cst.Leaf(c)))   => Right(Elem.Run(c))
    case Cst.Node("elemAdj", List(inner))         => elemToKernel(inner).map(Elem.Adjacent1.apply)
    case other => Left(s"not an elem: ${other.render}")

  def segToKernel(s: Cst): Either[String, PrintSeg] = s match
    case Cst.Node("segLit", List(Cst.Leaf(t)))      => Right(PrintSeg.Lit(t))
    case Cst.Node("segSp", _)                       => Right(PrintSeg.Space)
    case Cst.Node("segNl", _)                       => Right(PrintSeg.Newline)
    case Cst.Node("segIn", _)                       => Right(PrintSeg.IndentIn)
    case Cst.Node("segOut", _)                      => Right(PrintSeg.IndentOut)
    case Cst.Node("segField", List(Cst.Leaf(i)))    => Right(PrintSeg.Field(i.toInt))
    case Cst.Node("segStrField", List(Cst.Leaf(i))) => Right(PrintSeg.StrField(i.toInt))
    case Cst.Node("segSep", List(Cst.Leaf(i), Cst.Leaf(sep))) => Right(PrintSeg.SepFields(i.toInt, sep))
    case other => Left(s"not a print seg: ${other.render}")

  private def tagOf(t: String): String = if t == "group" then "$group" else t

  def elaborateFragment(cst: Cst): Either[String, Fragment] = cst match
    case Cst.Node("fragmentDecl", List(Cst.Leaf(name), providesOpt, requiresOpt, Cst.Node("list", items))) =>
      def clause(o: Cst, tag: String): List[String] = o match
        case Cst.Node("some", List(Cst.Node(`tag`, List(ns)))) => names(ns)
        case _ => Nil
      val sorts = List.newBuilder[SortDef]
      val ctors = List.newBuilder[CtorDef]
      var varCtor: Option[String] = None
      val keywords = List.newBuilder[String]
      val puncts = List.newBuilder[String]
      var identCont = ""
      // categories in first-encounter order, alternatives in decl order
      val catOrder = List.newBuilder[String]
      val catAlts = scala.collection.mutable.Map[String, List[ConstructorSpec]]()
      val printRules = List.newBuilder[PrintRule]
      val infixOrder = List.newBuilder[(String, String)]
      val infixOps = scala.collection.mutable.Map[(String, String), List[InfixOp]]()
      val rules = List.newBuilder[RewriteRule]
      val judgments = List.newBuilder[JudgmentDef]
      var top: Option[String] = None
      val err = new StringBuilder

      for item <- items do item match
        case Cst.Node("sortTree", List(Cst.Leaf(s)))  => sorts += SortDef(s, SortMode.Tree)
        case Cst.Node("sortGraph", List(Cst.Leaf(s))) => sorts += SortDef(s, SortMode.Graph)
        case Cst.Node("ctorDecl", List(Cst.Leaf(c), Cst.Leaf(sort), argsOpt, bindsList)) =>
          val args = argsOpt match
            case Cst.Node("some", List(Cst.Node("argList", List(ns)))) => names(ns)
            case _ => Nil
          val binders = bindsList match
            case Cst.Node("list", bs) => bs.collect {
              case Cst.Node("binds", List(Cst.Leaf(bi), scope)) => (bi.toInt, names(scope).map(_.toInt)) }
            case _ => Nil
          ctors += CtorDef(c, sort, args, binders)
        case Cst.Node("varCtorDecl", List(Cst.Leaf(v)))  => varCtor = Some(v)
        case Cst.Node("keywordDecl", List(Cst.Leaf(k)))  => keywords += k
        case Cst.Node("punctDecl", List(Cst.Leaf(p)))    => puncts += p
        case Cst.Node("identContDecl", List(Cst.Leaf(s))) => identCont += s
        case Cst.Node("syntaxDecl", List(Cst.Leaf(cat), Cst.Leaf(tag), Cst.Node("list", elems))) =>
          seq(elems.map(elemToKernel)) match
            case Right(es) =>
              if !catAlts.contains(cat) then catOrder += cat
              catAlts(cat) = catAlts.getOrElse(cat, Nil) :+ ConstructorSpec(tagOf(tag), es)
            case Left(e) => err ++= e
        case Cst.Node("printDecl", List(Cst.Leaf(tag), Cst.Node("list", segs))) =>
          seq(segs.map(segToKernel)) match
            case Right(ss) => printRules += PrintRule(tagOf(tag), ss)
            case Left(e)   => err ++= e
        case Cst.Node("infixDecl", List(Cst.Leaf(cat), Cst.Leaf(base), Cst.Leaf(text), Cst.Leaf(tag), Cst.Leaf(prec), assocN)) =>
          val rightAssoc = assocN match
            case Cst.Node("assocRight", _) => true
            case _                          => false
          val key = (cat, base)
          if !infixOps.contains(key) then infixOrder += key
          infixOps(key) = infixOps.getOrElse(key, Nil) :+ InfixOp(text, tag, prec.toInt, rightAssoc)
        case Cst.Node("ruleDecl", List(Cst.Leaf(rn), patP, tmplP)) =>
          (for p <- patToCst(patP); t <- patToCst(tmplP) yield RewriteRule(rn, p, t)) match
            case Right(r) => rules += r
            case Left(e)  => err ++= e
        case Cst.Node("judgmentDecl", List(Cst.Leaf(jn), Cst.Node("list", rs))) =>
          val inferRules = seq(rs.map {
            case Cst.Node("judgRule", List(Cst.Leaf(rn), premsOpt, concP, whereOpt)) =>
              for
                prems <- seq(optItems(premsOpt).map(patToCst))
                conc <- patToCst(concP)
                conds <- whereOpt match
                  case Cst.Node("some", List(Cst.Node("whereC", List(ws)))) =>
                    seq((ws match
                      case Cst.Node("list", items) => items
                      case single                  => List(single)).map(patToCst))
                  case _ => Right(Nil)
              yield InferRule(rn, prems, conc, conds)
            case other => Left(s"not a judgment rule: ${other.render}")
          })
          inferRules match
            case Right(irs) => judgments += JudgmentDef(jn, irs)
            case Left(e)    => err ++= e
        case Cst.Node("topDecl", List(Cst.Leaf(t))) => top = Some(t)
        case other => err ++= s"unknown item: ${other.render}; "

      if err.nonEmpty then Left(err.result())
      else Right(Fragment(
        name = name,
        provides = clause(providesOpt, "provides"),
        requires = clause(requiresOpt, "requires"),
        sorts = sorts.result(),
        constructors = ctors.result(),
        grammar = GrammarPart(
          keywords = keywords.result(),
          puncts = puncts.result(),
          categories = catOrder.result().map(c => CategorySpec(c, catAlts(c))),
          precCategories = infixOrder.result().map((c, b) => PrecCategory(c, b, infixOps((c, b)))),
          printRules = printRules.result(),
          top = top,
          identContExtra = identCont),
        rewriteRules = rules.result(),
        judgments = judgments.result(),
        varCtor = varCtor))
    case other => Left(s"not a fragment declaration: ${other.render}")

  def parseFragment(src: String): Either[String, Fragment] =
    Parser.parse(grammar.copy(top = "fragmentDecl"), src).flatMap(elaborateFragment)

  /** Parse a `language NAME { fragment* }` file into name + fragments (no compose).
    * Use [[PackLoader.close]] when fragments may `requires` interfaces from other packs.
    */
  def parseLanguageAst(src: String): Either[String, (String, List[Fragment])] =
    Parser.parse(grammar, src).flatMap {
      case Cst.Node("file", List(Cst.Leaf(name), Cst.Node("list", frags))) =>
        seq(frags.map(elaborateFragment)).map(name -> _)
      case Cst.Node("surfaceFile", _) =>
        Left("expected language file, got surface file (use parseSurfaceAst)")
      case other => Left(s"not a language file: ${other.render}")
    }

  /** Parse a `surface STYLE for LANG { fragment* }` file (Phase 3). */
  def parseSurfaceAst(src: String): Either[String, (String, String, List[Fragment])] =
    Parser.parse(grammar, src).flatMap {
      case Cst.Node("surfaceFile", List(Cst.Leaf(style), Cst.Leaf(lang), Cst.Node("list", frags))) =>
        seq(frags.map(elaborateFragment)).map((style, lang, _))
      case Cst.Node("file", _) =>
        Left("expected surface file, got language file (use parseLanguageAst)")
      case other => Left(s"not a surface file: ${other.render}")
    }

  /** Parse a self-contained `language NAME { fragment* }` file into a composed language. */
  def parseFile(src: String): Either[String, ComposedLanguage] =
    parseLanguageAst(src).flatMap { (name, fs) =>
      Compose.compose(name, fs).left.map(_.map(_.render).mkString("\n")) }

  // ======================= encoding (Fragment -> Cst) =======================

  def cstToPat(t: Cst): Cst = t match
    case Cst.Leaf(x) if x.startsWith("$") => n("patMeta", leaf(x.drop(1)))
    case Cst.Leaf(x) if x.nonEmpty && x.forall(ch => ch.isLetterOrDigit || "_'-".contains(ch)) && !x.head.isDigit =>
      n("patLeaf", leaf(x))
    case Cst.Leaf(x) => n("patStr", leaf(x))
    case Cst.Node(tag, cs) if tag.startsWith("$") =>
      n("patMetaNode", leaf(tag.drop(1)), optList(cs.map(cstToPat)))
    case Cst.Node(tag, cs) =>
      n("patNode", leaf(tag), optList(cs.map(cstToPat)))

  def elemToCst(e: Elem): Cst = e match
    case Elem.Tok(s)       => n("elemTok", leaf(s))
    case Elem.TokField(s)  => n("elemTokField", leaf(s))
    case Elem.NameLeaf     => n("elemName")
    case Elem.AnyIdentLeaf => n("elemAnyIdent")
    case Elem.NumLeaf      => n("elemNum")
    case Elem.StrLeaf      => n("elemStr")
    case Elem.RestOfLine   => n("elemRest")
    case Elem.Cat(c)       => n("elemCat", leaf(c))
    case Elem.Opt(i)       => n("elemOpt", elemToCst(i))
    case Elem.Star(i)      => n("elemStar", elemToCst(i))
    case Elem.SepBy1(i, s) => n("elemSepBy1", elemToCst(i), leaf(s))
    case Elem.Block(c)     => n("elemBlock", leaf(c))
    case Elem.Run(c)       => n("elemRun", leaf(c))
    case Elem.Adjacent1(i) => n("elemAdj", elemToCst(i))

  def segToCst(s: PrintSeg): Cst = s match
    case PrintSeg.Lit(t)          => n("segLit", leaf(t))
    case PrintSeg.Space           => n("segSp")
    case PrintSeg.Newline         => n("segNl")
    case PrintSeg.IndentIn        => n("segIn")
    case PrintSeg.IndentOut       => n("segOut")
    case PrintSeg.Field(i)        => n("segField", leaf(i.toString))
    case PrintSeg.StrField(i)     => n("segStrField", leaf(i.toString))
    case PrintSeg.SepFields(i, s) => n("segSep", leaf(i.toString), leaf(s))

  private def untag(t: String): String = if t == "$group" then "group" else t

  /** Encode a Fragment as a meta-surface Cst (inverse of elaborateFragment). */
  def encode(f: Fragment): Cst =
    val items = List.newBuilder[Cst]
    for s <- f.sorts do
      items += (if s.mode == SortMode.Tree then n("sortTree", leaf(s.name)) else n("sortGraph", leaf(s.name)))
    for c <- f.constructors do
      items += n("ctorDecl", leaf(c.name), leaf(c.sort),
        if c.argSorts.isEmpty then n("none") else n("some", n("argList", lst(c.argSorts.map(leaf)))),
        lst(c.binders.map((bi, scope) => n("binds", leaf(bi.toString), lst(scope.map(i => leaf(i.toString)))))))
    for v <- f.varCtor do items += n("varCtorDecl", leaf(v))
    for k <- f.grammar.keywords do items += n("keywordDecl", leaf(k))
    for p <- f.grammar.puncts do items += n("punctDecl", leaf(p))
    if f.grammar.identContExtra.nonEmpty then items += n("identContDecl", leaf(f.grammar.identContExtra))
    for cat <- f.grammar.categories; alt <- cat.ctors do
      items += n("syntaxDecl", leaf(cat.name), leaf(untag(alt.tag)), lst(alt.elems.map(elemToCst)))
    for pc <- f.grammar.precCategories; op <- pc.ops do
      items += n("infixDecl", leaf(pc.name), leaf(pc.base), leaf(op.text), leaf(op.tag),
        leaf(op.prec.toString), if op.rightAssoc then n("assocRight") else n("assocLeft"))
    for pr <- f.grammar.printRules do
      items += n("printDecl", leaf(untag(pr.tag)), lst(pr.segs.map(segToCst)))
    for r <- f.rewriteRules do
      items += n("ruleDecl", leaf(r.name), cstToPat(r.pattern), cstToPat(r.template))
    for j <- f.judgments do
      items += n("judgmentDecl", leaf(j.name), lst(j.rules.map { ir =>
        n("judgRule", leaf(ir.name),
          optList(ir.premises.map(cstToPat)),
          cstToPat(ir.conclusion),
          if ir.conditions.isEmpty then n("none")
          else n("some", n("whereC", lst(ir.conditions.map(cstToPat))))) }))
    for t <- f.grammar.top do items += n("topDecl", leaf(t))
    n("fragmentDecl", leaf(f.name),
      if f.provides.isEmpty then n("none") else n("some", n("provides", lst(f.provides.map(leaf)))),
      if f.requires.isEmpty then n("none") else n("some", n("requires", lst(f.requires.map(leaf)))),
      lst(items.result()))

  def encodeLanguage(name: String, fragments: List[Fragment]): Cst =
    n("file", leaf(name), lst(fragments.map(encode)))

  def encodeSurface(name: String, language: String, fragments: List[Fragment]): Cst =
    n("surfaceFile", leaf(name), leaf(language), lst(fragments.map(encode)))

  /** Render a whole language as meta-surface text. */
  def printLanguage(name: String, fragments: List[Fragment]): Either[String, String] =
    Printer.print(grammar, encodeLanguage(name, fragments))

  /** Render a surface pack as `surface NAME for LANG { … }` text. */
  def printSurface(name: String, language: String, fragments: List[Fragment]): Either[String, String] =
    Printer.print(grammar, encodeSurface(name, language, fragments))

  /** Format-preserving regeneration: reprints ONLY the declarations that
    * actually changed, splicing original source bytes — comments included,
    * via each item's leading trivia (same primitive as `Concrete.put`) —
    * for every declaration whose canonical content is unchanged from
    * `currentText`. Scoped to the one shape every checked-in exemplar
    * (`languages/{pki,law,sds,search}.cairn`) actually has: one `language`
    * block, one `fragment` block. Falls back to a full canonical reprint
    * (identical to [[printLanguage]]) whenever that shape doesn't hold, or
    * a fragment's declaration COUNT changed (an add/remove) — an honest
    * degradation: preserving trivia across an insertion needs knowing which
    * comment belongs to which SURVIVING declaration, which is genuinely
    * ambiguous, not a case worth guessing at.
    */
  def printLanguagePreservingFormat(name: String, fragments: List[Fragment], currentText: String): Either[String, String] =
    def fullReprint: Either[String, String] = printLanguage(name, fragments)
    if fragments.length != 1 then fullReprint
    else
      Parser.parseFull(grammar, currentText) match
        case Left(_) => fullReprint
        case Right(out) =>
          (out.cst, encodeLanguage(name, fragments)) match
            case (Cst.Node("file", List(Cst.Leaf(n2), Cst.Node("list", List(origFragCst)))),
                  Cst.Node("file", List(_, Cst.Node("list", List(canonFragCst))))) if n2 == name =>
              (origFragCst, canonFragCst) match
                case (Cst.Node("fragmentDecl", List(_, _, _, Cst.Node("list", origItems))),
                      Cst.Node("fragmentDecl", List(_, _, _, Cst.Node("list", canonItems))))
                    if origItems.length == canonItems.length =>
                  spliceItems(out, currentText, origItems, canonItems) match
                    case Left(_) => fullReprint
                    case Right(itemsText) =>
                      val f = fragments.head
                      val prov = if f.provides.isEmpty then "" else s" provides ${f.provides.mkString(", ")}"
                      val req = if f.requires.isEmpty then "" else s" requires ${f.requires.mkString(", ")}"
                      val leading = out.tokens.headOption.map(_.leading).getOrElse("")
                      // no trailing newline after the final "}" — matches printLanguage's
                      // own file PrintRule, whose last segment is a bare Lit("}")
                      Right(s"$leading" + s"language $name {\n  fragment ${f.name}$prov$req {" +
                        itemsText + "\n  }\n}")
                case _ => fullReprint
            case _ => fullReprint

  /** One spliced-or-freshly-printed piece per item, each carrying its own
    * leading separator (spliced pieces bring their original "\n" + indent +
    * any comment verbatim; fresh pieces get a matching "\n    " manually) —
    * concatenating them needs no join separator of its own. Splices from
    * `out`/`source` (only that side has real spans — `canonItems` is
    * freshly constructed by `encode`, never parsed from anything); prints
    * the OTHER side (`canonItems`) when an item differs, since that's the
    * intended new content.
    */
  private def spliceItems(out: ParseOut, source: String, origItems: List[Cst], canonItems: List[Cst]): Either[String, String] =
    seq(origItems.zip(canonItems).map { (oi, ci) =>
      if oi == ci then
        out.spans.get(oi).toRight("spliceItems: item has no recorded span").map { (startTok, endTok) =>
          val prev = out.tokens(startTok - 1) // always > 0: items are never the file's first token
          val startOff = prev.offset + prev.rawLen
          val endOff = if endTok == 0 then startOff else { val last = out.tokens(endTok - 1); last.offset + last.rawLen }
          source.substring(startOff, endOff)
        }
      else
        Printer.print(grammar, ci).map(t => "\n    " + t.replace("\n", "\n    "))
    }).map(_.mkString)

  /** Same idea as [[spliceItems]], but for comparing two PARSED texts rather
    * than fragments-vs-text: `workOut`/`workingText` is both the comparison
    * target AND the only side with real spans to splice from; `refItems`
    * (from a separate baseline parse, e.g. git HEAD) is comparison-only.
    * When an item differs, the WORK item is printed (canonically — printing
    * a Cst is always canonical regardless of how it was typed), since work
    * is the intended new content here, not ref.
    */
  private def spliceItemsVsReference(workOut: ParseOut, workingText: String,
                                     refItems: List[Cst], workItems: List[Cst]): Either[String, String] =
    seq(refItems.zip(workItems).map { (ref, work) =>
      if ref == work then
        workOut.spans.get(work).toRight("spliceItemsVsReference: item has no recorded span").map { (startTok, endTok) =>
          val prev = workOut.tokens(startTok - 1)
          val startOff = prev.offset + prev.rawLen
          val endOff = if endTok == 0 then startOff else { val last = workOut.tokens(endTok - 1); last.offset + last.rawLen }
          workingText.substring(startOff, endOff)
        }
      else
        Printer.print(grammar, work).map(t => "\n    " + t.replace("\n", "\n    "))
    }).map(_.mkString)

  private def optClauseNames(c: Cst): List[String] = c match
    case Cst.Node("some", List(Cst.Node(_, List(Cst.Node("list", items))))) =>
      items.collect { case Cst.Leaf(n) => n }
    case _ => Nil

  /** Format-preserving regeneration against a SEPARATE baseline text (e.g.
    * git HEAD's checked-in version), for the case where the working text
    * itself — not an independent Scala source — already carries whatever
    * edits (and any newly-added comments) should be kept: only declarations
    * that differ from `referenceText` get canonically reprinted; everything
    * identical to the reference is spliced verbatim from `workingText`,
    * comments included. This is what keeps it sound as a companion to CI's
    * canonical-form check: a declaration being edited always gets a fresh
    * canonical reprint no matter how it's typed, so only content that
    * ALREADY passed that check at the reference commit gets to keep its
    * formatting — nothing new can sneak through non-canonical.
    *
    * Same scoping and fallback rules as [[printLanguagePreservingFormat]]
    * (single fragment, matching declaration counts; full reprint of
    * `workingText`'s own content otherwise).
    */
  def printLanguagePreservingFormatVsReference(name: String, workingText: String, referenceText: String): Either[String, String] =
    def fullReprint: Either[String, String] =
      parseLanguageAst(workingText).flatMap((n2, fs) => printLanguage(n2, fs))
    (Parser.parseFull(grammar, workingText), Parser.parseFull(grammar, referenceText)) match
      case (Right(workOut), Right(refOut)) =>
        (workOut.cst, refOut.cst) match
          case (Cst.Node("file", List(Cst.Leaf(n2), Cst.Node("list", List(workFragCst)))),
                Cst.Node("file", List(Cst.Leaf(n3), Cst.Node("list", List(refFragCst))))) if n2 == name && n3 == name =>
            (workFragCst, refFragCst) match
              case (Cst.Node("fragmentDecl", List(Cst.Leaf(fname), provCst, reqCst, Cst.Node("list", workItems))),
                    Cst.Node("fragmentDecl", List(_, _, _, Cst.Node("list", refItems))))
                  if workItems.length == refItems.length =>
                spliceItemsVsReference(workOut, workingText, refItems, workItems) match
                  case Left(_) => fullReprint
                  case Right(itemsText) =>
                    val provides = optClauseNames(provCst)
                    val requires = optClauseNames(reqCst)
                    val prov = if provides.isEmpty then "" else s" provides ${provides.mkString(", ")}"
                    val req = if requires.isEmpty then "" else s" requires ${requires.mkString(", ")}"
                    val leading = workOut.tokens.headOption.map(_.leading).getOrElse("")
                    Right(s"$leading" + s"language $name {\n  fragment $fname$prov$req {" + itemsText + "\n  }\n}")
              case _ => fullReprint
          case _ => fullReprint
      case _ => fullReprint

  /** Format-preserving surface regeneration (Fragment source vs on-disk text). */
  def printSurfacePreservingFormat(
      name: String, language: String, fragments: List[Fragment], currentText: String,
  ): Either[String, String] =
    def fullReprint: Either[String, String] = printSurface(name, language, fragments)
    if fragments.length != 1 then fullReprint
    else
      Parser.parseFull(grammar, currentText) match
        case Left(_) => fullReprint
        case Right(out) =>
          (out.cst, encodeSurface(name, language, fragments)) match
            case (Cst.Node("surfaceFile", List(Cst.Leaf(n2), Cst.Leaf(l2), Cst.Node("list", List(origFragCst)))),
                  Cst.Node("surfaceFile", List(_, _, Cst.Node("list", List(canonFragCst)))))
                if n2 == name && l2 == language =>
              (origFragCst, canonFragCst) match
                case (Cst.Node("fragmentDecl", List(_, _, _, Cst.Node("list", origItems))),
                      Cst.Node("fragmentDecl", List(_, _, _, Cst.Node("list", canonItems))))
                    if origItems.length == canonItems.length =>
                  spliceItems(out, currentText, origItems, canonItems) match
                    case Left(_) => fullReprint
                    case Right(itemsText) =>
                      val f = fragments.head
                      val prov = if f.provides.isEmpty then "" else s" provides ${f.provides.mkString(", ")}"
                      val req = if f.requires.isEmpty then "" else s" requires ${f.requires.mkString(", ")}"
                      val leading = out.tokens.headOption.map(_.leading).getOrElse("")
                      Right(s"$leading" + s"surface $name for $language {\n  fragment ${f.name}$prov$req {" +
                        itemsText + "\n  }\n}")
                case _ => fullReprint
            case _ => fullReprint

  /** Format-preserving surface regeneration against a separate baseline (e.g. git HEAD). */
  def printSurfacePreservingFormatVsReference(
      name: String, language: String, workingText: String, referenceText: String,
  ): Either[String, String] =
    def fullReprint: Either[String, String] =
      parseSurfaceAst(workingText).flatMap((n2, l2, fs) => printSurface(n2, l2, fs))
    (Parser.parseFull(grammar, workingText), Parser.parseFull(grammar, referenceText)) match
      case (Right(workOut), Right(refOut)) =>
        (workOut.cst, refOut.cst) match
          case (Cst.Node("surfaceFile", List(Cst.Leaf(n2), Cst.Leaf(l2), Cst.Node("list", List(workFragCst)))),
                Cst.Node("surfaceFile", List(Cst.Leaf(n3), Cst.Leaf(l3), Cst.Node("list", List(refFragCst)))))
              if n2 == name && n3 == name && l2 == language && l3 == language =>
            (workFragCst, refFragCst) match
              case (Cst.Node("fragmentDecl", List(Cst.Leaf(fname), provCst, reqCst, Cst.Node("list", workItems))),
                    Cst.Node("fragmentDecl", List(_, _, _, Cst.Node("list", refItems))))
                  if workItems.length == refItems.length =>
                spliceItemsVsReference(workOut, workingText, refItems, workItems) match
                  case Left(_) => fullReprint
                  case Right(itemsText) =>
                    val provides = optClauseNames(provCst)
                    val requires = optClauseNames(reqCst)
                    val prov = if provides.isEmpty then "" else s" provides ${provides.mkString(", ")}"
                    val req = if requires.isEmpty then "" else s" requires ${requires.mkString(", ")}"
                    val leading = workOut.tokens.headOption.map(_.leading).getOrElse("")
                    Right(s"$leading" + s"surface $name for $language {\n  fragment $fname$prov$req {" +
                      itemsText + "\n  }\n}")
              case _ => fullReprint
          case _ => fullReprint
      case _ => fullReprint
