package cairn.workbench

import cairn.kernel.*

/** Self-description bootstrap (S44, §2b, §6 Phase 7).
  *
  * Cairn's own fragment IR expressed as a Cairn language: a grammar (in the
  * generic grammar engine) for fragment declarations, plus an elaborator from
  * the parsed Cst to the kernel [[Fragment]] value. A fragment defined in this
  * surface composes identically (same digest) to one constructed in host code.
  *
  * Staging note (docs/bootstrap.md): this meta surface covers sorts,
  * constructors (with binder positions), interfaces, and the variable
  * constructor. Grammar productions, rewrite rules, and judgments are still
  * seeded from host code — the remaining staged step of the primordial
  * meta-language/grammar-language pair (§2b); no fake stubs pretend otherwise.
  */
object Meta:
  val grammar: GrammarSpec = GrammarSpec(
    name = "cairn-meta",
    tokens = TokenSpec(
      keywords = List("fragment", "provides", "requires", "excludes", "sort",
        "ctor", "tree", "graph", "varctor", "binds", "in"),
      puncts = List("{", "}", "(", ")", ":", ";", ","),
      lineComment = Some("--")),
    categories = List(
      CategorySpec("fragmentDecl", List(
        ConstructorSpec("fragmentDecl", List(
          Elem.Tok("fragment"), Elem.NameLeaf,
          Elem.Opt(Elem.Cat("providesClause")),
          Elem.Opt(Elem.Cat("requiresClause")),
          Elem.Tok("{"), Elem.Star(Elem.Cat("item")), Elem.Tok("}"))))),
      CategorySpec("providesClause", List(
        ConstructorSpec("provides", List(Elem.Tok("provides"), Elem.SepBy1(Elem.NameLeaf, ","))))),
      CategorySpec("requiresClause", List(
        ConstructorSpec("requires", List(Elem.Tok("requires"), Elem.SepBy1(Elem.NameLeaf, ","))))),
      CategorySpec("item", List(
        ConstructorSpec("sortTree", List(Elem.Tok("sort"), Elem.NameLeaf, Elem.Tok("tree"), Elem.Tok(";"))),
        ConstructorSpec("sortGraph", List(Elem.Tok("sort"), Elem.NameLeaf, Elem.Tok("graph"), Elem.Tok(";"))),
        ConstructorSpec("ctorDecl", List(
          Elem.Tok("ctor"), Elem.NameLeaf, Elem.Tok(":"), Elem.NameLeaf,
          Elem.Opt(Elem.Cat("argList")),
          Elem.Star(Elem.Cat("bindsClause")),
          Elem.Tok(";"))),
        ConstructorSpec("varCtorDecl", List(Elem.Tok("varctor"), Elem.NameLeaf, Elem.Tok(";"))))),
      CategorySpec("argList", List(
        ConstructorSpec("argList", List(
          Elem.Tok("("), Elem.SepBy1(Elem.NameLeaf, ","), Elem.Tok(")"))))),
      CategorySpec("bindsClause", List(
        ConstructorSpec("binds", List(
          Elem.Tok("binds"), Elem.NumLeaf, Elem.Tok("in"), Elem.SepBy1(Elem.NumLeaf, ",")))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("fragmentDecl", List(
        PrintSeg.Lit("fragment"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Field(1),
        PrintSeg.Field(2), PrintSeg.Space, PrintSeg.Lit("{"), PrintSeg.Newline, PrintSeg.IndentIn,
        PrintSeg.SepFields(3, "\n"), PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
      PrintRule("provides", List(PrintSeg.Space, PrintSeg.Lit("provides"), PrintSeg.Space, PrintSeg.SepFields(0, ", "))),
      PrintRule("requires", List(PrintSeg.Space, PrintSeg.Lit("requires"), PrintSeg.Space, PrintSeg.SepFields(0, ", "))),
      PrintRule("sortTree", List(PrintSeg.Lit("sort"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("tree"), PrintSeg.Lit(";"))),
      PrintRule("sortGraph", List(PrintSeg.Lit("sort"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("graph"), PrintSeg.Lit(";"))),
      PrintRule("ctorDecl", List(
        PrintSeg.Lit("ctor"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Field(2), PrintSeg.SepFields(3, ""), PrintSeg.Lit(";"))),
      PrintRule("argList", List(PrintSeg.Lit("("), PrintSeg.SepFields(0, ", "), PrintSeg.Lit(")"))),
      PrintRule("binds", List(
        PrintSeg.Space, PrintSeg.Lit("binds"), PrintSeg.Space, PrintSeg.Field(0),
        PrintSeg.Space, PrintSeg.Lit("in"), PrintSeg.Space, PrintSeg.SepFields(1, ", "))),
      PrintRule("varCtorDecl", List(PrintSeg.Lit("varctor"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit(";")))),
    top = "fragmentDecl")

  private def names(c: Cst): List[String] = c match
    case Cst.Node("list", items) => items.collect { case Cst.Leaf(n) => n }
    case Cst.Node("some", List(inner)) => names(inner)
    case Cst.Node("none", _) => Nil
    case Cst.Leaf(n) => List(n)
    case _ => Nil

  /** Elaborate a parsed fragment declaration into a kernel Fragment. */
  def elaborate(cst: Cst): Either[String, Fragment] = cst match
    case Cst.Node("fragmentDecl", List(Cst.Leaf(name), providesOpt, requiresOpt, Cst.Node("list", items))) =>
      def clause(opt: Cst, tag: String): List[String] = opt match
        case Cst.Node("some", List(Cst.Node(`tag`, List(ns)))) => names(ns)
        case _ => Nil
      var sorts = List.newBuilder[SortDef]
      var ctors = List.newBuilder[CtorDef]
      var varCtor: Option[String] = None
      val errs = List.newBuilder[String]
      for item <- items do item match
        case Cst.Node("sortTree", List(Cst.Leaf(s)))  => sorts += SortDef(s, SortMode.Tree)
        case Cst.Node("sortGraph", List(Cst.Leaf(s))) => sorts += SortDef(s, SortMode.Graph)
        case Cst.Node("ctorDecl", List(Cst.Leaf(c), Cst.Leaf(sort), argsOpt, bindsList)) =>
          val args = argsOpt match
            case Cst.Node("some", List(Cst.Node("argList", List(ns)))) => names(ns)
            case _ => Nil
          val binders = bindsList match
            case Cst.Node("list", bs) => bs.collect {
              case Cst.Node("binds", List(Cst.Leaf(bi), scope)) =>
                (bi.toInt, names(scope).map(_.toInt)) }
            case _ => Nil
          ctors += CtorDef(c, sort, args, binders)
        case Cst.Node("varCtorDecl", List(Cst.Leaf(v))) =>
          if varCtor.isDefined then errs += s"duplicate varctor in fragment '$name'"
          varCtor = Some(v)
        case other => errs += s"unknown fragment item: ${other.render}"
      val es = errs.result()
      if es.nonEmpty then Left(es.mkString("; "))
      else Right(Fragment(
        name = name,
        provides = clause(providesOpt, "provides"),
        requires = clause(requiresOpt, "requires"),
        sorts = sorts.result(),
        constructors = ctors.result(),
        varCtor = varCtor))
    case other => Left(s"not a fragment declaration: ${other.render}")

  def parseFragment(src: String): Either[String, Fragment] =
    Parser.parse(grammar, src).flatMap(elaborate)
