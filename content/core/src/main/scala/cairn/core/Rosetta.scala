package cairn.core

import cairn.kernel.*

/** Rosetta interchange layer (S30–S33, §2 Rosetta, §4.10).
  *
  * One typed artifact graph ([[RosettaModule]]) projects to host targets.
  * Ports are generated VIEWS plus obligations/tests — never new compilers.
  * Each emitter is a grammar-as-data print table whose output is re-parsed
  * under the same grammar and checked for round-trip before it is written:
  * the emitted declaration region is a surface of the artifact, not free text.
  */
enum RType:
  case RInt, RBool, RListInt

/** Host-neutral expression / statement shapes (Cst tags):
  *   rvar(x) | rnum(n) | rnil | rcall(f, args...) | rif(c, a, b)
  * theorem statements: rforall(x, body) | rsorted(e) | rperm(a, b)
  */
final case class RDef(name: String, params: List[(String, RType)], ret: RType, body: Cst)
final case class RTheorem(name: String, statement: Cst)

final case class RosettaModule(name: String, defs: List[RDef], theorems: List[RTheorem]):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "defs" -> Canon.CList(defs.map(d => Canon.cmap(
      "name" -> Canon.CStr(d.name),
      "params" -> Canon.CList(d.params.map((p, t) => Canon.cmap(
        "name" -> Canon.CStr(p), "type" -> Canon.CStr(t.toString)))),
      "ret" -> Canon.CStr(d.ret.toString),
      "body" -> Cst.toCanon(d.body)))),
    "theorems" -> Canon.CList(theorems.map(t => Canon.cmap(
      "name" -> Canon.CStr(t.name), "statement" -> Cst.toCanon(t.statement)))))
  def artifact: Artifact = Artifact(ArtifactKind.RosettaDecl, canon)

trait Port:
  def hostName: String
  /** Emit host text for the module; the grammar-covered declaration region is
    * round-trip verified before the file is assembled.
    */
  def emit(m: RosettaModule): Either[String, String]

object ScalaPort extends Port:
  val hostName = "scala"

  val grammar: GrammarSpec = GrammarSpec(
    name = "rosetta-scala-subset",
    tokens = TokenSpec(
      keywords = List("def", "if", "then", "else", "Int", "Boolean", "List"),
      puncts = List("(", ")", ":", "=", ",", "[", "]"),
      lineComment = Some("//")),
    categories = List(
      CategorySpec("file", List(ConstructorSpec("file", List(Elem.Star(Elem.Cat("decl")))))),
      CategorySpec("decl", List(
        ConstructorSpec("defD", List(
          Elem.Tok("def"), Elem.NameLeaf, Elem.Tok("("),
          Elem.Opt(Elem.SepBy1(Elem.Cat("param"), ",")), Elem.Tok(")"),
          Elem.Tok(":"), Elem.Cat("type"), Elem.Tok("="), Elem.Cat("expr"))))),
      CategorySpec("param", List(
        ConstructorSpec("param", List(Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("type"))))),
      CategorySpec("type", List(
        ConstructorSpec("tInt", List(Elem.Tok("Int"))),
        ConstructorSpec("tBool", List(Elem.Tok("Boolean"))),
        ConstructorSpec("tList", List(Elem.Tok("List"), Elem.Tok("["), Elem.Tok("Int"), Elem.Tok("]"))))),
      CategorySpec("expr", List(
        ConstructorSpec("ifE", List(
          Elem.Tok("if"), Elem.Cat("expr"), Elem.Tok("then"), Elem.Cat("expr"),
          Elem.Tok("else"), Elem.Cat("expr"))),
        ConstructorSpec("listLit", List(
          Elem.Tok("List"), Elem.Tok("("), Elem.Opt(Elem.SepBy1(Elem.Cat("expr"), ",")), Elem.Tok(")"))),
        ConstructorSpec("call", List(
          Elem.NameLeaf, Elem.Tok("("), Elem.Opt(Elem.SepBy1(Elem.Cat("expr"), ",")), Elem.Tok(")"))),
        ConstructorSpec("evar", List(Elem.NameLeaf)),
        ConstructorSpec("num", List(Elem.NumLeaf))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("file", List(PrintSeg.SepFields(0, "\n"))),
      PrintRule("defD", List(
        PrintSeg.Lit("def"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit("("),
        PrintSeg.Field(1), PrintSeg.Lit(")"), PrintSeg.Lit(":"), PrintSeg.Space,
        PrintSeg.Field(2), PrintSeg.Space, PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(3))),
      PrintRule("param", List(PrintSeg.Field(0), PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(1))),
      PrintRule("tInt", List(PrintSeg.Lit("Int"))),
      PrintRule("tBool", List(PrintSeg.Lit("Boolean"))),
      PrintRule("tList", List(PrintSeg.Lit("List[Int]"))),
      PrintRule("ifE", List(
        PrintSeg.Lit("if"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("then"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
        PrintSeg.Lit("else"), PrintSeg.Space, PrintSeg.Field(2))),
      PrintRule("listLit", List(PrintSeg.Lit("List("), PrintSeg.Field(0), PrintSeg.Lit(")"))),
      PrintRule("call", List(PrintSeg.Field(0), PrintSeg.Lit("("), PrintSeg.Field(1), PrintSeg.Lit(")"))),
      PrintRule("evar", List(PrintSeg.Field(0))),
      PrintRule("num", List(PrintSeg.Field(0)))),
    top = "file")

  private def some(items: List[Cst]): Cst =
    if items.isEmpty then Cst.node("none") else Cst.node("some", Cst.Node("list", items))

  def expr(r: Cst): Cst = r match
    case Cst.Node("rvar", List(Cst.Leaf(x)))  => Cst.node("evar", Cst.Leaf(x))
    case Cst.Node("rnum", List(Cst.Leaf(n)))  => Cst.node("num", Cst.Leaf(n))
    case Cst.Node("rnil", _)                  => Cst.node("listLit", Cst.node("none"))
    case Cst.Node("rcall", Cst.Leaf(f) :: as) => Cst.node("call", Cst.Leaf(f), some(as.map(expr)))
    case Cst.Node("rif", List(c, a, b))       => Cst.node("ifE", expr(c), expr(a), expr(b))
    case other => throw CodecError(s"not a rosetta expr: ${other.render}")

  private def typ(t: RType): Cst = t match
    case RType.RInt     => Cst.node("tInt")
    case RType.RBool    => Cst.node("tBool")
    case RType.RListInt => Cst.node("tList")

  def emit(m: RosettaModule): Either[String, String] =
    val fileCst = Cst.node("file", Cst.Node("list", m.defs.map { d =>
      Cst.node("defD", Cst.Leaf(d.name),
        some(d.params.map((p, t) => Cst.node("param", Cst.Leaf(p), typ(t)))),
        typ(d.ret), expr(d.body)) }))
    for
      _ <- RoundTrip.check(grammar, fileCst)
      decls <- Printer.print(grammar, fileCst)
    yield
      val cap = m.name.capitalize
      val samples = "List(List(), List(1), List(3, 1, 2), List(5, 4, 3, 2, 1), List(2, 2, 1, 1))"
      // theorem obligations become executable sample-based checks
      val checks = m.theorems.map { t =>
        def prop(s: Cst, subst: Map[String, String]): String = s match
          case Cst.Node("rforall", List(Cst.Leaf(x), body)) => prop(body, subst + (x -> "xs"))
          case Cst.Node("rsorted", List(e)) => s"isSorted(${exprStr(e, subst)})"
          case Cst.Node("rperm", List(a, b)) => s"isPerm(${exprStr(a, subst)}, ${exprStr(b, subst)})"
          case other => throw CodecError(s"not a rosetta statement: ${other.render}")
        s"""    assert(${prop(t.statement, Map.empty)}, s"${t.name} failed on $$xs")"""
      }.mkString("\n")
      s"""// generated by cairn rosetta scala port — do not edit (artifact ${m.artifact.digest.short})
         |object $cap:
         |  // prelude (fixed surface for the host)
         |  def hd(xs: List[Int]): Int = xs.head
         |  def tl(xs: List[Int]): List[Int] = xs.tail
         |  def append(a: List[Int], b: List[Int]): List[Int] = a ++ b
         |  def cons(h: Int, t: List[Int]): List[Int] = h :: t
         |  def filterLt(p: Int, xs: List[Int]): List[Int] = xs.filter(_ < p)
         |  def filterGe(p: Int, xs: List[Int]): List[Int] = xs.filter(_ >= p)
         |  def isEmpty(xs: List[Int]): Boolean = xs.isEmpty
         |  def isSorted(xs: List[Int]): Boolean = xs.zip(xs.drop(1)).forall(_ <= _)
         |  def isPerm(a: List[Int], b: List[Int]): Boolean = a.sorted == b.sorted
         |  // generated declarations (round-trip verified)
         |${decls.linesIterator.map("  " + _).mkString("\n")}
         |
         |@main def runTests(): Unit =
         |  import $cap.*
         |  for xs <- $samples do
         |$checks
         |  println("ALL TESTS PASS")
         |""".stripMargin

  private def exprStr(r: Cst, subst: Map[String, String]): String = r match
    case Cst.Node("rvar", List(Cst.Leaf(x)))  => subst.getOrElse(x, x)
    case Cst.Node("rcall", Cst.Leaf(f) :: as) => s"$f(${as.map(exprStr(_, subst)).mkString(", ")})"
    case other => throw CodecError(s"unsupported in statement position: ${other.render}")

object LeanPort extends Port:
  val hostName = "lean"

  val grammar: GrammarSpec = GrammarSpec(
    name = "rosetta-lean-subset",
    tokens = TokenSpec(
      keywords = List("partial", "def", "theorem", "by", "sorry", "bif", "then",
        "else", "Int", "Bool", "List", "listSorted", "listPerm"),
      puncts = List("(", ")", ":=", ":", ",", "[", "]", "∀"),
      lineComment = Some("--")),
    categories = List(
      CategorySpec("file", List(ConstructorSpec("file", List(Elem.Star(Elem.Cat("decl")))))),
      CategorySpec("decl", List(
        ConstructorSpec("defD", List(
          Elem.Tok("partial"), Elem.Tok("def"), Elem.NameLeaf, Elem.Star(Elem.Cat("param")),
          Elem.Tok(":"), Elem.Cat("type"), Elem.Tok(":="), Elem.Cat("expr"))),
        ConstructorSpec("thmD", List(
          Elem.Tok("theorem"), Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("prop"),
          Elem.Tok(":="), Elem.Tok("by"), Elem.Tok("sorry"))))),
      CategorySpec("param", List(
        ConstructorSpec("param", List(
          Elem.Tok("("), Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("type"), Elem.Tok(")"))))),
      CategorySpec("type", List(
        ConstructorSpec("tInt", List(Elem.Tok("Int"))),
        ConstructorSpec("tBool", List(Elem.Tok("Bool"))),
        ConstructorSpec("tList", List(Elem.Tok("List"), Elem.Tok("Int"))))),
      CategorySpec("expr", List(
        ConstructorSpec("ifE", List(
          Elem.Tok("("), Elem.Tok("bif"), Elem.Cat("expr"), Elem.Tok("then"),
          Elem.Cat("expr"), Elem.Tok("else"), Elem.Cat("expr"), Elem.Tok(")"))),
        ConstructorSpec("listLit", List(
          Elem.Tok("["), Elem.Opt(Elem.SepBy1(Elem.Cat("expr"), ",")), Elem.Tok("]"))),
        ConstructorSpec("call", List(
          Elem.Tok("("), Elem.NameLeaf, Elem.Star(Elem.Cat("expr")), Elem.Tok(")"))),
        ConstructorSpec("evar", List(Elem.NameLeaf)),
        ConstructorSpec("num", List(Elem.NumLeaf)))),
      CategorySpec("prop", List(
        ConstructorSpec("forallP", List(
          Elem.Tok("∀"), Elem.NameLeaf, Elem.Tok(":"), Elem.Tok("List"), Elem.Tok("Int"),
          Elem.Tok(","), Elem.Cat("prop"))),
        ConstructorSpec("sortedP", List(Elem.Tok("listSorted"), Elem.Cat("expr"))),
        ConstructorSpec("permP", List(Elem.Tok("listPerm"), Elem.Cat("expr"), Elem.Cat("expr")))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("file", List(PrintSeg.SepFields(0, "\n"))),
      PrintRule("defD", List(
        PrintSeg.Lit("partial def"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(2),
        PrintSeg.Space, PrintSeg.Lit(":="), PrintSeg.Space, PrintSeg.Field(3))),
      PrintRule("thmD", List(
        PrintSeg.Lit("theorem"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
        PrintSeg.Lit(":= by sorry"))),
      PrintRule("param", List(
        PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"),
        PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(")"))),
      PrintRule("tInt", List(PrintSeg.Lit("Int"))),
      PrintRule("tBool", List(PrintSeg.Lit("Bool"))),
      PrintRule("tList", List(PrintSeg.Lit("List Int"))),
      PrintRule("ifE", List(
        PrintSeg.Lit("(bif"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("then"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
        PrintSeg.Lit("else"), PrintSeg.Space, PrintSeg.Field(2), PrintSeg.Lit(")"))),
      PrintRule("listLit", List(PrintSeg.Lit("["), PrintSeg.Field(0), PrintSeg.Lit("]"))),
      PrintRule("call", List(
        PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.SepFields(1, " "), PrintSeg.Lit(")"))),
      PrintRule("evar", List(PrintSeg.Field(0))),
      PrintRule("num", List(PrintSeg.Field(0))),
      PrintRule("forallP", List(
        PrintSeg.Lit("∀"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit(": List Int,"), PrintSeg.Space, PrintSeg.Field(1))),
      PrintRule("sortedP", List(PrintSeg.Lit("listSorted"), PrintSeg.Space, PrintSeg.Field(0))),
      PrintRule("permP", List(
        PrintSeg.Lit("listPerm"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Field(1)))),
    top = "file")

  def expr(r: Cst): Cst = r match
    case Cst.Node("rvar", List(Cst.Leaf(x)))  => Cst.node("evar", Cst.Leaf(x))
    case Cst.Node("rnum", List(Cst.Leaf(n)))  => Cst.node("num", Cst.Leaf(n))
    case Cst.Node("rnil", _)                  => Cst.node("listLit", Cst.node("none"))
    case Cst.Node("rcall", Cst.Leaf(f) :: as) => Cst.node("call", Cst.Leaf(f), Cst.Node("list", as.map(expr)))
    case Cst.Node("rif", List(c, a, b))       => Cst.node("ifE", expr(c), expr(a), expr(b))
    case other => throw CodecError(s"not a rosetta expr: ${other.render}")

  def prop(s: Cst): Cst = s match
    case Cst.Node("rforall", List(x, body)) => Cst.node("forallP", x, prop(body))
    case Cst.Node("rsorted", List(e))       => Cst.node("sortedP", exprAsCallArg(e))
    case Cst.Node("rperm", List(a, b))      => Cst.node("permP", exprAsCallArg(a), exprAsCallArg(b))
    case other => throw CodecError(s"not a rosetta statement: ${other.render}")
  private def exprAsCallArg(e: Cst): Cst = expr(e)

  private def typ(t: RType): Cst = t match
    case RType.RInt     => Cst.node("tInt")
    case RType.RBool    => Cst.node("tBool")
    case RType.RListInt => Cst.node("tList")

  def emit(m: RosettaModule): Either[String, String] =
    val decls: List[Cst] =
      m.defs.map { d =>
        Cst.node("defD", Cst.Leaf(d.name),
          Cst.Node("list", d.params.map((p, t) => Cst.node("param", Cst.Leaf(p), typ(t)))),
          typ(d.ret), expr(d.body)) } ++
      m.theorems.map(t => Cst.node("thmD", Cst.Leaf(t.name), prop(t.statement)))
    val fileCst = Cst.node("file", Cst.Node("list", decls))
    for
      _ <- RoundTrip.check(grammar, fileCst)
      text <- Printer.print(grammar, fileCst)
    yield
      s"""-- generated by cairn rosetta lean port — do not edit (artifact ${m.artifact.digest.short})
         |namespace ${m.name.capitalize}
         |-- prelude
         |partial def hd (xs : List Int) : Int := xs.headD 0
         |partial def tl (xs : List Int) : List Int := xs.tail
         |partial def append (a : List Int) (b : List Int) : List Int := a ++ b
         |partial def cons (h : Int) (t : List Int) : List Int := h :: t
         |partial def filterLt (p : Int) (xs : List Int) : List Int := xs.filter (fun x => decide (x < p))
         |partial def filterGe (p : Int) (xs : List Int) : List Int := xs.filter (fun x => decide (p <= x))
         |partial def isEmpty (xs : List Int) : Bool := xs.isEmpty
         |def listSorted (xs : List Int) : Prop := List.Pairwise (fun a b => a <= b) xs
         |def listPerm (a b : List Int) : Prop := List.Perm a b
         |-- generated declarations (round-trip verified)
         |$text
         |end ${m.name.capitalize}
         |""".stripMargin
