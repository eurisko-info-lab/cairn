package cairn.user.bend

import cairn.kernel.*
import cairn.core.*
import cairn.user.icnet.IcNet

/** Bend-profile surface (M29, §5b): a Bend/Kind-flavored functional surface
  * lowered to full interaction combinators (IcNet). Only exists NOW because
  * replicators are real (M25/M26) — per the honesty rule, no `examples/bend`
  * was allowed while nets were affine-only.
  *
  * Surface:
  *   def name = expr
  *   expr  ::= atom atom ...            (application by juxtaposition, M6 Run)
  *   atom  ::= @x ( expr ) | ( expr ) | tru | fls | numeral | name
  *
  * Numerals are **Church encodings** over existing `lam`/`app` nets — not
  * native HVM `NUM`/`OPR`. Recursion is reachable via an explicit Y/fix
  * combinator in source (self-application), not a primitive `let rec`.
  */
object Bend:
  val grammar: GrammarSpec = GrammarSpec(
    name = "bend",
    tokens = TokenSpec(List("def", "tru", "fls"), List("@", "(", ")", "="), Some("#")),
    categories = List(
      CategorySpec("file", List(
        ConstructorSpec("bfile", List(Elem.Star(Elem.Cat("defB")))))),
      CategorySpec("defB", List(
        ConstructorSpec("bdef", List(Elem.Tok("def"), Elem.NameLeaf, Elem.Tok("="), Elem.Cat("expr"))))),
      CategorySpec("expr", List(
        ConstructorSpec("brun", List(Elem.Run("atom"))))),
      CategorySpec("atom", List(
        ConstructorSpec("blam", List(
          Elem.Tok("@"), Elem.NameLeaf, Elem.Tok("("), Elem.Cat("expr"), Elem.Tok(")"))),
        ConstructorSpec("bgroup", List(Elem.Tok("("), Elem.Cat("expr"), Elem.Tok(")"))),
        ConstructorSpec("btru", List(Elem.Tok("tru"))),
        ConstructorSpec("bfls", List(Elem.Tok("fls"))),
        ConstructorSpec("bnum", List(Elem.NumLeaf)),
        ConstructorSpec("bvar", List(Elem.NameLeaf))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("bfile", List(PrintSeg.SepFields(0, "\n"))),
      PrintRule("bdef", List(
        PrintSeg.Lit("def"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1))),
      PrintRule("brun", List(PrintSeg.SepFields(0, " "))),
      PrintRule("blam", List(
        PrintSeg.Lit("@"), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("("),
        PrintSeg.Field(1), PrintSeg.Lit(")"))),
      PrintRule("bgroup", List(PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Lit(")"))),
      PrintRule("btru", List(PrintSeg.Lit("tru"))),
      PrintRule("bfls", List(PrintSeg.Lit("fls"))),
      PrintRule("bnum", List(PrintSeg.Field(0))),
      PrintRule("bvar", List(PrintSeg.Field(0)))),
    top = "file")

  /** Elaborate Bend surface Cst into λ-shaped Cst (var/lam/app/true/false). */
  def elaborate(e: Cst): Either[String, Cst] = e match
    case Cst.Node("brun", List(Cst.Node("list", atoms))) =>
      atoms.foldLeft[Either[String, Option[Cst]]](Right(None)) { (acc, a) =>
        for
          f <- acc
          x <- elaborate(a)
        yield Some(f.fold(x)(g => Cst.node("app", g, x)))
      }.flatMap(_.toRight("empty application run"))
    case Cst.Node("blam", List(Cst.Leaf(x), body)) =>
      elaborate(body).map(b => Cst.node("lam", Cst.Leaf(x), Cst.node("tyBool"), b))
    case Cst.Node("bgroup", List(inner)) => elaborate(inner)
    case Cst.Node("btru", _) => Right(Cst.node("true"))
    case Cst.Node("bfls", _) => Right(Cst.node("false"))
    case Cst.Node("bnum", List(Cst.Leaf(n))) =>
      n.toIntOption match
        case None => Left(s"not a bend numeral: $n")
        case Some(k) if k < 0 => Left(s"negative bend numeral: $n")
        case Some(k) =>
          val s = "s"; val z = "z"
          val body = (1 to k).foldLeft[Cst](Cst.node("var", Cst.Leaf(z))) { (acc, _) =>
            Cst.node("app", Cst.node("var", Cst.Leaf(s)), acc)
          }
          Right(Cst.node("lam", Cst.Leaf(s), Cst.node("tyBool"),
            Cst.node("lam", Cst.Leaf(z), Cst.node("tyBool"), body)))
    case Cst.Node("bvar", List(Cst.Leaf(x))) => Right(Cst.node("var", Cst.Leaf(x)))
    case other => Left(s"not a bend expr: ${other.render}")

  /** Parse a program, inline definitions into `main`, lower, reduce, read back. */
  def run(source: String): Either[String, Cst] =
    for
      cst <- Parser.parse(grammar, source)
      defs <- cst match
        case Cst.Node("bfile", List(Cst.Node("list", ds))) =>
          ds.foldLeft[Either[String, List[(String, Cst)]]](Right(Nil)) {
            case (acc, Cst.Node("bdef", List(Cst.Leaf(n), body))) =>
              for xs <- acc; b <- elaborate(body) yield xs :+ (n, b)
            case (_, other) => Left(s"not a def: ${other.render}")
          }
        case other => Left(s"not a bend file: ${other.render}")
      main <- defs.collectFirst { case ("main", b) => b }.toRight("no `def main`")
      // inline definitions (earlier defs may appear in later ones and in main)
      inlined = defs.foldLeft(main) { case (acc, (n, b)) =>
        substFree(acc, n, defs.foldLeft(b) { case (acc2, (n2, b2)) =>
          if n2 == n then acc2 else substFree(acc2, n2, b2) }) }
      lowered <- IcNet.lower(inlined)
      _ <- NetEngine.wellFormed(IcNet.language, lowered._1).left.map(_.mkString("; "))
      normal <- NetEngine.normalize(IcNet.language, lowered._1)
      value <- IcNet.readback(normal, lowered._2)
    yield value

  private def substFree(t: Cst, name: String, replacement: Cst): Cst = t match
    case Cst.Node("var", List(Cst.Leaf(x))) if x == name => replacement
    case Cst.Node("lam", List(Cst.Leaf(x), ty, body)) =>
      if x == name then t
      else Cst.Node("lam", List(Cst.Leaf(x), ty, substFree(body, name, replacement)))
    case Cst.Node(c, cs) => Cst.Node(c, cs.map(substFree(_, name, replacement)))
    case leaf => leaf
