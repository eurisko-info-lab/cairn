package cairn.workbench

import cairn.kernel.*

/** M12: non-text surfaces (§2b). A "foreign format" is another surface INSIDE
  * the language system. JSON is itself defined as a grammar-engine language
  * (no hand-written JSON parser); canonical bytes are the binary surface.
  */
object JsonSurface:
  /** JSON subset grammar (objects / arrays / strings) — everything the Cst
    * encoding needs. Defined as data, interpreted by the one generic engine.
    */
  val grammar: GrammarSpec = GrammarSpec(
    name = "json-cst",
    tokens = TokenSpec(List("true", "false", "null"), List("{", "}", "[", "]", ",", ":", "-"), None),
    categories = List(
      CategorySpec("json", List(
        ConstructorSpec("obj", List(
          Elem.Tok("{"), Elem.Opt(Elem.SepBy1(Elem.Cat("pair"), ",")), Elem.Tok("}"))),
        ConstructorSpec("arr", List(
          Elem.Tok("["), Elem.Opt(Elem.SepBy1(Elem.Cat("json"), ",")), Elem.Tok("]"))),
        ConstructorSpec("jstr", List(Elem.StrLeaf)),
        ConstructorSpec("jneg", List(Elem.Tok("-"), Elem.NumLeaf)),
        ConstructorSpec("jnum", List(Elem.NumLeaf)),
        ConstructorSpec("jtrue", List(Elem.Tok("true"))),
        ConstructorSpec("jfalse", List(Elem.Tok("false"))),
        ConstructorSpec("jnull", List(Elem.Tok("null"))))),
      CategorySpec("pair", List(
        ConstructorSpec("pair", List(Elem.StrLeaf, Elem.Tok(":"), Elem.Cat("json")))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("obj", List(PrintSeg.Lit("{"), PrintSeg.Field(0), PrintSeg.Lit("}"))),
      PrintRule("arr", List(PrintSeg.Lit("["), PrintSeg.Field(0), PrintSeg.Lit("]"))),
      PrintRule("jstr", List(PrintSeg.StrField(0))),
      PrintRule("jneg", List(PrintSeg.Lit("-"), PrintSeg.Field(0))),
      PrintRule("jnum", List(PrintSeg.Field(0))),
      PrintRule("jtrue", List(PrintSeg.Lit("true"))),
      PrintRule("jfalse", List(PrintSeg.Lit("false"))),
      PrintRule("jnull", List(PrintSeg.Lit("null"))),
      PrintRule("pair", List(PrintSeg.StrField(0), PrintSeg.Lit(": "), PrintSeg.Field(1)))),
    top = "json")

  private def some(items: List[Cst]): Cst =
    if items.isEmpty then Cst.node("none") else Cst.node("some", Cst.Node("list", items))

  /** Encode any Cst as a JSON value tree:
    * leaf  => {"leaf": text}
    * node  => {"node": ctor, "kids": [...]}
    */
  def toJsonCst(c: Cst): Cst = c match
    case Cst.Leaf(t) => Cst.node("obj", some(List(
      Cst.node("pair", Cst.Leaf("leaf"), Cst.node("jstr", Cst.Leaf(t))))))
    case Cst.Node(ctor, kids) => Cst.node("obj", some(List(
      Cst.node("pair", Cst.Leaf("node"), Cst.node("jstr", Cst.Leaf(ctor))),
      Cst.node("pair", Cst.Leaf("kids"), Cst.node("arr", some(kids.map(toJsonCst)))))))

  def fromJsonCst(j: Cst): Either[String, Cst] =
    def pairs(opt: Cst): List[(String, Cst)] = opt match
      case Cst.Node("some", List(Cst.Node("list", ps))) => ps.collect {
        case Cst.Node("pair", List(Cst.Leaf(k), v)) => (k, v) }
      case _ => Nil
    j match
      case Cst.Node("obj", List(members)) =>
        val ps = pairs(members).toMap
        (ps.get("leaf"), ps.get("node"), ps.get("kids")) match
          case (Some(Cst.Node("jstr", List(Cst.Leaf(t)))), None, _) => Right(Cst.Leaf(t))
          case (None, Some(Cst.Node("jstr", List(Cst.Leaf(ctor)))), Some(Cst.Node("arr", List(kidsOpt)))) =>
            val kids = kidsOpt match
              case Cst.Node("some", List(Cst.Node("list", ks))) => ks
              case _ => Nil
            kids.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, k) =>
              for xs <- acc; x <- fromJsonCst(k) yield xs :+ x
            }.map(Cst.Node(ctor, _))
          case _ => Left(s"json object is neither a leaf nor a node: ${j.render}")
      case other => Left(s"not a json cst object: ${other.render}")

  def encode(term: Cst): Either[String, String] = Printer.print(grammar, toJsonCst(term))
  def decode(json: String): Either[String, Cst] = Parser.parse(grammar, json).flatMap(fromJsonCst)
  /** Parse arbitrary JSON text to the raw json Cst (no Cst-encoding decode). */
  def decodeRaw(json: String): Either[String, Cst] = Parser.parse(grammar, json)

/** Per-language surface/encoding registry (M12, §2b capability row
  * "import/export formats"): text (the bidirectional grammar), json, canon.
  */
final case class Surface(name: String,
                         encode: Cst => Either[String, String],
                         decode: String => Either[String, Cst])

object Surfaces:
  def forLanguage(l: ComposedLanguage): Map[String, Surface] = Map(
    "text" -> Surface("text",
      t => Printer.print(l.grammar, t),
      s => Parser.parse(l.grammar, s)),
    "json" -> Surface("json", JsonSurface.encode, JsonSurface.decode),
    "canon" -> Surface("canon",
      t => Right(Canon.encode(Cst.toCanon(t)).map(b => f"${b & 0xff}%02x").mkString),
      hex => Canon.decode(hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray).map(Cst.fromCanon)))
