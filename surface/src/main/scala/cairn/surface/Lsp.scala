package cairn.surface

import cairn.kernel.*
import cairn.workbench.*
import cairn.compute.TreeEngine

/** M44: a language server for ANY registered language, built entirely on the
  * generic engine: diagnostics come from the parser (with spans/carets),
  * formatting is the printer, hover is derivation search, and RENAME is the
  * generic ΔL rename-with-footprint — every rename emits a ValidatedChangeSet.
  *
  * Documents use the generic module surface: `name = <term> ;` lines derived
  * mechanically for any language (no per-language server code).
  */
// ModuleSurface moved to cairn.workbench.ModuleSurface (L1) — format-preserving
// Delta application needs it without pulling in the surface layer; imported here
// via the existing `import cairn.workbench.*` above.

/** JSON value helpers over the JsonSurface Cst shapes. */
object J:
  def obj(fields: (String, Cst)*): Cst =
    Cst.node("obj", if fields.isEmpty then Cst.node("none")
      else Cst.node("some", Cst.Node("list",
        fields.toList.map((k, v) => Cst.node("pair", Cst.Leaf(k), v)))))
  def arr(items: List[Cst]): Cst =
    Cst.node("arr", if items.isEmpty then Cst.node("none")
      else Cst.node("some", Cst.Node("list", items)))
  def str(s: String): Cst = Cst.node("jstr", Cst.Leaf(s))
  def num(i: Long): Cst =
    if i < 0 then Cst.node("jneg", Cst.Leaf((-i).toString)) else Cst.node("jnum", Cst.Leaf(i.toString))

  def fields(c: Cst): Map[String, Cst] = c match
    case Cst.Node("obj", List(Cst.Node("some", List(Cst.Node("list", ps))))) =>
      ps.collect { case Cst.Node("pair", List(Cst.Leaf(k), v)) => k -> v }.toMap
    case _ => Map.empty
  def asStr(c: Cst): Option[String] = c match
    case Cst.Node("jstr", List(Cst.Leaf(s))) => Some(s)
    case _ => None
  def asNum(c: Cst): Option[Long] = c match
    case Cst.Node("jnum", List(Cst.Leaf(n))) => n.toLongOption
    case Cst.Node("jneg", List(Cst.Leaf(n))) => n.toLongOption.map(-_)
    case _ => None
  def print(c: Cst): String = Printer.print(JsonSurface.grammar, c).getOrElse("{}")

final case class LspConfig(
    language: ComposedLanguage,
    /** optional hover type-inference: term => rendered type */
    typeOf: Option[Cst => Either[String, String]] = None)

/** Message-level LSP server (transport-agnostic; see [[Lsp.serve]] for the
  * Content-Length framing loop).
  */
final class LspServer(cfg: LspConfig):
  private val docs = scala.collection.mutable.Map[String, String]()
  private val moduleGrammar = ModuleSurface.grammar(cfg.language)
  /** every successful rename appends its kernel-gated change-set here (M44 AC) */
  val changeSets = scala.collection.mutable.ListBuffer[Delta.ValidatedChangeSet]()

  private def response(id: Cst, result: Cst): Cst =
    J.obj("jsonrpc" -> J.str("2.0"), "id" -> id, "result" -> result)
  private def notification(method: String, params: Cst): Cst =
    J.obj("jsonrpc" -> J.str("2.0"), "method" -> J.str(method), "params" -> params)

  private def diagnosticsFor(uri: String, text: String): Cst =
    val diags = Parser.parse(moduleGrammar, text) match
      case Right(_)  => Nil
      case Left(err) =>
        // extract line:col from the rich error
        val re = """parse error at (\d+):(\d+)""".r.unanchored
        val (l, c) = err match
          case re(ln, cl) => (ln.toInt - 1, cl.toInt - 1)
          case _          => (0, 0)
        List(J.obj(
          "range" -> J.obj(
            "start" -> J.obj("line" -> J.num(l), "character" -> J.num(c)),
            "end" -> J.obj("line" -> J.num(l), "character" -> J.num(c + 1))),
          "severity" -> J.num(1),
          "message" -> J.str(err.linesIterator.nextOption.getOrElse("parse error"))))
    notification("textDocument/publishDiagnostics",
      J.obj("uri" -> J.str(uri), "diagnostics" -> J.arr(diags)))

  private def wholeDocEdit(text: String, newText: String): Cst =
    val lines = text.count(_ == '\n') + 1
    J.obj("range" -> J.obj(
      "start" -> J.obj("line" -> J.num(0), "character" -> J.num(0)),
      "end" -> J.obj("line" -> J.num(lines), "character" -> J.num(0))),
      "newText" -> J.str(newText))

  private def wordAt(text: String, line: Long, character: Long): Option[String] =
    text.linesIterator.drop(line.toInt).nextOption.flatMap { l =>
      val i = character.toInt
      if i > l.length then None
      else
        def isW(c: Char) = c.isLetterOrDigit || c == '_'
        var s = math.min(i, l.length - 1); var e = i
        if s < 0 then None
        else
          while s > 0 && isW(l(s - 1)) do s -= 1
          while e < l.length && isW(l(e)) do e += 1
          if s < e then Some(l.substring(s, e)) else None
    }

  /** Handle one incoming message; returns zero or more outgoing messages. */
  def handle(message: Cst): List[Cst] =
    val f = J.fields(message)
    val id = f.getOrElse("id", Cst.node("jnull"))
    val method = f.get("method").flatMap(J.asStr).getOrElse("")
    val params = f.getOrElse("params", J.obj())
    val p = J.fields(params)
    def docUri: String =
      J.fields(p.getOrElse("textDocument", J.obj())).get("uri").flatMap(J.asStr).getOrElse("")
    method match
      case "initialize" =>
        List(response(id, J.obj("capabilities" -> J.obj(
          "textDocumentSync" -> J.num(1),
          "documentFormattingProvider" -> Cst.node("jtrue"),
          "renameProvider" -> Cst.node("jtrue"),
          "hoverProvider" -> Cst.node("jtrue")))))
      case "textDocument/didOpen" =>
        val td = J.fields(p.getOrElse("textDocument", J.obj()))
        val uri = td.get("uri").flatMap(J.asStr).getOrElse("")
        val text = td.get("text").flatMap(J.asStr).getOrElse("")
        docs(uri) = text
        List(diagnosticsFor(uri, text))
      case "textDocument/didChange" =>
        val uri = docUri
        val text = (p.get("contentChanges") match
          case Some(Cst.Node("arr", List(Cst.Node("some", List(Cst.Node("list", changes)))))) =>
            changes.lastOption.flatMap(c => J.fields(c).get("text")).flatMap(J.asStr)
          case _ => None).getOrElse("")
        docs(uri) = text
        List(diagnosticsFor(uri, text))
      case "textDocument/formatting" =>
        val uri = docUri
        val text = docs.getOrElse(uri, "")
        Parser.parse(moduleGrammar, text) match
          case Right(cst) =>
            Printer.print(moduleGrammar, cst) match
              case Right(formatted) => List(response(id, J.arr(List(wholeDocEdit(text, formatted)))))
              case Left(_)          => List(response(id, J.arr(Nil)))
          case Left(_) => List(response(id, J.arr(Nil)))
      case "textDocument/rename" =>
        val uri = docUri
        val text = docs.getOrElse(uri, "")
        val pos = J.fields(p.getOrElse("position", J.obj()))
        val newName = p.get("newName").flatMap(J.asStr).getOrElse("renamed")
        val result = for
          line <- pos.get("line").flatMap(J.asNum).toRight("no line")
          char <- pos.get("character").flatMap(J.asNum).toRight("no character")
          oldName <- wordAt(text, line, char).toRight("no identifier at position")
          cst <- Parser.parse(moduleGrammar, text)
          module <- ModuleSurface.toModule(cst)
          dl <- Delta.deltaOf(cfg.language).left.map(_.map(_.render).mkString("; "))
          // the generic ΔL rename with its exact footprint (M44 = M17 = §2b)
          footprint = module.defs.collect { case (n, t)
            if n != oldName && Binding.freeVars(cfg.language.binderSpec,
              cfg.language.varCtor.getOrElse("var"))(t).contains(oldName) => n }
          fpCst = if footprint.isEmpty then Cst.node("none")
                  else Cst.node("some", Cst.Node("list", footprint.map(Cst.Leaf(_))))
          change = Cst.Node(Delta.tag(cfg.language, "changeset"), List(Cst.Node("list", List(
            Cst.Node(Delta.tag(cfg.language, "rename"),
              List(Cst.Leaf(oldName), Cst.Leaf(newName), fpCst))))))
          applied <- Delta.apply(cfg.language, module, change)
          newText <- Printer.print(moduleGrammar, ModuleSurface.fromModule(applied._1))
        yield (applied._2, newText)
        result match
          case Right((vcs, newText)) =>
            changeSets += vcs
            List(response(id, J.obj("changes" -> J.obj(uri -> J.arr(List(wholeDocEdit(text, newText)))))))
          case Left(err) =>
            List(J.obj("jsonrpc" -> J.str("2.0"), "id" -> id,
              "error" -> J.obj("code" -> J.num(-32602), "message" -> J.str(err))))
      case "textDocument/hover" =>
        val uri = docUri
        val text = docs.getOrElse(uri, "")
        val pos = J.fields(p.getOrElse("position", J.obj()))
        val hover = for
          line <- pos.get("line").flatMap(J.asNum)
          char <- pos.get("character").flatMap(J.asNum)
          word <- wordAt(text, line, char)
          typeOf <- cfg.typeOf
          cst <- Parser.parse(moduleGrammar, text).toOption
          module <- ModuleSurface.toModule(cst).toOption
          term <- module.get(word)
          ty <- typeOf(term).toOption
        yield s"$word : $ty"
        List(response(id, hover.fold(Cst.node("jnull"))(h =>
          J.obj("contents" -> J.str(h)))))
      case "shutdown" => List(response(id, Cst.node("jnull")))
      case _          => Nil // notifications we ignore

object Lsp:
  /** Content-Length framed serve loop (stdio or any stream pair). */
  def serve(cfg: LspConfig, in: java.io.InputStream, out: java.io.OutputStream): Unit =
    val server = LspServer(cfg)
    var running = true
    while running do
      readMessage(in) match
        case None => running = false
        case Some(text) =>
          JsonSurface.decodeRaw(text) match
            case Right(msg) =>
              val method = J.fields(msg).get("method").flatMap(J.asStr)
              if method.contains("exit") then running = false
              else server.handle(msg).foreach(m => writeMessage(out, J.print(m)))
            case Left(_) => ()

  def readMessage(in: java.io.InputStream): Option[String] =
    var length = -1
    var line = new StringBuilder
    var b = in.read()
    while b >= 0 do
      if b == '\n' then
        val l = line.toString.trim
        line = new StringBuilder
        if l.isEmpty then
          if length >= 0 then
            val buf = in.readNBytes(length)
            return Some(new String(buf, "UTF-8"))
          else return None
        else if l.toLowerCase.startsWith("content-length:") then
          length = l.drop("content-length:".length).trim.toInt
      else if b != '\r' then line.append(b.toChar)
      b = in.read()
    None

  def writeMessage(out: java.io.OutputStream, body: String): Unit =
    val bytes = body.getBytes("UTF-8")
    out.write(s"Content-Length: ${bytes.length}\r\n\r\n".getBytes("UTF-8"))
    out.write(bytes)
    out.flush()

/** M44: a REPL over any registered language. */
final class Repl(l: ComposedLanguage):
  private var module = Module(Nil)
  def eval(line: String): String =
    line.trim match
      case s if s.startsWith(":delta ") =>
        val r = for
          dl <- Delta.deltaOf(l).left.map(_.map(_.render).mkString("; "))
          ch <- Parser.parse(dl.grammar, s.drop(7))
          out <- Delta.apply(l, module, ch)
        yield { module = out._1; s"module ${out._2.base.short} -> ${out._2.result.short}" }
        r.fold(e => s"error: $e", identity)
      case s if s.startsWith(":defs") =>
        if module.defs.isEmpty then "(empty module)"
        else module.defs.map((n, t) => s"$n = ${Printer.print(l.grammar, t).getOrElse(t.render)}").mkString("\n")
      case s if s.nonEmpty =>
        val r = for
          t <- Parser.parse(l.grammar, s)
          v <- TreeEngine.normalize(l, t)
          printed <- Printer.print(l.grammar, v)
        yield printed
        r.fold(e => s"error: ${e.linesIterator.nextOption.getOrElse(e)}", identity)
      case _ => ""
