package cairn.systemhandler

import cairn.systeminterface.Lsp as LspIface
import cairn.kernel.{Authority, Effects}
import java.io.{InputStream, OutputStream}

/** LSP Content-Length framing transport (Phase 3 lsp family). Message
  * semantics stay in `surface.LspServer`; this only does bytes on the wire.
  * `readMessage`/`writeMessage` stay public: they're pure framing over
  * whatever stream is passed in (no I/O of their own beyond that stream),
  * so a caller building fixture bytes over a `ByteArrayOutputStream` (as
  * `WaveH2Suite` does) has no real-world effect to gate — the same
  * reasoning as `Filesystem.Resolve`. `perform` is where the actual
  * session I/O happens and is gated by [[AuthorityGate]] (Subject("local")
  * is a placeholder — this family has no real multi-tenant identity yet).
  */
object LspTransport:
  def readMessage(in: InputStream): Option[String] =
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

  def writeMessage(out: OutputStream, body: String): Unit =
    val bytes = body.getBytes("UTF-8")
    out.write(s"Content-Length: ${bytes.length}\r\n\r\n".getBytes("UTF-8"))
    out.write(bytes)
    out.flush()

  def perform(req: LspIface.Request, in: InputStream, out: OutputStream)
      : Either[LspIface.Error, LspIface.Response] =
    val action = req match
      case LspIface.Request.ReadMessage     => Effects.Action.LspRead
      case LspIface.Request.WriteMessage(_) => Effects.Action.LspWrite
    val authReq = Authority.EffectRequest(Authority.Subject("local"), action, Authority.Resource("lsp", "*"))
    AuthorityGate.forFamily(Effects.Family.Lsp).checked(authReq)(err => LspIface.Error.Framing(s"denied: $err")) {
      try req match
        case LspIface.Request.ReadMessage =>
          readMessage(in) match
            case Some(p) => Right(LspIface.Response.Message(p))
            case None    => Right(LspIface.Response.SessionEnded)
        case LspIface.Request.WriteMessage(payload) =>
          writeMessage(out, payload); Right(LspIface.Response.Ok)
      catch
        case _: java.io.IOException => Left(LspIface.Error.Closed)
        case e: Exception           => Left(LspIface.Error.Framing(e.getMessage))
    }
