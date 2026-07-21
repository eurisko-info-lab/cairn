package cairn.systemhandler

import cairn.systeminterface.Lsp as LspIface
import java.io.{InputStream, OutputStream}

/** LSP Content-Length framing transport (Phase 3 lsp family). Message
  * semantics stay in `surface.LspServer`; this only does bytes on the wire.
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
