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
  * reasoning as `Filesystem.Resolve`. [[perform]] is where the actual
  * session I/O happens and requires a pre-authorized [[AuthorizedEffect]].
  * Keys from [[EffectMeta.lsp]].
  */
object LspTransport:
  private def iface(reg: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds) =
    reg.require(Effects.Family.Lsp)

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

  def intent(req: LspIface.Request,
      registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds): (Effects.ActionKey, Authority.Resource) =
    val ctor = req match
      case LspIface.Request.ReadMessage     => "readMessage"
      case LspIface.Request.WriteMessage(_) => "writeMessage"
    (iface(registry).keyFor(ctor).get, iface(registry).resource.any)

  def run(req: LspIface.Request, in: InputStream, out: OutputStream, ctx: EffectContext)
      : Either[LspIface.Error, LspIface.Response] =
    val (action, resource) = intent(req, ctx.registry)
    ctx.authorize(action, resource) match
      case Left(err)   => Left(LspIface.Error.Framing(s"denied: $err"))
      case Right(auth) => perform(req, in, out, auth, ctx.registry)

  def perform(req: LspIface.Request, in: InputStream, out: OutputStream, auth: AuthorizedEffect, registry: RuntimeEffectRegistry = RuntimeEffectRegistry.seeds)
      : Either[LspIface.Error, LspIface.Response] =
    val (action, resource) = intent(req, registry)
    if !auth.covers(action, resource) then
      Left(LspIface.Error.Framing("authorized effect does not cover request"))
    else
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
