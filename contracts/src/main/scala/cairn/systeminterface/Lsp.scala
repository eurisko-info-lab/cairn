package cairn.systeminterface

/** Effect family: LSP transport framing (Phase 3). Message semantics stay in
  * surface/core; this only names Content-Length framed stdio I/O.
  */
object Lsp:
  enum Request:
    case ReadMessage
    case WriteMessage(payload: String)

  enum Response:
    case Message(payload: String)
    case SessionEnded
    case Ok

  enum Error:
    case Framing(message: String)
    case Closed
