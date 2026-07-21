package cairn.systeminterface

/** Effect family: HTTP serve/client (Phase 3). Distinct from raw Network —
  * typed HTTP verbs and response codes used by ledger sync and the browser UI.
  */
object Http:
  enum Request:
    case Serve(bindHost: String, bindPort: Int)
    case Get(baseUrl: String, path: String)
    case Stop(handle: ServeHandle)

  opaque type ServeHandle = Int
  object ServeHandle:
    def apply(port: Int): ServeHandle = port
    extension (h: ServeHandle) def port: Int = h

  enum Response:
    case Listening(handle: ServeHandle)
    case Body(bytes: Array[Byte])
    case Stopped

  enum Error:
    case BindFailed(message: String)
    case RequestFailed(message: String)
