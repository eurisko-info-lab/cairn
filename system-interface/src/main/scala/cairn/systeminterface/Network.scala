package cairn.systeminterface

/** Effect family: network endpoints (Phase 3). */
object Network:
  opaque type Endpoint = String
  object Endpoint:
    def apply(url: String): Endpoint = url
    extension (e: Endpoint) def url: String = e

  enum Request:
    case Connect(endpoint: Endpoint)
    case GetBytes(endpoint: Endpoint, path: String)

  enum Response:
    case Connected(endpoint: Endpoint)
    case Bytes(body: Array[Byte])

  enum Error:
    case Unreachable(endpoint: Endpoint, message: String)
    case HttpStatus(code: Int, path: String)
