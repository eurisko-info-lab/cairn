package cairn.systeminterface

/** Effect family: secure randomness (Phase 3). */
object Random:
  enum Request:
    case Bytes(n: Int)

  enum Response:
    case Bytes(value: Array[Byte])

  enum Error:
    case Unavailable(message: String)
