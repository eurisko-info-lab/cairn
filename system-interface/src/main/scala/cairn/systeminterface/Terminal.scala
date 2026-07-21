package cairn.systeminterface

/** Effect family: terminal / stdio (Phase 3). */
object Terminal:
  enum Request:
    case ReadLine
    case Write(text: String)
    case WriteLine(text: String)

  enum Response:
    case Line(text: String)
    case Eof
    case Ok

  enum Error:
    case Closed
    case Io(message: String)
