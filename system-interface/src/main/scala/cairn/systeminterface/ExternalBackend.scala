package cairn.systeminterface

/** Effect family: external host backends (scala-cli, cargo, runghc, …)
  * (Phase 3). Builds on Process with host-tool discovery.
  */
object ExternalBackend:
  enum Host:
    case ScalaCli, Cargo, Runghc, Lake

  enum Request:
    case Find(host: Host)
    case Run(host: Host, args: List[String], cwd: Option[Filesystem.Path] = None)

  enum Response:
    case Found(path: Filesystem.Path)
    case Missing(host: Host)
    case ProcessResult(exitCode: Int, output: String)

  enum Error:
    case Io(message: String)
