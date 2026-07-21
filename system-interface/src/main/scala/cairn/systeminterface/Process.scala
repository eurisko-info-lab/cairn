package cairn.systeminterface

/** Effect family: process execution (Phase 3). */
object Process:
  enum Request:
    case Run(command: List[String], cwd: Option[Filesystem.Path] = None,
             mergeStderr: Boolean = true)

  final case class Result(exitCode: Int, stdout: String, stderr: String):
    def ok: Boolean = exitCode == 0
    def combined: String = if stderr.isEmpty then stdout else s"$stdout$stderr"

  enum Error:
    case NotFound(command: String)
    case Io(message: String)
