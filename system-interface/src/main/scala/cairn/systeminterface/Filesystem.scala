package cairn.systeminterface

/** Effect family: filesystem — pure request/response contract
  * (MIGRATION-PLAN.md Phase 3). Paths are platform-agnostic strings; handlers
  * interpret them against a real filesystem root.
  */
object Filesystem:
  opaque type Path = String
  object Path:
    def apply(s: String): Path = s
    extension (p: Path)
      def value: String = p
      def /(child: String): Path = if p.isEmpty then child else s"$p/$child"

  enum Request:
    case Read(path: Path)
    case Write(path: Path, content: String)
    case WriteBytes(path: Path, bytes: Array[Byte])
    case Mkdirs(path: Path)
    case Exists(path: Path)
    case IsDirectory(path: Path)
    case IsRegularFile(path: Path)
    case IsExecutable(path: Path)
    case List(path: Path)
    case Delete(path: Path)
    case CreateTempDirectory(prefix: String)
    case Resolve(base: Path, rel: Path)

  enum Response:
    case Text(content: String)
    case Bytes(bytes: Array[Byte])
    case Ok
    case Bool(value: Boolean)
    case Entries(names: List[String])
    case PathValue(path: Path)

  enum Error:
    case NotFound(path: Path)
    case Io(message: String)

  type Result[A] = Either[Error, A]
