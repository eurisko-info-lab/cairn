package cairn.systeminterface

/** Effect family: workspace / language-pack discovery (Phase 3). */
object Workspace:
  enum Request:
    case LanguageDirs
    case ListCairnFiles(dir: Filesystem.Path)
    case ListSubdirs(dir: Filesystem.Path)
    case ListSurfaceCairnFiles(langDir: Filesystem.Path)
    case ReadText(path: Filesystem.Path)

  enum Response:
    case Paths(paths: List[Filesystem.Path])
    case Text(content: String)

  enum Error:
    case Io(message: String)
