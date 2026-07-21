package cairn.kernel

/** Meta-defined effect interfaces (post-migration priority #1): each effect
  * family's Request/Response/Error vocabulary as a Kernel-owned [[Fragment]]
  * (sorts + constructors, no grammar — effect requests are host-constructed,
  * not user-typed source text), so the family's rights vocabulary is checked
  * against a composable, Kernel-valid description instead of maintained by
  * hand with nothing to keep it honest. Kept in `kernel` (not
  * `system-interface`), matching [[Authority]]'s stated constraint that the
  * action vocabulary stays inside the TCB.
  *
  * Rights are many-to-one: several `Request`-sorted constructors can share
  * one [[Effects.Action]] (e.g. `Filesystem`'s dozen request shapes reduce
  * to three capability classes — read/write/mkdirs — by design, not by
  * accident). `requestActions` makes that grouping an explicit, checked
  * declaration instead of an inferred name match, which only happened to
  * work for the first four families because each had ≤2 distinct request
  * shapes.
  *
  * All 8 live families (`random`/`clock`/`process`/`externalBackend`/
  * `terminal`/`workspace`/`filesystem`/`lsp`) are converted. The vestigial
  * families (`Http`/`Network`/`Crypto`/`LedgerTransport`, no handler
  * implementation at all) and `Cas` (a trait, not this shape) remain a
  * separate decision — see `docs/architecture.md`.
  */
object EffectMeta:
  /** A Fragment paired with the explicit constructor-name → Action grouping
    * that governs it. `Action` stays a closed Scala enum in this slice
    * (widening it to fully mint cases from the Fragment is a separate,
    * larger move) — this makes the grouping authoritative and checked
    * ([[completeness]]) rather than replacing `Action` outright.
    *
    * A `None` entry is an explicit, checked declaration that a request
    * needs no authorization at all (e.g. `Filesystem.Resolve`, pure
    * path-string arithmetic with zero I/O) — distinct from an omitted
    * entry, which `completeness` still flags as an ungated request.
    */
  final case class EffectFamily(fragment: Fragment, requestActions: Map[String, Option[Effects.Action]])

  private val randomFragment: Fragment = Fragment(
    name = "effect.random",
    provides = List("effect.random"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("bytes", "Request", List("Int")),
      CtorDef("bytesValue", "Response", List("Bytes")),
      CtorDef("unavailable", "Error", List("Str"))))

  val random: EffectFamily = EffectFamily(randomFragment, Map(
    "bytes" -> Some(Effects.Action.RandomBytes)))

  private val clockFragment: Fragment = Fragment(
    name = "effect.clock",
    provides = List("effect.clock"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("now", "Request", Nil),
      CtorDef("timestampSlug", "Request", Nil),
      CtorDef("instant", "Response", List("Int")),
      CtorDef("slug", "Response", List("Str")),
      CtorDef("unavailable", "Error", List("Str"))))

  val clock: EffectFamily = EffectFamily(clockFragment, Map(
    "now" -> Some(Effects.Action.ClockNow),
    "timestampSlug" -> Some(Effects.Action.ClockTimestampSlug)))

  private val processFragment: Fragment = Fragment(
    name = "effect.process",
    provides = List("effect.process"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("run", "Request", List("Command", "Path", "Bool")),
      CtorDef("result", "Response", List("Int", "Str", "Str")),
      CtorDef("notFound", "Error", List("Str")),
      CtorDef("io", "Error", List("Str"))))

  val process: EffectFamily = EffectFamily(processFragment, Map(
    "run" -> Some(Effects.Action.ProcessRun)))

  private val externalBackendFragment: Fragment = Fragment(
    name = "effect.externalBackend",
    provides = List("effect.externalBackend"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("find", "Request", List("Host")),
      CtorDef("run", "Request", List("Host", "Args", "Path")),
      CtorDef("found", "Response", List("Path")),
      CtorDef("missing", "Response", List("Host")),
      CtorDef("processResult", "Response", List("Int", "Str")),
      CtorDef("io", "Error", List("Str"))))

  val externalBackend: EffectFamily = EffectFamily(externalBackendFragment, Map(
    "find" -> Some(Effects.Action.BackendFind),
    "run" -> Some(Effects.Action.BackendRun)))

  private val terminalFragment: Fragment = Fragment(
    name = "effect.terminal",
    provides = List("effect.terminal"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("readLine", "Request", Nil),
      CtorDef("write", "Request", List("Str")),
      CtorDef("writeLine", "Request", List("Str")),
      CtorDef("line", "Response", List("Str")),
      CtorDef("eof", "Response", Nil),
      CtorDef("ok", "Response", Nil),
      CtorDef("closed", "Error", Nil),
      CtorDef("io", "Error", List("Str"))))

  /** `write` and `writeLine` are both terminal-output operations, gated by
    * the same right — the first non-retrofitted use of the many-to-one
    * grouping (contrast with random/clock/process/externalBackend above,
    * each of which happened to be 1:1).
    */
  val terminal: EffectFamily = EffectFamily(terminalFragment, Map(
    "readLine" -> Some(Effects.Action.TerminalRead),
    "write" -> Some(Effects.Action.TerminalWrite),
    "writeLine" -> Some(Effects.Action.TerminalWrite)))

  private val workspaceFragment: Fragment = Fragment(
    name = "effect.workspace",
    provides = List("effect.workspace"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("languageDirs", "Request", Nil),
      CtorDef("listCairnFiles", "Request", List("Path")),
      CtorDef("listSubdirs", "Request", List("Path")),
      CtorDef("listSurfaceCairnFiles", "Request", List("Path")),
      CtorDef("readText", "Request", List("Path")),
      CtorDef("paths", "Response", List("Paths")),
      CtorDef("text", "Response", List("Str")),
      CtorDef("io", "Error", List("Str"))))

  /** All 5 requests are read-only discovery/reading (confirmed via
    * `system-handler.PackFiles.perform` — nothing mutates), so all 5 map to
    * the single existing `WorkspaceRead` right — a genuine 5-to-1 grouping,
    * unlike `Terminal`'s 2-to-1, with no drift to fix.
    */
  val workspace: EffectFamily = EffectFamily(workspaceFragment, Map(
    "languageDirs" -> Some(Effects.Action.WorkspaceRead),
    "listCairnFiles" -> Some(Effects.Action.WorkspaceRead),
    "listSubdirs" -> Some(Effects.Action.WorkspaceRead),
    "listSurfaceCairnFiles" -> Some(Effects.Action.WorkspaceRead),
    "readText" -> Some(Effects.Action.WorkspaceRead)))

  private val filesystemFragment: Fragment = Fragment(
    name = "effect.filesystem",
    provides = List("effect.filesystem"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("read", "Request", List("Path")),
      CtorDef("write", "Request", List("Path", "Str")),
      CtorDef("writeBytes", "Request", List("Path", "Bytes")),
      CtorDef("mkdirs", "Request", List("Path")),
      CtorDef("exists", "Request", List("Path")),
      CtorDef("isDirectory", "Request", List("Path")),
      CtorDef("isRegularFile", "Request", List("Path")),
      CtorDef("isExecutable", "Request", List("Path")),
      CtorDef("list", "Request", List("Path")),
      CtorDef("delete", "Request", List("Path")),
      CtorDef("createTempDirectory", "Request", List("Str")),
      CtorDef("resolve", "Request", List("Path", "Path")),
      CtorDef("text", "Response", List("Str")),
      CtorDef("bytes", "Response", List("Bytes")),
      CtorDef("ok", "Response", Nil),
      CtorDef("bool", "Response", List("Bool")),
      CtorDef("entries", "Response", List("Paths")),
      CtorDef("pathValue", "Response", List("Path")),
      CtorDef("notFound", "Error", List("Path")),
      CtorDef("io", "Error", List("Str"))))

  /** Three judgment calls, made explicitly rather than guessed at (asked of
    * the user directly): `delete` groups under the existing `FsWrite`
    * (mutating, like write/writeBytes — no family has finer-grained rights
    * than broad classes yet); `createTempDirectory` groups under `FsMkdirs`
    * (it creates a directory, regardless of the path being system-chosen);
    * `resolve` needs no right at all — confirmed via
    * `system-handler.Filesystem.perform`, it's pure path-string arithmetic
    * with zero `Files.*` calls, so it can't leak, corrupt, or touch
    * anything. No new `Action` needed: the existing 3 (`FsRead`/`FsWrite`/
    * `FsMkdirs`) already cover all 11 authorized requests.
    */
  val filesystem: EffectFamily = EffectFamily(filesystemFragment, Map(
    "read" -> Some(Effects.Action.FsRead),
    "exists" -> Some(Effects.Action.FsRead),
    "isDirectory" -> Some(Effects.Action.FsRead),
    "isRegularFile" -> Some(Effects.Action.FsRead),
    "isExecutable" -> Some(Effects.Action.FsRead),
    "list" -> Some(Effects.Action.FsRead),
    "write" -> Some(Effects.Action.FsWrite),
    "writeBytes" -> Some(Effects.Action.FsWrite),
    "delete" -> Some(Effects.Action.FsWrite),
    "mkdirs" -> Some(Effects.Action.FsMkdirs),
    "createTempDirectory" -> Some(Effects.Action.FsMkdirs),
    "resolve" -> None))

  private val lspFragment: Fragment = Fragment(
    name = "effect.lsp",
    provides = List("effect.lsp"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("readMessage", "Request", Nil),
      CtorDef("writeMessage", "Request", List("Str")),
      CtorDef("message", "Response", List("Str")),
      CtorDef("sessionEnded", "Response", Nil),
      CtorDef("ok", "Response", Nil),
      CtorDef("framing", "Error", List("Str")),
      CtorDef("closed", "Error", Nil)))

  /** `ReadMessage`/`WriteMessage` are symmetric Content-Length-framed stdio
    * operations, structurally identical to `Terminal.ReadLine`/`Write` — not
    * a session-establishment concept. The prior `Action` (`LspServe`) was a
    * confirmed orphan (referenced nowhere outside its own declaration, not
    * matching either Request case), replaced with `LspRead`/`LspWrite`.
    */
  val lsp: EffectFamily = EffectFamily(lspFragment, Map(
    "readMessage" -> Some(Effects.Action.LspRead),
    "writeMessage" -> Some(Effects.Action.LspWrite)))

  val families: Map[Effects.Family, EffectFamily] = Map(
    Effects.Family.Random -> random,
    Effects.Family.Clock -> clock,
    Effects.Family.Process -> process,
    Effects.Family.ExternalBackend -> externalBackend,
    Effects.Family.Terminal -> terminal,
    Effects.Family.Workspace -> workspace,
    Effects.Family.Filesystem -> filesystem,
    Effects.Family.Lsp -> lsp)

  /** The rights vocabulary for a family: every distinct [[Effects.Action]]
    * its `requestActions` grouping declares (`None` entries contribute
    * nothing — they declare "no right needed," not a right).
    */
  def actions(family: EffectFamily): Set[Effects.Action] = family.requestActions.values.flatten.toSet

  /** Checked correspondence between a family's Fragment and its grouping:
    * every `Request`-sorted constructor must have a `requestActions` entry
    * (no ungated request — though the entry may explicitly be `None`),
    * every entry must name a real constructor (no dangling grouping), and
    * every `Some`-mapped `Action` must actually belong to `expected` (no
    * cross-family mistagging). Empty result = consistent.
    */
  def completeness(family: EffectFamily, expected: Effects.Family): List[String] =
    val reqCtorNames = family.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    val mappedNames = family.requestActions.keySet
    val missing = reqCtorNames.diff(mappedNames).toList.map(n =>
      s"constructor '$n' has no requestActions entry")
    val dangling = mappedNames.diff(reqCtorNames).toList.map(n =>
      s"requestActions entry '$n' has no matching Request constructor")
    val misfamily = family.requestActions.toList.collect {
      case (n, Some(a)) if a.family != expected =>
        s"requestActions('$n') = $a is tagged ${a.family}, not $expected"
    }
    missing ++ dangling ++ misfamily
