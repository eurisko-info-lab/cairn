package cairn.kernel

/** Meta-defined effect interfaces: each effect family's Request/Response/Error
  * vocabulary as a Kernel-owned [[Fragment]], plus declared [[ActionKey]]s and
  * a [[ResourceSchema]] — the artifact from which typed rights and resource
  * kinds are derived rather than hand-maintained beside the Fragment.
  *
  * Rights are many-to-one: several `Request`-sorted constructors can share
  * one action key (e.g. Filesystem's dozen request shapes → read/write/mkdirs).
  * `requestActions` maps constructor name → declared action name (or `None`
  * for ungated requests such as `Filesystem.resolve`).
  *
  * [[Effects.Action]] remains a closed host bridge; [[completeness]] checks
  * derived keys against it. Policies and EffectRequest construction use
  * [[ActionKey]] / [[ResourceSchema.at]].
  */
object EffectMeta:

  /** Per-family resource language: kind + informal path grammar, bound to the
    * effect-interface Fragment digest when constructed from an [[EffectFamily]].
    * `pathPattern` documents how path values are shaped (`Path`, `Command`,
    * `Host`, `*` for unscoped families); matching still uses
    * [[Authority.Resource.matches]] (exact / prefix-wildcard / `*`).
    */
  final case class ResourceSchema(
      kind: String,
      pathPattern: String,
      interfaceDigest: Option[Digest] = None):
    def at(path: String): Authority.Resource = Authority.Resource(kind, path)
    def any: Authority.Resource = Authority.Resource(kind, "*")

  /** Fragment + declared actions + resource schema + request→action grouping.
    * Action keys and resources are derived from these declarations and bound
    * to [[fragment]].digest.
    */
  final case class EffectFamily(
      fragment: Fragment,
      family: Effects.Family,
      /** Declared capability-class names (distinct [[Effects.ActionKey]] names). */
      actions: List[String],
      resourceKind: String,
      resourcePathPattern: String,
      /** Request ctor name → declared action name, or `None` if ungated. */
      requestActions: Map[String, Option[String]],
  ):
    def resource: ResourceSchema =
      ResourceSchema(resourceKind, resourcePathPattern, Some(fragment.digest))

    def actionKeys: Set[Effects.ActionKey] =
      actions.map(actionKey).toSet

    def actionKey(name: String): Effects.ActionKey =
      Effects.ActionKey.bound(family, name, fragment.digest)

    /** Resolve a Request constructor to its derived action key, if gated. */
    def keyFor(ctor: String): Option[Effects.ActionKey] =
      requestActions.get(ctor).flatten.map(actionKey)

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

  val random: EffectFamily = EffectFamily(
    randomFragment,
    Effects.Family.Random,
    actions = List("bytes"),
    resourceKind = "random",
    resourcePathPattern = "*",
    requestActions = Map("bytes" -> Some("bytes")))

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

  val clock: EffectFamily = EffectFamily(
    clockFragment,
    Effects.Family.Clock,
    actions = List("now", "timestampSlug"),
    resourceKind = "clock",
    resourcePathPattern = "*",
    requestActions = Map(
      "now" -> Some("now"),
      "timestampSlug" -> Some("timestampSlug")))

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

  val process: EffectFamily = EffectFamily(
    processFragment,
    Effects.Family.Process,
    actions = List("run"),
    resourceKind = "process",
    resourcePathPattern = "Command",
    requestActions = Map("run" -> Some("run")))

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

  val externalBackend: EffectFamily = EffectFamily(
    externalBackendFragment,
    Effects.Family.ExternalBackend,
    actions = List("find", "run"),
    resourceKind = "externalBackend",
    resourcePathPattern = "Host",
    requestActions = Map(
      "find" -> Some("find"),
      "run" -> Some("run")))

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

  val terminal: EffectFamily = EffectFamily(
    terminalFragment,
    Effects.Family.Terminal,
    actions = List("read", "write"),
    resourceKind = "terminal",
    resourcePathPattern = "*",
    requestActions = Map(
      "readLine" -> Some("read"),
      "write" -> Some("write"),
      "writeLine" -> Some("write")))

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

  val workspace: EffectFamily = EffectFamily(
    workspaceFragment,
    Effects.Family.Workspace,
    actions = List("read"),
    resourceKind = "workspace",
    resourcePathPattern = "Path",
    requestActions = Map(
      "languageDirs" -> Some("read"),
      "listCairnFiles" -> Some("read"),
      "listSubdirs" -> Some("read"),
      "listSurfaceCairnFiles" -> Some("read"),
      "readText" -> Some("read")))

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
      CtorDef("readBytes", "Request", List("Path")),
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

  val filesystem: EffectFamily = EffectFamily(
    filesystemFragment,
    Effects.Family.Filesystem,
    actions = List("read", "write", "mkdirs"),
    resourceKind = "filesystem",
    resourcePathPattern = "Path",
    requestActions = Map(
      "read" -> Some("read"),
      "readBytes" -> Some("read"),
      "exists" -> Some("read"),
      "isDirectory" -> Some("read"),
      "isRegularFile" -> Some("read"),
      "isExecutable" -> Some("read"),
      "list" -> Some("read"),
      "write" -> Some("write"),
      "writeBytes" -> Some("write"),
      "delete" -> Some("write"),
      "mkdirs" -> Some("mkdirs"),
      "createTempDirectory" -> Some("mkdirs"),
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

  val lsp: EffectFamily = EffectFamily(
    lspFragment,
    Effects.Family.Lsp,
    actions = List("read", "write"),
    resourceKind = "lsp",
    resourcePathPattern = "*",
    requestActions = Map(
      "readMessage" -> Some("read"),
      "writeMessage" -> Some("write")))

  /** CAS put/get/contains + admin (fsck/gc/stats) — Meta-defined interface. */
  private val casFragment: Fragment = Fragment(
    name = "effect.cas",
    provides = List("effect.cas"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("put", "Request", List("Artifact")),
      CtorDef("get", "Request", List("Digest")),
      CtorDef("contains", "Request", List("Digest")),
      CtorDef("fsck", "Request", List("Path")),
      CtorDef("gc", "Request", List("Path", "Digests")),
      CtorDef("stats", "Request", List("Path")),
      CtorDef("typedKey", "Response", List("TypedKey")),
      CtorDef("artifact", "Response", List("Artifact")),
      CtorDef("bool", "Response", List("Bool")),
      CtorDef("fsckReport", "Response", List("FsckReport")),
      CtorDef("gcReport", "Response", List("GcReport")),
      CtorDef("statsReport", "Response", List("Stats")),
      CtorDef("missing", "Error", List("Digest")),
      CtorDef("io", "Error", List("Str"))))

  val cas: EffectFamily = EffectFamily(
    casFragment,
    Effects.Family.Cas,
    actions = List("put", "get", "fsck", "gc", "stats"),
    resourceKind = "cas",
    resourcePathPattern = "*",
    requestActions = Map(
      "put" -> Some("put"),
      "get" -> Some("get"),
      "contains" -> Some("get"),
      "fsck" -> Some("fsck"),
      "gc" -> Some("gc"),
      "stats" -> Some("stats")))

  /** Ledger transport append — Meta-defined over Node.append. */
  private val ledgerTransportFragment: Fragment = Fragment(
    name = "effect.ledgerTransport",
    provides = List("effect.ledgerTransport"),
    requires = Nil,
    sorts = List(
      SortDef("Request", SortMode.Tree),
      SortDef("Response", SortMode.Tree),
      SortDef("Error", SortMode.Tree)),
    constructors = List(
      CtorDef("append", "Request", List("Authority", "Txs")),
      CtorDef("block", "Response", List("Block")),
      CtorDef("denied", "Error", List("Str")),
      CtorDef("io", "Error", List("Str"))))

  val ledgerTransport: EffectFamily = EffectFamily(
    ledgerTransportFragment,
    Effects.Family.LedgerTransport,
    actions = List("append"),
    resourceKind = "ledger",
    resourcePathPattern = "Path",
    requestActions = Map("append" -> Some("append")))

  val families: Map[Effects.Family, EffectFamily] = Map(
    Effects.Family.Random -> random,
    Effects.Family.Clock -> clock,
    Effects.Family.Process -> process,
    Effects.Family.ExternalBackend -> externalBackend,
    Effects.Family.Terminal -> terminal,
    Effects.Family.Workspace -> workspace,
    Effects.Family.Filesystem -> filesystem,
    Effects.Family.Lsp -> lsp,
    Effects.Family.Cas -> cas,
    Effects.Family.LedgerTransport -> ledgerTransport)

  /** All action keys declared by Meta-defined families. */
  def derivedActionKeys: Set[Effects.ActionKey] =
    families.values.flatMap(_.actionKeys).toSet

  /** Formerly host-only Cas/Ledger; now empty — retained for migration checks. */
  def hostOnlyActionKeys: Set[Effects.ActionKey] =
    Effects.Action.values.map(_.key).toSet -- derivedActionKeys

  /** Every known action key: derived + residual host-only bridge. */
  def allActionKeys: Set[Effects.ActionKey] =
    derivedActionKeys ++ hostOnlyActionKeys

  /** Rights vocabulary projected from declared actions. */
  def actions(family: EffectFamily): Set[Effects.ActionKey] = family.actionKeys

  /** Host enum cases corresponding to a family's derived keys. */
  def hostActions(family: EffectFamily): Set[Effects.Action] =
    family.actionKeys.flatMap(_.toHost)

  def completeness(family: EffectFamily): List[String] =
    completeness(family, family.family)

  /** Checked correspondence: Request ctors ↔ requestActions, declared
    * action names ↔ requestActions targets, and derived keys ↔ host enum
    * for the expected family. Empty result = consistent.
    */
  def completeness(family: EffectFamily, expected: Effects.Family): List[String] =
    val famErr =
      if family.family != expected then
        List(s"EffectFamily.family is ${family.family}, expected $expected")
      else Nil
    val reqCtorNames = family.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    val mappedNames = family.requestActions.keySet
    val missing = reqCtorNames.diff(mappedNames).toList.map(n =>
      s"constructor '$n' has no requestActions entry")
    val dangling = mappedNames.diff(reqCtorNames).toList.map(n =>
      s"requestActions entry '$n' has no matching Request constructor")
    val declared = family.actions.toSet
    val usedNames = family.requestActions.values.flatten.toSet
    val undeclared = usedNames.diff(declared).toList.map(n =>
      s"requestActions targets undeclared action '$n'")
    val unusedDecl = declared.diff(usedNames).toList.map(n =>
      s"declared action '$n' is never targeted by requestActions")
    val hostMismatch = family.actionKeys.toList.flatMap { k =>
      k.toHost match
        case None =>
          List(s"derived ActionKey ${k.id} has no host Effects.Action bridge")
        case Some(a) if a.family != expected =>
          List(s"ActionKey ${k.id} bridges to $a tagged ${a.family}, not $expected")
        case Some(_) => Nil
    }
    val hostExtra = Effects.Action.values.filter(_.family == expected).toSet.diff(hostActions(family)).toList.map(a =>
      s"host Action $a has no matching derived ActionKey in the EffectFamily")
    famErr ++ missing ++ dangling ++ undeclared ++ unusedDecl ++ hostMismatch ++ hostExtra

  opaque type PinnedInterface = EffectFamily
  object PinnedInterface:
    /** Host-embedded family as an already-accepted pin (bootstrap / tests). */
    def fromHost(family: EffectFamily): PinnedInterface = family

    /** Decode + verify a pinned interface artifact (kind [[ArtifactKind.Fragment]]
      * envelope under tag `effect-interface`). Rejects digest mismatch and
      * incomplete declarations.
      */
    def fromArtifact(a: Artifact): Either[String, PinnedInterface] =
      if a.kind != ArtifactKind.Fragment then
        Left(s"pinned effect interface must be fragment-kind, got ${a.kind.name}")
      else a.body match
        case Canon.CTag("effect-interface", body) =>
          decodeFamily(body).flatMap { ef =>
            val errs = completeness(ef)
            if errs.nonEmpty then Left(errs.mkString("; "))
            else
              val expected = interfaceArtifact(ef)
              if expected.digest != a.digest then
                Left(s"pinned interface digest mismatch: got ${a.digest.short}, expected ${expected.digest.short}")
              else Right(ef)
          }
        case _ => Left("pinned effect interface: missing effect-interface tag")

    extension (p: PinnedInterface)
      def family: Effects.Family = p.family
      def fragment: Fragment = p.fragment
      def actions: List[String] = p.actions
      def actionKeys: Set[Effects.ActionKey] = p.actionKeys
      def actionKey(name: String): Effects.ActionKey = p.actionKey(name)
      def resource: ResourceSchema = p.resource
      def asFamily: EffectFamily = p
      def artifact: Artifact = interfaceArtifact(p)
      def pinDigest: Digest = artifact.digest

  /** CAS-storable encoding of an [[EffectFamily]] (Fragment + declarations). */
  def interfaceArtifact(family: EffectFamily): Artifact =
    Artifact(ArtifactKind.Fragment, Canon.CTag("effect-interface", encodeFamily(family)))

  /** Accept a host-embedded family as pinned (digest = [[interfaceArtifact]]). */
  def pinHost(family: EffectFamily): PinnedInterface =
    PinnedInterface.fromHost(family)

  private def encodeFamily(ef: EffectFamily): Canon =
    val reqActs = ef.requestActions.toList.sortBy(_._1).map { (ctor, act) =>
      Canon.cmap(
        "ctor" -> Canon.CStr(ctor),
        "action" -> act.fold(Canon.CTag("none", Canon.CStr("")))(a => Canon.CTag("some", Canon.CStr(a))))
    }
    Canon.cmap(
      "family" -> Canon.CStr(ef.family.toString),
      "fragment" -> FragmentCodec.toCanon(ef.fragment),
      "actions" -> Canon.cstrs(ef.actions),
      "resourceKind" -> Canon.CStr(ef.resourceKind),
      "resourcePathPattern" -> Canon.CStr(ef.resourcePathPattern),
      "requestActions" -> Canon.CList(reqActs))

  private def decodeFamily(body: Canon): Either[String, EffectFamily] =
    try
      val famName = body.field("family").asStr
      Effects.Family.values.find(_.toString == famName) match
        case None => Left(s"unknown effect family '$famName'")
        case Some(f) =>
          val reqActs = body.field("requestActions").asList.map { c =>
            val ctor = c.field("ctor").asStr
            val act = c.field("action") match
              case Canon.CTag("none", _) => None
              case Canon.CTag("some", s) => Some(s.asStr)
              case other => throw IllegalArgumentException(s"bad action tag: $other")
            ctor -> act
          }.toMap
          Right(EffectFamily(
            FragmentCodec.fromCanon(body.field("fragment")),
            f,
            body.field("actions").asList.map(_.asStr),
            body.field("resourceKind").asStr,
            body.field("resourcePathPattern").asStr,
            reqActs))
    catch
      case e: Exception => Left(e.getMessage)
