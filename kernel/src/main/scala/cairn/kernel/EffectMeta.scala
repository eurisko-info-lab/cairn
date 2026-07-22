package cairn.kernel

/** Meta-defined effect interfaces: each effect family's Request/Response/Error
  * vocabulary as a Kernel-owned [[Fragment]], plus declared [[ActionKey]]s and
  * a [[ResourceSchema]] — the artifact from which typed rights and resource
  * kinds are derived rather than hand-maintained beside the Fragment.
  *
  * Rights are many-to-one: several `Request`-sorted constructors can share
  * one action key (e.g. Filesystem's dozen request shapes → read/write/mkdirs).
  * [[InterfaceDecl.requestActions]] maps constructor name → declared action
  * name (or `None` for ungated requests such as `Filesystem.resolve`).
  *
  * Declaration SoT lives on disk as languages/effect-<family>/iface.cairn modules
  * of the `effect-interface` language; vocabulary SoT is
  * languages/effect-<family>.cairn. Host [[packDecls]] + embedded Fragments are
  * cold-start seeds verified by runtime EffectBootstrap.
  *
  * [[Effects.Family]] is a thin JVM routing tag (cases ↔ packDecls family ids);
  * action lists are **not** hand-maintained — they come from pack declarations
  * / [[ActionKey]] digests. [[completeness]] checks Request↔gate↔action decls.
  * Policies and EffectRequest construction use [[ActionKey]] /
  * [[ResourceSchema.at]].
  */
object EffectMeta:

  /** Typed path-shape vocabulary for effect-interface declarations.
    *
    * Encoded on disk as the `path "..."` string in `iface.cairn`. Handlers
    * still authorize via [[Authority.Resource.matches]]; this enum is the
    * schema that documents (and optionally validates) concrete path values
    * so families do not lag on ad-hoc `"*"` / free strings.
    */
  enum PathPattern:
    /** No per-request target (Clock / Random / Terminal / Lsp). */
    case Unscoped
    /** Filesystem- or workspace-style path (also LedgerTransport node roots). */
    case FsPath
    /** Process executable name (or `name+args` after Process narrowing). */
    case Command
    /** ExternalBackend host id (`scala-cli`, `cargo`, …). */
    case Host
    /** CAS content digest (hex). */
    case Digest
    /** CAS: digest for put/get/contains; store root path for admin. */
    case DigestOrPath

    def encode: String = this match
      case Unscoped     => "*"
      case FsPath       => "Path"
      case Command      => "Command"
      case Host         => "Host"
      case Digest       => "Digest"
      case DigestOrPath => "Digest|Path"

    /** Whether `path` is a well-shaped concrete value *or* a policy wildcard
      * (`*` / `prefix*`). Wildcard forms are always accepted so grant/policy
      * attenuation stays orthogonal to concrete shape checks.
      */
    def accepts(path: String): Boolean =
      if path == "*" || path.endsWith("*") then true
      else this match
        case Unscoped     => true
        case FsPath       => path.nonEmpty
        case Command      => path.nonEmpty
        case Host         => path.nonEmpty
        case Digest       => PathPattern.isHexDigest(path)
        case DigestOrPath => PathPattern.isHexDigest(path) || path.nonEmpty

  object PathPattern:
    private val HexDigest = raw"(?i)[0-9a-f]{64}".r
    def isHexDigest(s: String): Boolean = HexDigest.matches(s)

    def parse(s: String): Either[String, PathPattern] = s match
      case "*"           => Right(Unscoped)
      case "Path"        => Right(FsPath)
      case "Command"     => Right(Command)
      case "Host"        => Right(Host)
      case "Digest"      => Right(Digest)
      case "Digest|Path" => Right(DigestOrPath)
      case other         => Left(s"unknown resource path pattern '$other'")

    def known: List[PathPattern] =
      List(Unscoped, FsPath, Command, Host, Digest, DigestOrPath)

  /** Per-family resource language: kind + typed [[PathPattern]], bound to the
    * effect-interface Fragment digest when constructed from an [[EffectFamily]].
    * `pathPattern` is the on-disk encoding (`Path`, `Command`, `Host`,
    * `Digest`, `Digest|Path`, `*`); matching still uses
    * [[Authority.Resource.matches]] (exact / prefix-wildcard / `*`).
    */
  final case class ResourceSchema(
      kind: String,
      pathPattern: String,
      interfaceDigest: Option[Digest] = None):
    def pattern: Either[String, PathPattern] = PathPattern.parse(pathPattern)

    /** Construct a resource; rejects concrete paths that violate [[pattern]]. */
    def at(path: String): Authority.Resource =
      pattern match
        case Right(p) if !p.accepts(path) =>
          throw IllegalArgumentException(
            s"resource path '$path' does not match schema $kind/$pathPattern")
        case Left(err) =>
          throw IllegalArgumentException(s"resource schema $kind: $err")
        case Right(_) =>
          Authority.Resource(kind, path)

    def atChecked(path: String): Either[String, Authority.Resource] =
      pattern.flatMap { p =>
        if p.accepts(path) then Right(Authority.Resource(kind, path))
        else Left(s"resource path '$path' does not match schema $kind/$pathPattern")
      }

    def any: Authority.Resource = Authority.Resource(kind, "*")

  /** Action / resource / gating declaration — the half of an effect interface
    * that is not the Request/Response/Error vocabulary Fragment.
    * Disk SoT: `effect-interface` module items (`family`/`kind`/`path`/
    * `action`/`gate`/`ungated`).
    */
  final case class InterfaceDecl(
      familyId: String,
      actions: List[String],
      resourceKind: String,
      resourcePathPattern: String,
      requestActions: Map[String, Option[String]]):
    def family: Either[String, Effects.Family] =
      Effects.Family.fromId(familyId).toRight(s"unknown effect family '$familyId'")

  object InterfaceDecl:
    /** Rebuild a declaration from `effect-interface` Item terms (module bodies). */
    def fromItems(items: List[Cst]): Either[String, InterfaceDecl] =
      scala.util.boundary:
        var familyId: Option[String] = None
        var kind: Option[String] = None
        var path: Option[String] = None
        val acts = List.newBuilder[String]
        val gates = Map.newBuilder[String, Option[String]]
        items.foreach {
          case Cst.Node("family", List(Cst.Leaf(n))) =>
            if familyId.isDefined then scala.util.boundary.break(Left("duplicate family item"))
            familyId = Some(n)
          case Cst.Node("kind", List(Cst.Leaf(n))) =>
            if kind.isDefined then scala.util.boundary.break(Left("duplicate kind item"))
            kind = Some(n)
          case Cst.Node("path", List(Cst.Leaf(s))) =>
            if path.isDefined then scala.util.boundary.break(Left("duplicate path item"))
            path = Some(s)
          case Cst.Node("action", List(Cst.Leaf(n))) =>
            acts += n
          case Cst.Node("gate", List(Cst.Leaf(ctor), Cst.Leaf(act))) =>
            gates += ctor -> Some(act)
          case Cst.Node("ungated", List(Cst.Leaf(ctor))) =>
            gates += ctor -> None
          case other =>
            scala.util.boundary.break(Left(s"not an effect-interface item: ${other.render}"))
        }
        for
          f <- familyId.toRight("missing family item")
          k <- kind.toRight("missing kind item")
          p <- path.toRight("missing path item")
          _ <- PathPattern.parse(p)
        yield InterfaceDecl(f, acts.result(), k, p, gates.result())

    def fromModule(defs: List[(String, Cst)]): Either[String, InterfaceDecl] =
      fromItems(defs.map(_._2))

    def fromFamily(ef: EffectFamily): InterfaceDecl =
      InterfaceDecl(
        ef.family.toString, ef.actions, ef.resourceKind, ef.resourcePathPattern, ef.requestActions)

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

    def decl: InterfaceDecl = InterfaceDecl.fromFamily(this)

  /** Pack name → cold-start declaration seed (mirrors languages/effect-<family>/iface.cairn). */
  val packDecls: Map[String, InterfaceDecl] = Map(
    "effect-clock" -> InterfaceDecl(
      "Clock", List("now", "timestampSlug"), "clock", "*",
      Map("now" -> Some("now"), "timestampSlug" -> Some("timestampSlug"))),
    "effect-random" -> InterfaceDecl(
      "Random", List("bytes"), "random", "*",
      Map("bytes" -> Some("bytes"))),
    "effect-process" -> InterfaceDecl(
      "Process", List("run"), "process", "Command",
      Map("run" -> Some("run"))),
    "effect-externalBackend" -> InterfaceDecl(
      "ExternalBackend", List("find", "run"), "externalBackend", "Host",
      Map("find" -> Some("find"), "run" -> Some("run"))),
    "effect-terminal" -> InterfaceDecl(
      "Terminal", List("read", "write"), "terminal", "*",
      Map("readLine" -> Some("read"), "write" -> Some("write"), "writeLine" -> Some("write"))),
    "effect-workspace" -> InterfaceDecl(
      "Workspace", List("read"), "workspace", "Path",
      Map(
        "languageDirs" -> Some("read"), "listCairnFiles" -> Some("read"),
        "listSubdirs" -> Some("read"), "listSurfaceCairnFiles" -> Some("read"),
        "readText" -> Some("read"))),
    "effect-filesystem" -> InterfaceDecl(
      "Filesystem", List("read", "write", "mkdirs"), "filesystem", "Path",
      Map(
        "read" -> Some("read"), "readBytes" -> Some("read"), "exists" -> Some("read"),
        "isDirectory" -> Some("read"), "isRegularFile" -> Some("read"),
        "isExecutable" -> Some("read"), "list" -> Some("read"),
        "write" -> Some("write"), "writeBytes" -> Some("write"), "delete" -> Some("write"),
        "mkdirs" -> Some("mkdirs"), "createTempDirectory" -> Some("mkdirs"),
        "resolve" -> None)),
    "effect-lsp" -> InterfaceDecl(
      "Lsp", List("read", "write"), "lsp", "*",
      Map("readMessage" -> Some("read"), "writeMessage" -> Some("write"))),
    "effect-cas" -> InterfaceDecl(
      "Cas", List("put", "get", "fsck", "gc", "stats"), "cas", "Digest|Path",
      Map(
        "put" -> Some("put"), "get" -> Some("get"), "contains" -> Some("get"),
        "fsck" -> Some("fsck"), "gc" -> Some("gc"), "stats" -> Some("stats"))),
    "effect-ledgerTransport" -> InterfaceDecl(
      "LedgerTransport", List("append"), "ledger", "Path",
      Map("append" -> Some("append"))))

  /** Pack names under `languages/` for fragment-loaded effect interfaces. */
  val fragmentPackNames: List[String] = List(
    "effect-clock", "effect-random", "effect-process", "effect-externalBackend",
    "effect-terminal", "effect-workspace", "effect-filesystem", "effect-lsp",
    "effect-cas", "effect-ledgerTransport")

  /** Thin Family bridge: pack name → host Family enum (interpreter routing). */
  val packFamily: Map[String, Effects.Family] =
    packDecls.flatMap { (pack, d) => d.family.toOption.map(pack -> _) }

  private def seedFamily(pack: String, fragment: Fragment): EffectFamily =
    fromFragmentPack(pack, fragment).fold(e => throw IllegalStateException(e), identity)

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

  val random: EffectFamily = seedFamily("effect-random", randomFragment)

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

  val clock: EffectFamily = seedFamily("effect-clock", clockFragment)

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

  val process: EffectFamily = seedFamily("effect-process", processFragment)

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

  val externalBackend: EffectFamily = seedFamily("effect-externalBackend", externalBackendFragment)

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

  val terminal: EffectFamily = seedFamily("effect-terminal", terminalFragment)

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

  val workspace: EffectFamily = seedFamily("effect-workspace", workspaceFragment)

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

  val filesystem: EffectFamily = seedFamily("effect-filesystem", filesystemFragment)

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

  val lsp: EffectFamily = seedFamily("effect-lsp", lspFragment)

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

  val cas: EffectFamily = seedFamily("effect-cas", casFragment)

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

  val ledgerTransport: EffectFamily = seedFamily("effect-ledgerTransport", ledgerTransportFragment)

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

  /** All action keys declared by Meta-defined families (pack SoT). */
  def derivedActionKeys: Set[Effects.ActionKey] =
    families.values.flatMap(_.actionKeys).toSet

  /** Action keys implied by cold-start [[packDecls]] (unbound — no Fragment digest). */
  def packDeclActionKeys: Set[Effects.ActionKey] =
    packDecls.values.flatMap { d =>
      Effects.Family.fromId(d.familyId).toList.flatMap(f =>
        d.actions.map(a => Effects.ActionKey.of(f, a)))
    }.toSet

  /** Every known digest-bound action key (Meta families). No host Action enum. */
  def allActionKeys: Set[Effects.ActionKey] = derivedActionKeys

  /** Rights vocabulary projected from declared actions. */
  def actions(family: EffectFamily): Set[Effects.ActionKey] = family.actionKeys

  def completeness(family: EffectFamily): List[String] =
    completeness(family, family.family)

  /** Checked correspondence: Request ctors ↔ requestActions and declared
    * action names ↔ requestActions targets. Empty result = consistent.
    * (No hand-maintained Action enum to cross-check.)
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
    famErr ++ missing ++ dangling ++ undeclared ++ unusedDecl

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

  /** Rebuild an [[EffectFamily]] from a vocabulary Fragment + [[InterfaceDecl]].
    * Declarations come from disk (`iface.cairn`) or cold-start [[packDecls]].
    */
  def familyFromFragment(
      fragment: Fragment,
      family: Effects.Family,
      actions: List[String],
      resourceKind: String,
      resourcePathPattern: String,
      requestActions: Map[String, Option[String]]
  ): Either[String, EffectFamily] =
    val ef = EffectFamily(
      fragment, family, actions, resourceKind, resourcePathPattern, requestActions)
    val errs = completeness(ef)
    if errs.nonEmpty then Left(errs.mkString("; ")) else Right(ef)

  def familyFrom(fragment: Fragment, decl: InterfaceDecl): Either[String, EffectFamily] =
    decl.family.flatMap(f =>
      familyFromFragment(
        fragment, f, decl.actions, decl.resourceKind, decl.resourcePathPattern, decl.requestActions))

  def fromFragmentPack(name: String, fragment: Fragment, decl: InterfaceDecl): Either[String, EffectFamily] =
    familyFrom(fragment, decl).flatMap { ef =>
      packFamily.get(name) match
        case Some(expected) if ef.family != expected =>
          Left(s"pack '$name' declares family ${ef.family}, expected $expected")
        case None if !packDecls.contains(name) =>
          Left(s"unknown effect pack '$name'")
        case _ => Right(ef)
    }

  def fromFragmentPack(name: String, fragment: Fragment): Either[String, EffectFamily] =
    packDecls.get(name) match
      case None => Left(s"unknown effect pack '$name'")
      case Some(d) => fromFragmentPack(name, fragment, d)

  def clockFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-clock", fragment)
  def randomFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-random", fragment)
  def processFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-process", fragment)
  def externalBackendFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-externalBackend", fragment)
  def terminalFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-terminal", fragment)
  def workspaceFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-workspace", fragment)
  def filesystemFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-filesystem", fragment)
  def lspFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-lsp", fragment)
  def casFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-cas", fragment)
  def ledgerTransportFromFragment(fragment: Fragment): Either[String, EffectFamily] =
    fromFragmentPack("effect-ledgerTransport", fragment)

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
      Effects.Family.fromId(famName) match
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
