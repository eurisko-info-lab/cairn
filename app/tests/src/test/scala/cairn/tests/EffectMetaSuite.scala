package cairn.tests

import cairn.kernel.*
import cairn.systeminterface.Random as RandomEffect
import cairn.systeminterface.Clock as ClockEffect
import cairn.systeminterface.Process as ProcessEffect
import cairn.systeminterface.ExternalBackend as BackendEffect
import cairn.systeminterface.Terminal as TerminalEffect
import cairn.systeminterface.Workspace as WorkspaceEffect
import cairn.systeminterface.Filesystem as FsEffect
import cairn.systeminterface.Lsp as LspEffect
import cairn.systeminterface.Cas as CasEffect
import cairn.systeminterface.LedgerTransport as LedgerEffect

/** Meta-defined effect interfaces (post-migration priority #1 / #4): all 8 live
  * effect families (`Random`, `Clock`, `Process`, `ExternalBackend`,
  * `Terminal`, `Workspace`, `Filesystem`, `Lsp`) are Fragment-defined with
  * declared [[Effects.ActionKey]]s and [[EffectMeta.ResourceSchema]].
  * EffectMeta's many-to-one requestActions grouping (`Terminal`'s
  * `write`/`writeLine` share one right; `Workspace`'s 5 requests all share
  * one; `Filesystem`'s 12 reduce to 3, with `resolve` needing none at all).
  * These are mechanical drift guards, same spirit as `ModuleBoundarySuite`.
  */
class EffectMetaSuite extends munit.FunSuite:

  test("EffectMeta.random composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.random", List(EffectMeta.random.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.random's requestActions is complete and matches packDecls Random actions exactly"):
    assertEquals(EffectMeta.completeness(EffectMeta.random), Nil)
    val derived = EffectMeta.actions(EffectMeta.random)
    val fromPack = EffectMeta.packDecls("effect-random").actions.map(a =>
      EffectMeta.random.actionKey(a)).toSet
    assertEquals(derived, fromPack)

  test("system-interface.Random.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Exhaustive match: adding a Request case without updating this table
    // (and EffectMeta.random) surfaces as a non-exhaustive-match warning here.
    def ctorNameOf(r: RandomEffect.Request): String = r match
      case RandomEffect.Request.Bytes(_) => "bytes"
    val scalaReqCases = Set(ctorNameOf(RandomEffect.Request.Bytes(0)))
    val fragmentReqCtors = EffectMeta.random.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.clock composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.clock", List(EffectMeta.clock.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.clock's requestActions is complete and matches packDecls actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.clock), Nil)
    val derived = EffectMeta.actions(EffectMeta.clock)
    val fromPack = EffectMeta.packDecls("effect-clock").actions.map(a =>
      EffectMeta.clock.actionKey(a)).toSet
    assertEquals(derived, fromPack)
    assertEquals(derived.size, 2) // Now + TimestampSlug — was 1 before that slice

  test("system-interface.Clock.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Clock.Request's cases are all parameterless, so .values works directly
    // (unlike Random's Bytes(n: Int), which needed an exhaustive match).
    val scalaReqCases = ClockEffect.Request.values.map(v =>
      (v.toString.head.toLower +: v.toString.tail)).toSet
    val fragmentReqCtors = EffectMeta.clock.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.process composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.process", List(EffectMeta.process.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.process's requestActions is complete and matches packDecls actions (no drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.process), Nil)
    val derived = EffectMeta.actions(EffectMeta.process)
    val fromPack = EffectMeta.packDecls("effect-process").actions.map(a =>
      EffectMeta.process.actionKey(a)).toSet
    assertEquals(derived, fromPack)
    assertEquals(derived.size, 1) // Run — matched cleanly, unlike Clock
    assertEquals(EffectMeta.process.resource.kind, "process")
    assertEquals(EffectMeta.process.resource.pathPattern, "Command")

  test("system-interface.Process.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Run(command, cwd, mergeStderr) takes parameters, so exhaustive match
    // like Random rather than Clock's .values.
    def ctorNameOf(r: ProcessEffect.Request): String = r match
      case ProcessEffect.Request.Run(_, _, _) => "run"
    val scalaReqCases = Set(ctorNameOf(ProcessEffect.Request.Run(Nil)))
    val fragmentReqCtors = EffectMeta.process.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.externalBackend composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.externalBackend", List(EffectMeta.externalBackend.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.externalBackend's requestActions is complete and matches packDecls actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.externalBackend), Nil)
    val derived = EffectMeta.actions(EffectMeta.externalBackend)
    val fromPack = EffectMeta.packDecls("effect-externalBackend").actions.map(a =>
      EffectMeta.externalBackend.actionKey(a)).toSet
    assertEquals(derived, fromPack)
    assertEquals(derived.size, 2) // Find + Run — was 1 (Run only) before that slice

  test("system-interface.ExternalBackend.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Find(host) and Run(host, args, cwd) both take parameters, so
    // exhaustive match, same as Random/Process.
    def ctorNameOf(r: BackendEffect.Request): String = r match
      case BackendEffect.Request.Find(_)       => "find"
      case BackendEffect.Request.Run(_, _, _)  => "run"
    val scalaReqCases = Set(
      ctorNameOf(BackendEffect.Request.Find(BackendEffect.Host.ScalaCli)),
      ctorNameOf(BackendEffect.Request.Run(BackendEffect.Host.ScalaCli, Nil)))
    val fragmentReqCtors = EffectMeta.externalBackend.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.terminal composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.terminal", List(EffectMeta.terminal.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.terminal's requestActions is complete and matches packDecls actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.terminal), Nil)
    val derived = EffectMeta.actions(EffectMeta.terminal)
    val fromPack = EffectMeta.packDecls("effect-terminal").actions.map(a =>
      EffectMeta.terminal.actionKey(a)).toSet
    assertEquals(derived, fromPack)
    assertEquals(derived.size, 2) // TerminalRead + TerminalWrite — was 1 (write only) before this slice

  test("system-interface.Terminal.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // Write/WriteLine take parameters; ReadLine doesn't — one exhaustive
    // match covers all three, same style as Random/Process/ExternalBackend.
    def ctorNameOf(r: TerminalEffect.Request): String = r match
      case TerminalEffect.Request.ReadLine       => "readLine"
      case TerminalEffect.Request.Write(_)       => "write"
      case TerminalEffect.Request.WriteLine(_)   => "writeLine"
    val scalaReqCases = Set(
      ctorNameOf(TerminalEffect.Request.ReadLine),
      ctorNameOf(TerminalEffect.Request.Write("")),
      ctorNameOf(TerminalEffect.Request.WriteLine("")))
    val fragmentReqCtors = EffectMeta.terminal.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.workspace composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.workspace", List(EffectMeta.workspace.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.workspace's requestActions is complete and matches packDecls actions (no drift, 5-to-1)"):
    assertEquals(EffectMeta.completeness(EffectMeta.workspace), Nil)
    val derived = EffectMeta.actions(EffectMeta.workspace)
    val fromPack = EffectMeta.packDecls("effect-workspace").actions.map(a =>
      EffectMeta.workspace.actionKey(a)).toSet
    assertEquals(derived, fromPack)
    assertEquals(derived.size, 1) // all 5 requests share WorkspaceRead
    assertEquals(EffectMeta.workspace.resource.kind, "workspace")
    assertEquals(EffectMeta.workspace.resource.pathPattern, "Path")

  test("system-interface.Workspace.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // LanguageDirs is parameterless; the other four take a Filesystem.Path.
    def ctorNameOf(r: WorkspaceEffect.Request): String = r match
      case WorkspaceEffect.Request.LanguageDirs               => "languageDirs"
      case WorkspaceEffect.Request.ListCairnFiles(_)          => "listCairnFiles"
      case WorkspaceEffect.Request.ListSubdirs(_)             => "listSubdirs"
      case WorkspaceEffect.Request.ListSurfaceCairnFiles(_)   => "listSurfaceCairnFiles"
      case WorkspaceEffect.Request.ReadText(_)                => "readText"
    val dummyPath = cairn.systeminterface.Filesystem.Path("")
    val scalaReqCases = Set(
      ctorNameOf(WorkspaceEffect.Request.LanguageDirs),
      ctorNameOf(WorkspaceEffect.Request.ListCairnFiles(dummyPath)),
      ctorNameOf(WorkspaceEffect.Request.ListSubdirs(dummyPath)),
      ctorNameOf(WorkspaceEffect.Request.ListSurfaceCairnFiles(dummyPath)),
      ctorNameOf(WorkspaceEffect.Request.ReadText(dummyPath)))
    val fragmentReqCtors = EffectMeta.workspace.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.filesystem composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.filesystem", List(EffectMeta.filesystem.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.filesystem's requestActions is complete and matches packDecls actions (no new action, 13-to-3)"):
    assertEquals(EffectMeta.completeness(EffectMeta.filesystem), Nil)
    val derived = EffectMeta.actions(EffectMeta.filesystem)
    val fromPack = EffectMeta.packDecls("effect-filesystem").actions.map(a =>
      EffectMeta.filesystem.actionKey(a)).toSet
    assertEquals(derived, fromPack)
    assertEquals(derived.size, 3) // FsRead, FsWrite, FsMkdirs — unchanged from before this slice
    assertEquals(EffectMeta.filesystem.resource.kind, "filesystem")
    assertEquals(EffectMeta.filesystem.resource.pathPattern, "Path")

  test("EffectMeta.filesystem's resolve constructor explicitly requires no right"):
    // The mechanism's new case: an entry may be None (no authorization
    // needed), distinct from a missing entry (which completeness flags).
    assertEquals(EffectMeta.filesystem.requestActions("resolve"), None)
    assertEquals(EffectMeta.filesystem.keyFor("resolve"), None)

  test("system-interface.Filesystem.Request cases correspond 1:1 to the Fragment's Request constructors"):
    def ctorNameOf(r: FsEffect.Request): String = r match
      case FsEffect.Request.Read(_)                => "read"
      case FsEffect.Request.ReadBytes(_)           => "readBytes"
      case FsEffect.Request.Write(_, _)             => "write"
      case FsEffect.Request.WriteBytes(_, _)        => "writeBytes"
      case FsEffect.Request.Mkdirs(_)                => "mkdirs"
      case FsEffect.Request.Exists(_)                => "exists"
      case FsEffect.Request.IsDirectory(_)           => "isDirectory"
      case FsEffect.Request.IsRegularFile(_)         => "isRegularFile"
      case FsEffect.Request.IsExecutable(_)          => "isExecutable"
      case FsEffect.Request.List(_)                  => "list"
      case FsEffect.Request.Delete(_)                => "delete"
      case FsEffect.Request.CreateTempDirectory(_)   => "createTempDirectory"
      case FsEffect.Request.Resolve(_, _)            => "resolve"
    val p = FsEffect.Path("")
    val scalaReqCases = Set(
      ctorNameOf(FsEffect.Request.Read(p)),
      ctorNameOf(FsEffect.Request.ReadBytes(p)),
      ctorNameOf(FsEffect.Request.Write(p, "")),
      ctorNameOf(FsEffect.Request.WriteBytes(p, Array.empty)),
      ctorNameOf(FsEffect.Request.Mkdirs(p)),
      ctorNameOf(FsEffect.Request.Exists(p)),
      ctorNameOf(FsEffect.Request.IsDirectory(p)),
      ctorNameOf(FsEffect.Request.IsRegularFile(p)),
      ctorNameOf(FsEffect.Request.IsExecutable(p)),
      ctorNameOf(FsEffect.Request.List(p)),
      ctorNameOf(FsEffect.Request.Delete(p)),
      ctorNameOf(FsEffect.Request.CreateTempDirectory("")),
      ctorNameOf(FsEffect.Request.Resolve(p, p)))
    val fragmentReqCtors = EffectMeta.filesystem.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.lsp composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.lsp", List(EffectMeta.lsp.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.lsp's requestActions is complete and matches packDecls actions (LspServe orphan replaced)"):
    assertEquals(EffectMeta.completeness(EffectMeta.lsp), Nil)
    val derived = EffectMeta.actions(EffectMeta.lsp)
    val fromPack = EffectMeta.packDecls("effect-lsp").actions.map(a =>
      EffectMeta.lsp.actionKey(a)).toSet
    assertEquals(derived, fromPack)
    assertEquals(derived.size, 2) // LspRead + LspWrite — replaces the orphaned LspServe

  test("system-interface.Lsp.Request cases correspond 1:1 to the Fragment's Request constructors"):
    // ReadMessage is parameterless; WriteMessage takes a payload.
    def ctorNameOf(r: LspEffect.Request): String = r match
      case LspEffect.Request.ReadMessage      => "readMessage"
      case LspEffect.Request.WriteMessage(_)  => "writeMessage"
    val scalaReqCases = Set(
      ctorNameOf(LspEffect.Request.ReadMessage),
      ctorNameOf(LspEffect.Request.WriteMessage("")))
    val fragmentReqCtors = EffectMeta.lsp.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.cas composes; Request cases correspond 1:1"):
    Compose.compose("effect.cas", List(EffectMeta.cas.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)
    assertEquals(EffectMeta.completeness(EffectMeta.cas), Nil)
    def ctorNameOf(r: CasEffect.Request): String = r match
      case CasEffect.Request.Put(_)       => "put"
      case CasEffect.Request.Get(_)       => "get"
      case CasEffect.Request.Contains(_)  => "contains"
      case CasEffect.Request.Fsck(_)      => "fsck"
      case CasEffect.Request.Gc(_, _)     => "gc"
      case CasEffect.Request.Stats(_)     => "stats"
    val dummy = Artifact(ArtifactKind.Term, Canon.CStr("x"))
    val scalaReqCases = Set(
      ctorNameOf(CasEffect.Request.Put(dummy)),
      ctorNameOf(CasEffect.Request.Get(Digest.ofBytes(Array.empty))),
      ctorNameOf(CasEffect.Request.Contains(Digest.ofBytes(Array.empty))),
      ctorNameOf(CasEffect.Request.Fsck("/tmp")),
      ctorNameOf(CasEffect.Request.Gc("/tmp", Nil)),
      ctorNameOf(CasEffect.Request.Stats("/tmp")))
    val fragmentReqCtors = EffectMeta.cas.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("EffectMeta.ledgerTransport composes; Request cases correspond 1:1"):
    Compose.compose("effect.ledgerTransport", List(EffectMeta.ledgerTransport.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)
    assertEquals(EffectMeta.completeness(EffectMeta.ledgerTransport), Nil)
    def ctorNameOf(r: LedgerEffect.Request): String = r match
      case LedgerEffect.Request.Append(_, _, _) => "append"
    val scalaReqCases = Set(
      ctorNameOf(LedgerEffect.Request.Append("alice", Map.empty, Nil)))
    val fragmentReqCtors =
      EffectMeta.ledgerTransport.fragment.constructors.filter(_.sort == "Request").map(_.name).toSet
    assertEquals(scalaReqCases, fragmentReqCtors)

  test("derived ActionKeys come from packDecls; no hand-maintained Action enum; Cas/Ledger Meta-defined"):
    val derived = EffectMeta.derivedActionKeys
    assertEquals(EffectMeta.allActionKeys, derived)
    // Unbound packDecl keys share family.name with digest-bound derived keys
    assertEquals(
      derived.map(k => (k.family, k.name)),
      EffectMeta.packDeclActionKeys.map(k => (k.family, k.name)))
    assert(Effects.Family.idsMatchPackDecls)
    assert(EffectMeta.families.contains(Effects.Family.Cas))
    assert(EffectMeta.families.contains(Effects.Family.LedgerTransport))
    EffectMeta.families.values.foreach { f =>
      assertEquals(EffectMeta.completeness(f), Nil, clues(f.fragment.name))
      f.actionKeys.foreach { k =>
        assert(k.interfaceDigest.contains(f.fragment.digest), clues(k.id))
      }
      assertEquals(f.resource.interfaceDigest, Some(f.fragment.digest))
    }

  test("CAS-pinned effect interface: round-trip + ActionKey.fromPinned; forgery rejected"):
    val art = EffectMeta.interfaceArtifact(EffectMeta.filesystem)
    val pinned = EffectMeta.PinnedInterface.fromArtifact(art).fold(e => fail(e), identity)
    assertEquals(pinned.pinDigest, art.digest)
    val key = Effects.ActionKey.fromPinned(pinned, "read").fold(e => fail(e), identity)
    assertEquals(key, EffectMeta.filesystem.actionKey("read"))
    assert(Effects.ActionKey.fromPinned(pinned, "nope").isLeft)
    // Tampered envelope body with wrong claimed digest
    val tampered = Artifact(ArtifactKind.Fragment, Canon.CTag("effect-interface",
      Canon.cmap("family" -> Canon.CStr("Filesystem"))))
    assert(EffectMeta.PinnedInterface.fromArtifact(tampered).isLeft)
    // Host pin path
    val hostPin = EffectMeta.pinHost(EffectMeta.clock)
    assertEquals(
      Effects.ActionKey.fromPinned(hostPin, "now").toOption.get,
      EffectMeta.clock.actionKey("now"))

  test("InterfaceDecl packDecls cover all packs; Family.forPack thin bridge"):
    assertEquals(EffectMeta.packDecls.keySet, EffectMeta.fragmentPackNames.toSet)
    assertEquals(EffectMeta.packFamily.keySet, EffectMeta.fragmentPackNames.toSet)
    for name <- EffectMeta.fragmentPackNames do
      val d = EffectMeta.packDecls(name)
      assertEquals(Effects.Family.fromId(d.familyId), Effects.Family.forPack(name))
      assertEquals(EffectMeta.InterfaceDecl.fromFamily(EffectMeta.families(Effects.Family.forPack(name).get)), d)

  test("InterfaceDecl.fromItems round-trips filesystem gating (incl. ungated resolve)"):
    val items = List(
      Cst.node("family", Cst.Leaf("Filesystem")),
      Cst.node("kind", Cst.Leaf("filesystem")),
      Cst.node("path", Cst.Leaf("Path")),
      Cst.node("action", Cst.Leaf("read")),
      Cst.node("action", Cst.Leaf("write")),
      Cst.node("action", Cst.Leaf("mkdirs")),
      Cst.node("gate", Cst.Leaf("read"), Cst.Leaf("read")),
      Cst.node("gate", Cst.Leaf("write"), Cst.Leaf("write")),
      Cst.node("gate", Cst.Leaf("mkdirs"), Cst.Leaf("mkdirs")),
      Cst.node("ungated", Cst.Leaf("resolve")))
    // Minimal items — full map is on disk; this checks ungated decoding.
    val decl = EffectMeta.InterfaceDecl.fromItems(items).fold(e => fail(e), identity)
    assertEquals(decl.requestActions.get("resolve"), Some(None))
    assertEquals(decl.familyId, "Filesystem")

  test("PathPattern: all families declare a known pattern; Cas is Digest|Path"):
    import EffectMeta.PathPattern
    assertEquals(PathPattern.known.map(_.encode).toSet,
      Set("*", "Path", "Command", "Host", "Digest", "Digest|Path"))
    assertEquals(EffectMeta.cas.resource.pathPattern, "Digest|Path")
    assertEquals(EffectMeta.cas.resource.pattern, Right(PathPattern.DigestOrPath))
    assertEquals(EffectMeta.filesystem.resource.pattern, Right(PathPattern.FsPath))
    assertEquals(EffectMeta.clock.resource.pattern, Right(PathPattern.Unscoped))
    // Concrete shape checks
    val dig = Digest.ofBytes(Array(1, 2, 3)).hex
    assert(EffectMeta.cas.resource.atChecked(dig).isRight)
    assert(EffectMeta.cas.resource.atChecked("/tmp/cas").isRight)
    assert(EffectMeta.cas.resource.atChecked("*").isRight)
    assert(EffectMeta.filesystem.resource.atChecked("").isLeft)
    assert(EffectMeta.InterfaceDecl.fromItems(List(
      Cst.node("family", Cst.Leaf("Cas")),
      Cst.node("kind", Cst.Leaf("cas")),
      Cst.node("path", Cst.Leaf("Nope")),
      Cst.node("action", Cst.Leaf("put")))).isLeft)

