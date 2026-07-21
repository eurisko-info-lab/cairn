package cairn.tests

import cairn.kernel.*
import cairn.systeminterface.Random as RandomEffect
import cairn.systeminterface.Clock as ClockEffect
import cairn.systeminterface.Process as ProcessEffect
import cairn.systeminterface.ExternalBackend as BackendEffect
import cairn.systeminterface.Terminal as TerminalEffect
import cairn.systeminterface.Workspace as WorkspaceEffect
import cairn.systeminterface.Filesystem as FsEffect

/** Meta-defined effect interfaces (post-migration priority #1): `Random`,
  * `Clock`, `Process`, `ExternalBackend`, `Terminal`, `Workspace`, and
  * `Filesystem` are Fragment-defined families, using EffectMeta's
  * many-to-one requestActions grouping (`Terminal`'s `write`/`writeLine`
  * share one right; `Workspace`'s 5 requests all share one; `Filesystem`'s
  * 12 reduce to 3, with `resolve` needing none at all). These are
  * mechanical drift guards, same spirit as `ModuleBoundarySuite`.
  */
class EffectMetaSuite extends munit.FunSuite:

  test("EffectMeta.random composes and lints cleanly as a Kernel Fragment"):
    Compose.compose("effect.random", List(EffectMeta.random.fragment)).fold(
      errs => fail(errs.map(_.render).mkString("\n")), identity)

  test("EffectMeta.random's requestActions is complete and matches the hand-tagged Random actions exactly"):
    assertEquals(EffectMeta.completeness(EffectMeta.random, Effects.Family.Random), Nil)
    val derived = EffectMeta.actions(EffectMeta.random)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Random).toSet
    assertEquals(derived, handTagged)

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

  test("EffectMeta.clock's requestActions is complete and matches the hand-tagged actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.clock, Effects.Family.Clock), Nil)
    val derived = EffectMeta.actions(EffectMeta.clock)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Clock).toSet
    assertEquals(derived, handTagged)
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

  test("EffectMeta.process's requestActions is complete and matches the hand-tagged actions (no drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.process, Effects.Family.Process), Nil)
    val derived = EffectMeta.actions(EffectMeta.process)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Process).toSet
    assertEquals(derived, handTagged)
    assertEquals(derived.size, 1) // Run — matched cleanly, unlike Clock

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

  test("EffectMeta.externalBackend's requestActions is complete and matches the hand-tagged actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.externalBackend, Effects.Family.ExternalBackend), Nil)
    val derived = EffectMeta.actions(EffectMeta.externalBackend)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.ExternalBackend).toSet
    assertEquals(derived, handTagged)
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

  test("EffectMeta.terminal's requestActions is complete and matches the hand-tagged actions (incl. the fixed drift)"):
    assertEquals(EffectMeta.completeness(EffectMeta.terminal, Effects.Family.Terminal), Nil)
    val derived = EffectMeta.actions(EffectMeta.terminal)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Terminal).toSet
    assertEquals(derived, handTagged)
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

  test("EffectMeta.workspace's requestActions is complete and matches the hand-tagged actions (no drift, 5-to-1)"):
    assertEquals(EffectMeta.completeness(EffectMeta.workspace, Effects.Family.Workspace), Nil)
    val derived = EffectMeta.actions(EffectMeta.workspace)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Workspace).toSet
    assertEquals(derived, handTagged)
    assertEquals(derived.size, 1) // all 5 requests share WorkspaceRead

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

  test("EffectMeta.filesystem's requestActions is complete and matches the hand-tagged actions (no new action, 12-to-3)"):
    assertEquals(EffectMeta.completeness(EffectMeta.filesystem, Effects.Family.Filesystem), Nil)
    val derived = EffectMeta.actions(EffectMeta.filesystem)
    val handTagged = Effects.Action.values.filter(_.family == Effects.Family.Filesystem).toSet
    assertEquals(derived, handTagged)
    assertEquals(derived.size, 3) // FsRead, FsWrite, FsMkdirs — unchanged from before this slice

  test("EffectMeta.filesystem's resolve constructor explicitly requires no right"):
    // The mechanism's new case: an entry may be None (no authorization
    // needed), distinct from a missing entry (which completeness flags).
    assertEquals(EffectMeta.filesystem.requestActions("resolve"), None)

  test("system-interface.Filesystem.Request cases correspond 1:1 to the Fragment's Request constructors"):
    def ctorNameOf(r: FsEffect.Request): String = r match
      case FsEffect.Request.Read(_)                => "read"
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
