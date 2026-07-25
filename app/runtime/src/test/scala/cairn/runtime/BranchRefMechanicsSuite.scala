package cairn.runtime

import cairn.kernel.*
import cairn.core.Module
import cairn.systemhandler.{DiskCas, MemCas, CasEffects}

/** Raw branch-ref/manifest mechanics — deliberately package-scoped
  * (`advanceRaw` is `private[runtime]`) since these tests exercise the
  * low-level ref-write primitive itself (effect-context gating, append-only
  * history, domain-ancestry field preservation across a raw advance), not
  * the sealed semantic-acceptance surface (`commitTip` / `merge` /
  * `mergeBranches`). Moved out of `cairn.tests` for exactly that reason:
  * they were the only external callers keeping `advance` public, but what
  * they actually test is the ref/manifest primitive, not ΔL acceptance.
  */
class BranchRefMechanicsSuite extends munit.FunSuite:
  private val casCtx = EffectContexts.forBranches()

  test("Branches refs FS: gated under forBranches; denied under forCas-only"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-refs-auth")
    val cas = MemCas()
    val art = Artifact(ArtifactKind.Term, Canon.CStr("branch-seed"))
    val key = CasEffects.put(cas, art, EffectContexts.forCas()).fold(e => fail(e.toString), identity)
    val denied = Branches(cas, dir.resolve("refs"), EffectContexts.forCas())
    intercept[RuntimeException](denied.advanceRaw("main", key))
    val ok = Branches(cas, dir.resolve("refs"), EffectContexts.forBranches())
    ok.advanceRaw("main", key)
    assertEquals(ok.load("main").head, Some(key))
    assertEquals(ok.list(), List("main"))

  test("branch manifests: append-only history surviving restart (S18)"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-branches")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val k1 = CasEffects.put(cas, Module(List("x" -> Cst.Leaf("1"))).artifact, casCtx).fold(e => fail(e.toString), identity)
    val k2 = CasEffects.put(cas, Module(List("x" -> Cst.Leaf("2"))).artifact, casCtx).fold(e => fail(e.toString), identity)
    branches.advanceRaw("main", k1)
    branches.advanceRaw("main", k2)
    // fresh instances = process restart
    val branches2 = Branches(DiskCas(dir), dir.resolve("refs"), casCtx)
    val m = branches2.load("main")
    assertEquals(m.head, Some(k2))
    assertEquals(m.history, List(k1))
    assertEquals(branches2.list(), List("main"))

  test("domain trunk: LAW off ledger; SDS primary=LAW + refer CHEMISTRY"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-domain")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val mLaw = Module(List("law" -> Cst.Leaf("true")))
    val mChem = Module(List("chem" -> Cst.Leaf("false")))
    val mSds = Module(List("sds" -> Cst.Leaf("true")))
    val law = branches.forkFrom("LAW", primary = None, module = Some(mLaw))
      .fold(e => fail(e), identity)
    assertEquals(law.primaryAncestor, None)
    assertEquals(law.references, Nil)
    val chem = branches.forkFrom("CHEMISTRY", primary = None, module = Some(mChem))
      .fold(e => fail(e), identity)
    assertEquals(chem.primaryAncestor, None)
    val sds = branches.forkFrom(
        "SDS", primary = Some("LAW"), module = Some(mSds), references = List("CHEMISTRY"))
      .fold(e => fail(e), identity)
    assertEquals(sds.primaryAncestor, Some("LAW"))
    assertEquals(sds.references, List("CHEMISTRY"))
    // advanceRaw preserves domain ancestry
    val k = CasEffects.put(cas, Module(List("sds2" -> Cst.Leaf("false"))).artifact, casCtx)
      .fold(e => fail(e.toString), identity)
    val advanced = branches.advanceRaw("SDS", k)
    assertEquals(advanced.primaryAncestor, Some("LAW"))
    assertEquals(advanced.references, List("CHEMISTRY"))
    // soft ref can be added later
    val onlyLaw = branches.forkFrom("TAX", primary = Some("LAW")).fold(e => fail(e), identity)
    assertEquals(onlyLaw.references, Nil)
    val withChem = branches.referTo("TAX", "CHEMISTRY").fold(e => fail(e), identity)
    assertEquals(withChem.primaryAncestor, Some("LAW"))
    assertEquals(withChem.references, List("CHEMISTRY"))
