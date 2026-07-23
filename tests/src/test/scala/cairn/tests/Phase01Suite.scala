package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{MemCas, DiskCas, Branches, CasEffects, EffectContext, Ed25519, Keypair}
import cairn.core.TreeEngine
import cairn.examples.stlc.Stlc

/** Phase 0 acceptance (S2–S6): store/load round-trip, golden digests.
  *
  * Low-level MemCas/DiskCas put/get here are intentional **trait contract**
  * tests (no authority surface). Composition and branch seeds use [[CasEffects]].
  */
class Phase0Suite extends munit.FunSuite:
  test("CAS put/get round-trip, digest verified (S4)"):
    // Intentional direct MemCas trait contract (no EffectContext).
    val cas = MemCas()
    val a = Stlc.base.artifact
    val key = cas.put(a)
    assertEquals(cas.get(key), Right(a))

  test("disk CAS detects corruption (S4)"):
    // Intentional direct DiskCas trait contract (corruption check on read).
    val dir = java.nio.file.Files.createTempDirectory("cairn-cas")
    val cas = DiskCas(dir)
    val d = cas.putBytes("hello cairn".getBytes)
    // corrupt the object file
    val p = dir.resolve("objects").resolve(d.hex.take(2)).resolve(d.hex.drop(2))
    java.nio.file.Files.write(p, "tampered!!!".getBytes)
    assert(cas.getBytes(d).swap.exists(_.contains("corruption")))

  test("fragment artifacts round-trip through CAS (S7)"):
    // Intentional direct MemCas trait contract for fragment codec bytes.
    val cas = MemCas()
    for f <- Stlc.fragments do
      val key = cas.put(f.artifact)
      val back = cas.get(key).map(a => FragmentCodec.fromCanon(a.body))
      assertEquals(back, Right(f))

  test("grammar spec is a canonical artifact (S8)"):
    val g = Stlc.language.grammar
    val c = GrammarSpec.toCanon(g)
    assertEquals(GrammarSpec.fromCanon(c), g)
    assertEquals(Digest.of(c), Digest.of(GrammarSpec.toCanon(GrammarSpec.fromCanon(c))))

/** Phase 1 acceptance (S9–S18). */
class Phase1Suite extends munit.FunSuite:
  val lang = Stlc.language
  val g = lang.grammar

  test("composition is order-independent (S12/S13)"):
    val l1 = Compose.compose("stlc", Stlc.boundFragments).toOption.get
    val l2 = Compose.compose("stlc", Stlc.boundFragments.reverse).toOption.get
    assertEquals(l1.digest, l2.digest)
    assertEquals(l1.grammar, l2.grammar)

  test("composition conflict is a structured error (S12)"):
    val evil = Stlc.base.copy(name = "evil",
      constructors = List(CtorDef("var", "Term", List("Name", "Name"))))
    Compose.compose("bad", List(Stlc.base, evil)) match
      case Left(errs) =>
        assert(errs.exists(e => e.path == "constructors/var"), errs.map(_.render).mkString("\n"))
        assert(errs.head.render.contains("evil"))
      case Right(_) => fail("conflict not detected")

  test("missing requires is a structured error (S12)"):
    Compose.compose("bad", List(Stlc.lambda)) match
      case Left(errs) => assert(errs.exists(_.path.startsWith("requires/")))
      case Right(_)   => fail("unsatisfied requires not detected")

  test("left-recursion checker accepts stlc grammar (S10)"):
    assertEquals(Parser.leftRecursionCheck(g), Right(()))

  test("left recursion rejected statically (S10)"):
    val bad = GrammarSpec("bad", TokenSpec(Nil, List("+"), None),
      List(CategorySpec("e", List(ConstructorSpec("plus", List(Elem.Cat("e"), Elem.Tok("+"), Elem.Cat("e")))))),
      Nil, Nil, "e")
    assert(Parser.leftRecursionCheck(bad).swap.exists(_.contains("left recursion")))

  test("parse golden terms (S10)"):
    assertEquals(Parser.parse(g, "true"), Right(Stlc.tru))
    assertEquals(Parser.parse(g, "fun x : Bool . x"), Right(Stlc.idBool))
    assertEquals(Parser.parse(g, "((fun x : Bool . x) true)"),
      Right(Stlc.app1(Stlc.idBool, Stlc.tru)))
    assertEquals(Parser.parse(g, "fun f : Bool -> Bool -> Bool . (f true)"),
      Right(Stlc.lam1("f", Stlc.arrow1(Stlc.tBool, Stlc.arrow1(Stlc.tBool, Stlc.tBool)),
        Stlc.app1(Stlc.v("f"), Stlc.tru))))

  test("arrow associativity + parens (S10/S11)"):
    val t = Stlc.arrow1(Stlc.arrow1(Stlc.tBool, Stlc.tBool), Stlc.tBool)
    val printed = Printer.print(g, Stlc.lam1("f", t, Stlc.v("f"))).toOption.get
    assert(printed.contains("(Bool -> Bool) -> Bool"), printed)
    assertEquals(Parser.parse(g, printed), Right(Stlc.lam1("f", t, Stlc.v("f"))))

  val goldenTerms = List(
    Stlc.tru, Stlc.fls, Stlc.idBool, Stlc.churchTrue, Stlc.churchFalse,
    Stlc.app1(Stlc.idBool, Stlc.tru),
    Stlc.node3if,
    Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls),
    Stlc.lam1("f", Stlc.arrow1(Stlc.tBool, Stlc.tBool), Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.tru))))

  test("round-trip law: parse(print(t)) == t on golden suite (S11, §4.4)"):
    for t <- goldenTerms do
      RoundTrip.check(g, t) match
        case Right(())  => ()
        case Left(err)  => fail(err)

  test("evaluation: identity and if (S15/S16)"):
    assertEquals(TreeEngine.normalize(lang, Stlc.app1(Stlc.idBool, Stlc.tru)), Right(Stlc.tru))
    assertEquals(TreeEngine.normalize(lang, Stlc.node3if), Right(Stlc.fls))

  test("Church booleans evaluate (S16 acceptance)"):
    // churchTrue a b --> a ; churchFalse a b --> b
    assertEquals(TreeEngine.normalize(lang,
      Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls)), Right(Stlc.tru))
    assertEquals(TreeEngine.normalize(lang,
      Stlc.app1(Stlc.app1(Stlc.churchFalse, Stlc.tru), Stlc.fls)), Right(Stlc.fls))

  test("engine is generic: no rule => stuck term unchanged (S15)"):
    val stuck = Stlc.v("free")
    assertEquals(TreeEngine.normalize(lang, stuck), Right(stuck))

class DeltaSuite extends munit.FunSuite:
  val lang = Stlc.language

  test("ΔL is a real language and parses change syntax (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    assertEquals(dl.name, "Δstlc")
    val src = "{ add id = fun x : Bool . x ; }"
    val parsed = Parser.parse(dl.grammar, src)
    assert(parsed.isRight, parsed.toString)

  test("ΔL edit produces new digest; validated change-set recorded (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val m0 = Module(Nil)
    val change = Parser.parse(dl.grammar, "{ add id = fun x : Bool . x ; add both = (id true) ; }").toOption.get
    val Right((m1, vcs)) = Delta.apply(lang, m0, change): @unchecked
    assert(m1.digest != m0.digest)
    assertEquals(vcs.base, m0.digest)
    assertEquals(vcs.result, m1.digest)
    assertEquals(m1.get("id"), Some(Stlc.idBool))

  test("invalid ΔL edit rejected (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val change = Parser.parse(dl.grammar, "{ replace missing = true ; }").toOption.get
    assert(Delta.apply(lang, Module(Nil), change).swap.exists(_.contains("not defined")))

  test("rename requires exact footprint (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val m = Module(List(
      "id" -> Stlc.idBool,
      "use" -> Stlc.app1(Stlc.v("id"), Stlc.tru)))
    val wrong = Parser.parse(dl.grammar, "{ rename id to ident footprint [] ; }").toOption.get
    assert(Delta.apply(lang, m, wrong).swap.exists(_.contains("footprint mismatch")))
    val right = Parser.parse(dl.grammar, "{ rename id to ident footprint [use] ; }").toOption.get
    val Right((m2, _)) = Delta.apply(lang, m, right): @unchecked
    assertEquals(m2.get("ident"), Some(Stlc.idBool))
    assertEquals(m2.get("use"), Some(Stlc.app1(Stlc.v("ident"), Stlc.tru)))

  test("Δ(ΔL) exists — forced recursive closure (S17, §2b)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val ddl = Delta.deltaOf(dl).toOption.get
    assertEquals(ddl.name, "ΔΔstlc")
    assert(ddl.grammar.top == "ΔΔstlc.changeset")
    // and Δ(Δ(ΔL)) for good measure
    assert(Delta.deltaOf(ddl).isRight)

  test("ΔL round-trips its own surface (S17 + S11 law)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val src = "{ add id = fun x : Bool . x ; remove old ; rename a to b footprint [x, y] ; }"
    val t = Parser.parse(dl.grammar, src).toOption.get
    RoundTrip.check(dl.grammar, t).fold(e => fail(e), identity)

class BranchSuite extends munit.FunSuite:
  private val casCtx = EffectContext.forBranches()

  /** Put a sealed primary-owner delegation via [[Branches.mintAncestorDelegation]]. */
  private def mintDelegation(
      branches: Branches,
      claim: DomainAgreement,
      grantor: Keypair,
      idents: IdentityResolver,
  ): (DomainAgreement, (String, Vector[Byte])) =
    val del = DomainAncestorDelegation(
      ancestor = claim.primaryAncestor.get,
      child = claim.child,
      grantor = grantor.name,
      grantee = claim.owner,
      claimDigest = claim.claimDigest)
    val seal = (grantor.name, grantor.sign(Canon.encode(del.canon)))
    branches.mintAncestorDelegation(claim, seal, idents).fold(e => fail(e), identity)

  private def ownerSealOf(kp: Keypair, ag: DomainAgreement): (String, Vector[Byte]) =
    (kp.name, kp.sign(Canon.encode(ag.canon)))

  private def deps(ds: Digest*): Canon = DomainAgreement.dependencyEvidenceOf(ds.toList)

  private val LangIdx: Set[Digest] = Set(
    Digest.of(Canon.CStr("root")),
    Digest.of(Canon.CStr("cert")),
    Digest.of(Canon.CStr("law-lang")),
    Digest.of(Canon.CStr("sds-lang")),
    Digest.of(Canon.CStr("chem-lang")),
    Digest.of(Canon.CStr("known-lang")),
    Digest.of(Canon.CStr("unknown-lang")))

  private def idsOf(kps: Keypair*): IdentityResolver =
    IdentityResolver.bootstrapOnly(kps.map(k => k.name -> k.publicBytes).toMap)

  test("branch manifests: append-only history surviving restart (S18)"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-branches")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val k1 = CasEffects.put(cas, Stlc.base.artifact, casCtx).fold(e => fail(e.toString), identity)
    val k2 = CasEffects.put(cas, Stlc.types.artifact, casCtx).fold(e => fail(e.toString), identity)
    branches.advance("main", k1)
    branches.advance("main", k2)
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
    val mLaw = Module(List("law" -> Stlc.tru))
    val mChem = Module(List("chem" -> Stlc.fls))
    val mSds = Module(List("sds" -> Stlc.tru))
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
    // advance preserves domain ancestry
    val k = CasEffects.put(cas, Module(List("sds2" -> Stlc.fls)).artifact, casCtx)
      .fold(e => fail(e.toString), identity)
    val advanced = branches.advance("SDS", k)
    assertEquals(advanced.primaryAncestor, Some("LAW"))
    assertEquals(advanced.references, List("CHEMISTRY"))
    // soft ref can be added later
    val onlyLaw = branches.forkFrom("TAX", primary = Some("LAW")).fold(e => fail(e), identity)
    assertEquals(onlyLaw.references, Nil)
    val withChem = branches.referTo("TAX", "CHEMISTRY").fold(e => fail(e), identity)
    assertEquals(withChem.primaryAncestor, Some("LAW"))
    assertEquals(withChem.references, List("CHEMISTRY"))

  test("forkFrom rejects self-ref / primary∩refs / duplicate refs (no silent normalize)"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-fork-strict")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    branches.forkFrom("LAW", primary = None).fold(e => fail(e), identity)
    assert(branches.forkFrom("SDS", primary = Some("LAW"), references = List("SDS")).isLeft)
    assert(branches.forkFrom("SDS", primary = Some("LAW"), references = List("LAW")).isLeft)
    assert(branches.forkFrom(
      "SDS", primary = Some("LAW"), references = List("CHEMISTRY", "CHEMISTRY")).isLeft)
    assert(branches.forkFrom("CHEMISTRY", primary = None).isRight)
    assert(branches.forkFrom(
      "SDS", primary = Some("LAW"), references = List("CHEMISTRY")).isRight)

  test("DomainAgreement plantGoverned records owner + language evidence; rejects bad replaces"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-domain-agree")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val lawOwner = Keypair.dev("law-owner")
    val alice = Keypair.dev("alice")
    val idents = idsOf(lawOwner, alice)
    val lawAg = DomainAgreement(
      child = "LAW",
      primaryAncestor = None,
      references = Nil,
      owner = lawOwner.name,
      childLanguage = Some(Digest.of(Canon.CStr("law-lang"))),
      ancestorLanguages = Nil,
      dependencyEvidence = deps(Digest.of(Canon.CStr("root"))),
      replaces = None)
    branches.plantGoverned(lawAg, idents, ownerSealOf(lawOwner, lawAg), LangIdx).fold(e => fail(e), identity)
    val lawLang = Digest.of(Canon.CStr("law-lang"))
    val sdsLang = Digest.of(Canon.CStr("sds-lang"))
    val claim0 = DomainAgreement(
      child = "SDS",
      primaryAncestor = Some("LAW"),
      references = Nil,
      owner = alice.name,
      childLanguage = Some(sdsLang),
      ancestorLanguages = List("LAW" -> lawLang),
      dependencyEvidence = deps(Digest.of(Canon.CStr("cert"))),
      replaces = None)
    val (a0, gSeal0) = mintDelegation(branches, claim0, lawOwner, idents)
    val planted = branches.plantGoverned(
      a0, idents, ownerSealOf(alice, a0), LangIdx, grantorSeal = Some(gSeal0)).fold(e => fail(e), identity)
    assertEquals(planted.primaryAncestor, Some("LAW"))
    assert(planted.domainAgreement.isDefined, planted.domainAgreement)
    val liveDig0 = planted.domainAgreement.get
    // Manifest cites the sealed envelope, not the unsigned body digest.
    assert(liveDig0 != a0.digest)
    val sealedLive = CasEffects.get(cas, liveDig0, casCtx).flatMap { art =>
      SealedDomainAgreement.fromCanon(art.body)
    }.fold(e => fail(e.toString), identity)
    assertEquals(sealedLive.agreement.digest, a0.digest)
    assert(sealedLive.ownerSeal.nonEmpty)
    assert(sealedLive.grantorSeal.exists(_.nonEmpty))
    SealedDomainAgreement.verify(sealedLive, idents, Ed25519.verify, Some(lawOwner.name))
      .fold(e => fail(e), identity)
    assert(planted.certificates.contains(a0.ancestorDelegation.get))
    // Seal under a different key for the grantor name fails against the resolver
    val attacker = Keypair.dev("attacker")
    val forged = (
      lawOwner.name,
      Ed25519.sign(attacker.privateKey, Canon.encode(
        DomainAncestorDelegation("LAW", "SDS", lawOwner.name, alice.name, claim0.claimDigest).canon)))
    assert(branches.plantGoverned(
      a0, idents, ownerSealOf(alice, a0), LangIdx, grantorSeal = Some(forged)).isLeft)
    assert(branches.plantGoverned(claim0, idents, ownerSealOf(alice, claim0), LangIdx).isLeft)
    val chemAg = DomainAgreement(
      child = "CHEMISTRY",
      primaryAncestor = None,
      references = Nil,
      owner = lawOwner.name,
      childLanguage = Some(Digest.of(Canon.CStr("chem-lang"))),
      ancestorLanguages = Nil,
      dependencyEvidence = deps(Digest.of(Canon.CStr("root"))),
      replaces = None)
    branches.plantGoverned(chemAg, idents, ownerSealOf(lawOwner, chemAg), LangIdx).fold(e => fail(e), identity)
    val chemLang = Digest.of(Canon.CStr("chem-lang"))
    val noReplace = claim0.copy(
      references = List("CHEMISTRY"),
      ancestorLanguages = List("LAW" -> lawLang, "CHEMISTRY" -> chemLang),
      replaces = None)
    assert(branches.plantGoverned(noReplace, idents, ownerSealOf(alice, noReplace), LangIdx).isLeft)
    val claim1 = noReplace.copy(replaces = Some(liveDig0))
    val (a1, gSeal1) = mintDelegation(branches, claim1, lawOwner, idents)
    val amended = branches.plantGoverned(
      a1, idents, ownerSealOf(alice, a1), LangIdx, grantorSeal = Some(gSeal1)).fold(e => fail(e), identity)
    assertEquals(amended.references, List("CHEMISTRY"))
    assert(amended.domainAgreement.isDefined)
    assert(amended.domainAgreement.get != a1.digest)
    val badOwner = a1.copy(owner = "mallory", replaces = Some(amended.domainAgreement.get))
    assert(branches.plantGoverned(
      badOwner, idents, ("mallory", Vector.empty), LangIdx, grantorSeal = Some(gSeal1)).isLeft)

  test("DomainAgreement plantGoverned fails closed when live agreement is unloadable"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-domain-failclosed")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val lawOwner = Keypair.dev("law-owner")
    val alice = Keypair.dev("alice")
    val idents = idsOf(lawOwner, alice)
    val lawAg = DomainAgreement(
      child = "LAW",
      primaryAncestor = None,
      references = Nil,
      owner = lawOwner.name,
      childLanguage = Some(Digest.of(Canon.CStr("law-lang"))),
      ancestorLanguages = Nil,
      dependencyEvidence = deps(Digest.of(Canon.CStr("root"))),
      replaces = None)
    branches.plantGoverned(lawAg, idents, ownerSealOf(lawOwner, lawAg), LangIdx).fold(e => fail(e), identity)
    val lawLang = Digest.of(Canon.CStr("law-lang"))
    val sdsLang = Digest.of(Canon.CStr("sds-lang"))
    val claim0 = DomainAgreement(
      child = "SDS",
      primaryAncestor = Some("LAW"),
      references = Nil,
      owner = alice.name,
      childLanguage = Some(sdsLang),
      ancestorLanguages = List("LAW" -> lawLang),
      dependencyEvidence = deps(Digest.of(Canon.CStr("cert"))),
      replaces = None)
    val (a0, gSeal0) = mintDelegation(branches, claim0, lawOwner, idents)
    branches.plantGoverned(a0, idents, ownerSealOf(alice, a0), LangIdx, grantorSeal = Some(gSeal0))
      .fold(e => fail(e), identity)
    val liveDig0 = branches.load("SDS").domainAgreement.get
    val agr = liveDig0
    val obj = dir.resolve("objects").resolve(agr.hex.take(2)).resolve(agr.hex.drop(2))
    assert(java.nio.file.Files.exists(obj), s"expected CAS object at $obj")
    java.nio.file.Files.delete(obj)
    val chemAg = DomainAgreement(
      child = "CHEMISTRY",
      primaryAncestor = None,
      references = Nil,
      owner = lawOwner.name,
      childLanguage = Some(Digest.of(Canon.CStr("chem-lang"))),
      ancestorLanguages = Nil,
      dependencyEvidence = deps(Digest.of(Canon.CStr("root"))),
      replaces = None)
    branches.plantGoverned(chemAg, idents, ownerSealOf(lawOwner, chemAg), LangIdx).fold(e => fail(e), identity)
    val chemLang = Digest.of(Canon.CStr("chem-lang"))
    val claim1 = claim0.copy(
      references = List("CHEMISTRY"),
      ancestorLanguages = List("LAW" -> lawLang, "CHEMISTRY" -> chemLang),
      replaces = Some(liveDig0))
    val (a1, gSeal1) = mintDelegation(branches, claim1, lawOwner, idents)
    val err = branches.plantGoverned(a1, idents, ownerSealOf(alice, a1), LangIdx, grantorSeal = Some(gSeal1))
    assert(err.isLeft, err.toString)
    assert(err.swap.toOption.exists(_.contains("cannot load live")), err.toString)

  test("DomainAgreement rejects plant under ungoverned primary and bad delegation seals"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-domain-deleg")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    branches.forkFrom("LAW", primary = None).fold(e => fail(e), identity)
    val lawOwner = Keypair.dev("law-owner")
    val alice = Keypair.dev("alice")
    val idents = idsOf(lawOwner, alice)
    val claim = DomainAgreement(
      child = "SDS",
      primaryAncestor = Some("LAW"),
      references = Nil,
      owner = alice.name,
      childLanguage = Some(Digest.of(Canon.CStr("sds-lang"))),
      ancestorLanguages = List("LAW" -> Digest.of(Canon.CStr("law-lang"))),
      dependencyEvidence = deps(Digest.of(Canon.CStr("cert"))))
    val (a, seal) = mintDelegation(branches, claim, lawOwner, idents)
    assert(branches.plantGoverned(a, idents, ownerSealOf(alice, a), LangIdx, grantorSeal = Some(seal)).isLeft)
    val lawAg = DomainAgreement(
      child = "LAW",
      primaryAncestor = None,
      references = Nil,
      owner = lawOwner.name,
      childLanguage = Some(Digest.of(Canon.CStr("law-lang"))),
      ancestorLanguages = Nil,
      dependencyEvidence = deps(Digest.of(Canon.CStr("root"))))
    branches.plantGoverned(lawAg, idents, ownerSealOf(lawOwner, lawAg), LangIdx).fold(e => fail(e), identity)
    val mallory = Keypair.dev("mallory")
    val badSeal = (lawOwner.name, Ed25519.sign(mallory.privateKey, Canon.encode(
      DomainAncestorDelegation("LAW", "SDS", lawOwner.name, alice.name, claim.claimDigest).canon)))
    assert(branches.plantGoverned(a, idents, ownerSealOf(alice, a), LangIdx, grantorSeal = Some(badSeal)).isLeft)
    assert(branches.plantGoverned(a, idents, ownerSealOf(alice, a), LangIdx, grantorSeal = Some(seal)).isRight)

  test("DomainAgreement rejects unknown language digests when an index is supplied"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-domain-lang")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"), casCtx)
    val owner = Keypair.dev("owner")
    val idents = idsOf(owner)
    val known = Digest.of(Canon.CStr("known-lang"))
    val unknown = Digest.of(Canon.CStr("unknown-lang"))
    val ag = DomainAgreement(
      child = "LAW",
      primaryAncestor = None,
      references = Nil,
      owner = owner.name,
      childLanguage = Some(unknown),
      ancestorLanguages = Nil,
      dependencyEvidence = deps(known))
    assert(branches.plantGoverned(
      ag, idents, ownerSealOf(owner, ag), Set(known)).isLeft)
    val ok = ag.copy(childLanguage = Some(known), dependencyEvidence = deps(known))
    assert(branches.plantGoverned(
      ok, idents, ownerSealOf(owner, ok), Set(known)).isRight)
    // auditGoverned requires a non-empty PackAccess index; empty packs fail closed.
    val emptyPacks = new cairn.systeminterface.PackAccess:
      def requireOwn(name: String) = Nil
      def requireClosed(name: String, surface: String = "default") =
        throw RuntimeException("unused")
      def requireSurface(lang: String, surface: String = "default") =
        throw RuntimeException("unused")
      def surfacesFor(lang: String) = Map.empty
      def loadRaw() = Map.empty
      def loadClosed() = Map.empty
      def unmetRequires(name: String, packs: Map[String, List[Fragment]]) = Set.empty
    assert(branches.auditGoverned("LAW", idents, emptyPacks).isLeft)

  test("DomainBranch.wellFormed rejects unknown / self / primary∩refs"):
    val known = Set("LAW", "CHEMISTRY")
    assert(DomainBranch.wellFormed(
      BranchManifest("SDS", None, Nil, primaryAncestor = Some("LAW"), references = List("CHEMISTRY")),
      known).isRight)
    assert(DomainBranch.wellFormed(
      BranchManifest("SDS", None, Nil, primaryAncestor = Some("MISSING")), known).isLeft)
    assert(DomainBranch.wellFormed(
      BranchManifest("LAW", None, Nil, primaryAncestor = Some("LAW")), known).isLeft)
    assert(DomainBranch.wellFormed(
      BranchManifest("SDS", None, Nil, primaryAncestor = Some("LAW"), references = List("LAW")),
      known).isLeft)

  test("DomainBranch.wellFormed rejects transitive primary cycles"):
    val known = Set("A", "B", "C")
    // A→B→C→A
    val primaryOf: String => Option[String] =
      case "A" => Some("B")
      case "B" => Some("C")
      case "C" => Some("A")
      case _   => None
    assert(DomainBranch.wellFormed(
      BranchManifest("A", None, Nil, primaryAncestor = Some("B")),
      known, primaryOf).isLeft)
    assert(DomainBranch.primaryChain("A", Some("B"), primaryOf).isLeft)
    // acyclic A→B→C→trunk is fine
    val acyclic: String => Option[String] =
      case "A" => Some("B")
      case "B" => Some("C")
      case _   => None
    assert(DomainBranch.wellFormed(
      BranchManifest("A", None, Nil, primaryAncestor = Some("B")),
      known, acyclic).isRight)
