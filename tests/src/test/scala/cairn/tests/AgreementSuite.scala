package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.core.Agreement.NativeSource
import cairn.systemhandler.EffectContext
import cairn.examples.icnet.IcNet
import cairn.examples.affinenet.AffineNet
import cairn.examples.stlc.Stlc
import cairn.runtime.PackLoader

import java.nio.file.{Files, Path}

/** Differential agreement harness for LeanCore and HVM/IC envelopes
  * (`docs/agreement.md`).
  *
  * Always runs Cairn-side reference checks. Native `lean` / `hvm` on PATH are
  * optional: Lean is invoked live when present; otherwise goldens. HVM is
  * stubbed (`no-hvm-surface-exporter`) until an exporter exists — certificates
  * still bind Cairn to classical-IC goldens.
  */
class AgreementSuite extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private val packs = PackLoader(EffectContext.forPackLoader())
  private val LeanCore = cairn.examples.leancore.LeanCore(packs)

  private def onPath(name: String): Option[Path] =
    sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).iterator
      .map(dir => Path.of(dir, name))
      .find(Files.isExecutable)

  private def runCmd(cmd: List[String]): (Int, String) =
    val pb = new ProcessBuilder(cmd*)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val out = new String(proc.getInputStream.readAllBytes())
    (proc.waitFor(), out)

  private def leanCheck(caseName: String, body: String): (Int, NativeSource) =
    onPath("lean") match
      case None => (-1, NativeSource.Golden)
      case Some(lean) =>
        val dir = Files.createTempDirectory("cairn-agree-lean")
        val f = dir.resolve(s"$caseName.lean")
        // Build without stripMargin — body may contain `|` match arms that
        // an outer stripMargin would corrupt.
        val src =
          s"""/- Cairn lean-core agreement: $caseName -/
             |inductive N : Type where
             |  | z : N
             |  | s : N -> N
             |
             |""".stripMargin + body
        Files.writeString(f, src)
        val (code, _) = runCmd(List(lean.toString, f.toString))
        (code, NativeSource.Live("lean", s"exit=$code"))

  private def certify(
      env: Agreement.Envelope,
      caseName: String,
      subject: Digest,
      cairn: Digest,
      native: Digest,
      source: NativeSource
  ): Agreement.AgreementCertificate =
    Agreement.certify(env, caseName, subject, cairn, native, source)
      .fold(e => fail(e), identity)

  // ---- Lean envelope -------------------------------------------------------

  test("lean-core envelope is stable"):
    val e = Agreement.leanCore
    assertEquals(e.id, "lean-core")
    assert(e.claims.nonEmpty)
    assert(e.excludes.exists(_.contains("Lean 4 kernel")))
    assertEquals(e.digest, Agreement.leanCore.digest)

  test("lean-core: refl(zero) : Eq(Nat,zero,zero)"):
    import LeanCore.*
    val term = refl(zero)
    val ty = eqTy(natTy, zero, zero)
    assert(check(ctxNil, term, ty).isRight)
    val cairn = Agreement.outcome("ok")
    val (code, src) = leanCheck("refl-zero",
      """theorem ok : N.z = N.z := Eq.refl N.z
        |#check ok
        |""".stripMargin)
    src match
      case NativeSource.Live(_, _) => assertEquals(code, 0)
      case _                       => ()
    val native = Agreement.outcome("ok")
    val c = certify(Agreement.leanCore, "refl-zero",
      Digest.of(Canon.cmap("term" -> Cst.toCanon(term), "ty" -> Cst.toCanon(ty))),
      cairn, native, src)
    assert(c.agreed)
    assertEquals(c.artifact.kind, ArtifactKind.AgreementCertificate)

  test("lean-core: refl rejects unequal endpoints"):
    import LeanCore.*
    val term = refl(zero)
    val ty = eqTy(natTy, zero, succ(zero))
    assert(check(ctxNil, term, ty).isLeft)
    val cairn = Agreement.outcome("reject")
    val (code, src) = leanCheck("refl-bad",
      """theorem bad : N.z = N.s N.z := Eq.refl N.z
        |""".stripMargin)
    src match
      case NativeSource.Live(_, _) => assert(code != 0, "Lean must reject unequal refl")
      case _                       => ()
    val c = certify(Agreement.leanCore, "refl-bad",
      Digest.of(Canon.cmap("term" -> Cst.toCanon(term), "ty" -> Cst.toCanon(ty))),
      cairn, Agreement.outcome("reject"), src)
    assert(c.agreed)

  test("lean-core: subst along refl is identity"):
    import LeanCore.*
    val motive = lam("x", natTy, natTy)
    val term = subst(motive, refl(zero), zero)
    assert(check(ctxNil, term, natTy).isRight)
    assertEquals(normalize(term).fold(e => fail(e), identity), zero)
    val cairn = Agreement.outcome("ok", Cst.toCanon(zero))
    val (code, src) = leanCheck("subst-refl",
      """def transport {A : Type} {x y : A} (P : A -> Type) (e : x = y) (px : P x) : P y :=
        |  match e with
        |  | rfl => px
        |def ok : N := transport (fun _ => N) (rfl : N.z = N.z) N.z
        |#check ok
        |""".stripMargin)
    src match
      case NativeSource.Live(_, _) => assertEquals(code, 0)
      case _                       => ()
    val c = certify(Agreement.leanCore, "subst-refl",
      Digest.of(Cst.toCanon(term)), cairn, Agreement.outcome("ok", Cst.toCanon(zero)), src)
    assert(c.agreed)

  // ---- HVM / IC envelope ---------------------------------------------------

  private def hvmSource: NativeSource =
    if onPath("hvm").isDefined then NativeSource.Stub("no-hvm-surface-exporter")
    else NativeSource.Golden

  test("hvm-ic envelope is stable"):
    val e = Agreement.hvmIc
    assertEquals(e.id, "hvm-ic")
    assert(e.claims.exists(_.contains("IcNet")))
    assert(e.excludes.exists(_.contains("ABI")))
    assert(!AffineNet.language.kinds.exists(_.name == "dup"))
    assert(IcNet.language.kinds.exists(_.name == "dup"))

  test("hvm-ic: (λx. x) true → true (classical golden)"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val expected = Stlc.tru
    val Right((net, root)) = IcNet.lower(term): @unchecked
    val Right(normal) = NetEngine.normalize(IcNet.language, net): @unchecked
    val Right(got) = IcNet.readback(normal, root): @unchecked
    assertEquals(got, expected)
    val cairn = Agreement.outcome("ok", Cst.toCanon(got))
    val golden = Agreement.outcome("ok", Cst.toCanon(expected))
    val c = certify(Agreement.hvmIc, "id-true", Digest.of(Cst.toCanon(term)),
      cairn, golden, hvmSource)
    assert(c.agreed)
    assert(c.source.startsWith("golden") || c.source.startsWith("stub:"))

  test("hvm-ic: self-application with δ agrees with tree eval (classical)"):
    val selfApp = Stlc.lam1("d", Stlc.tBool, Stlc.app1(Stlc.v("d"), Stlc.v("d")))
    val term = Stlc.app1(selfApp, Stlc.idBool)
    val Right(treeValue) = TreeEngine.normalize(Stlc.language, term): @unchecked
    val Right((net, root)) = IcNet.lower(term): @unchecked
    val Right(normal) = NetEngine.normalize(IcNet.language, net): @unchecked
    val Right(netValue) = IcNet.readback(normal, root): @unchecked
    val spec = Stlc.language.binderSpec
    assert(Alpha.equivalent(spec, "var")(treeValue, netValue),
      s"tree ${treeValue.render} vs net ${netValue.render}")
    val cairn = Agreement.outcome("ok", Cst.toCanon(Alpha.normalize(spec, "var")(netValue)))
    val golden = Agreement.outcome("ok", Cst.toCanon(Alpha.normalize(spec, "var")(treeValue)))
    val c = certify(Agreement.hvmIc, "dup-id", Digest.of(Cst.toCanon(term)),
      cairn, golden, hvmSource)
    assert(c.agreed)

  test("hvm-ic: AffineNet era-fan erases constants (classical)"):
    // Same fixture as Phase3: era meets fan whose aux hold konsts → empty NF.
    val b = AffineNet.Builder()
    val e = b.agent("era"); val f = b.agent("fan")
    val k1 = b.agent("konst"); val k2 = b.agent("konst")
    b.wire(PortRef(e, 0), PortRef(f, 0))
    b.wire(PortRef(f, 1), PortRef(k1, 0))
    b.wire(PortRef(f, 2), PortRef(k2, 0))
    val Right(n2) = NetEngine.normalize(AffineNet.language, b.net): @unchecked
    assertEquals(n2.agents.size, 0)
    val cairn = Agreement.outcome("ok", Canon.CInt(0))
    val golden = Agreement.outcome("ok", Canon.CInt(0))
    val c = certify(Agreement.hvmIc, "era-fan",
      Digest.of(Canon.CStr("era-fan-fixture")), cairn, golden, hvmSource)
    assert(c.agreed)

  test("Agreement.check rejects forged agreement flag"):
    val bog = Agreement.AgreementCertificate(
      "lean-core", Agreement.leanCore.digest, "forged", Digest.of(Canon.CStr("s")),
      Agreement.outcome("ok"), Agreement.outcome("fail"),
      "golden", Agreement.evidenceFor(NativeSource.Golden), agreed = true)
    assert(Agreement.check(bog).isLeft)

  test("Agreement.check rejects envelope digest drift"):
    val ok = Agreement.outcome("ok")
    val cert = Agreement.AgreementCertificate(
      Agreement.leanCore.id, Digest.of(Canon.CStr("tampered-envelope")),
      "drift", Digest.of(Canon.CStr("s")), ok, ok,
      "golden", Agreement.evidenceFor(NativeSource.Golden), agreed = true)
    assert(Agreement.check(cert, Some(Agreement.leanCore)).isLeft)
