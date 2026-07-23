package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{CasAdmin, CasAdminEffects, CasEffects, Chunker, DigestMigration, DiskCas, EffectContext}
import cairn.examples.stlc.Stlc

/** Wave A acceptance (M1–M5).
  *
  * M3/M5 exercise gated admin/chunk paths. M4 algorithm agility remains a
  * direct DiskCas trait contract (putBytesAlgo / getBytesKey).
  */
class WaveASuite extends munit.FunSuite:
  private val casCtx = EffectContext.forCas()

  test("M1: distinct schemas => distinct type hashes"):
    import Canon.*
    // same head tag, different inner shape — old head-tag typeHash collided here
    val a = Artifact(ArtifactKind.Claim, CTag("x", CInt(1)))
    val b = Artifact(ArtifactKind.Claim, CTag("x", CStr("s")))
    assert(a.typeHash != b.typeHash)
    // extra map field changes the fingerprint
    val c = Artifact(ArtifactKind.Claim, Canon.cmap("f" -> CInt(1)))
    val d = Artifact(ArtifactKind.Claim, Canon.cmap("f" -> CInt(1), "g" -> CInt(2)))
    assert(c.typeHash != d.typeHash)
    // different VALUES with the same schema share a fingerprint
    val e = Artifact(ArtifactKind.Claim, Canon.cmap("f" -> CInt(1)))
    val f = Artifact(ArtifactKind.Claim, Canon.cmap("f" -> CInt(99)))
    assertEquals(e.typeHash, f.typeHash)

  test("M2: alpha-equivalent terms share a digest; names still print"):
    val lang = Stlc.language
    val spec = lang.binderSpec
    val idX = Stlc.lam1("x", Stlc.tBool, Stlc.v("x"))
    val idY = Stlc.lam1("y", Stlc.tBool, Stlc.v("y"))
    assert(idX != idY)
    assertEquals(Alpha.digest(spec, "var")(idX), Alpha.digest(spec, "var")(idY))
    assert(Alpha.equivalent(spec, "var")(Stlc.churchTrue,
      Stlc.lam1("a", Stlc.tBool, Stlc.lam1("b", Stlc.tBool, Stlc.v("a")))))
    // NOT equivalent to churchFalse
    assert(!Alpha.equivalent(spec, "var")(Stlc.churchTrue, Stlc.churchFalse))
    // free variables are never renamed
    assertEquals(Alpha.normalize(spec, "var")(Stlc.v("free")), Stlc.v("free"))
    // surface names survive: printer still prints the original tree
    val printed = Printer.print(lang.grammar, idY).toOption.get
    assert(printed.contains("y"))

  test("M2: nested and multi-binder normalization is level-based"):
    val spec = Stlc.language.binderSpec
    val t1 = Stlc.lam1("x", Stlc.tBool, Stlc.lam1("y", Stlc.tBool, Stlc.app1(Stlc.v("x"), Stlc.v("y"))))
    val t2 = Stlc.lam1("p", Stlc.tBool, Stlc.lam1("q", Stlc.tBool, Stlc.app1(Stlc.v("p"), Stlc.v("q"))))
    assertEquals(Alpha.normalize(spec, "var")(t1), Alpha.normalize(spec, "var")(t2))

  test("M3: fsck detects and quarantines corruption"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-fsck")
    val cas = DiskCas(dir)
    val d = CasEffects.putBytes(cas, "healthy".getBytes, casCtx).fold(e => fail(e.toString), identity)
    val d2 = CasEffects.putBytes(cas, "to be corrupted".getBytes, casCtx).fold(e => fail(e.toString), identity)
    val victim = dir.resolve("objects").resolve(d2.hex.take(2)).resolve(d2.hex.drop(2))
    java.nio.file.Files.write(victim, "garbage".getBytes)
    val report = CasAdminEffects.fsck(dir, casCtx).fold(e => fail(e.toString), identity)
    assertEquals(report.checked, 2)
    assertEquals(report.corrupt, List(d2))
    assert(CasEffects.contains(cas, d, casCtx).contains(true))
    assert(CasEffects.contains(cas, d2, casCtx).contains(false)) // quarantined, not served

  test("M3: GC never collects a reachable blob (property over random graphs)"):
    val rnd = new scala.util.Random(42)
    for _ <- 1 to 20 do
      val dir = java.nio.file.Files.createTempDirectory("cairn-gc")
      val cas = DiskCas(dir)
      // build a random DAG of artifacts referencing each other by digest
      var nodes = List.empty[Digest]
      for i <- 0 until 12 do
        val refs = nodes.filter(_ => rnd.nextBoolean()).take(3)
        val a = Artifact(ArtifactKind.Ir, Canon.cmap(
          "n" -> Canon.CInt(i), "refs" -> Canon.cstrs(refs.map(_.hex))))
        nodes = CasEffects.put(cas, a, casCtx).fold(e => fail(e.toString), _.valueHash) :: nodes
      val root = nodes.head
      def reachable(d: Digest, acc: Set[String]): Set[String] =
        if acc.contains(d.hex) then acc
        else CasEffects.getBytes(cas, d, casCtx).toOption.map(CasAdmin.references)
          .getOrElse(Set.empty).foldLeft(acc + d.hex)((s, r) => reachable(r, s))
      val expected = reachable(root, Set.empty)
      CasAdminEffects.gc(dir, Set(root), casCtx).fold(e => fail(e.toString), identity)
      for hex <- expected do
        assert(CasEffects.contains(cas, Digest(hex), casCtx).contains(true),
          s"reachable $hex was collected")

  test("M3: stats report objects by kind"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-stats")
    val cas = DiskCas(dir)
    Stlc.fragments.foreach(f =>
      CasEffects.put(cas, f.artifact, casCtx).fold(e => fail(e.toString), identity))
    CasEffects.put(cas, Stlc.language.artifact, casCtx).fold(e => fail(e.toString), identity)
    val s = CasAdminEffects.stats(dir, casCtx).fold(e => fail(e.toString), identity)
    assertEquals(s.objects, 6)
    assertEquals(s.byKind.get("fragment"), Some(5))
    assertEquals(s.byKind.get("language"), Some(1))

  test("M4: self-describing keys readable across algorithms"):
    // Intentional direct DiskCas trait contract for digest agility.
    val dir = java.nio.file.Files.createTempDirectory("cairn-algo")
    val cas = DiskCas(dir)
    val bs = "algorithm agility".getBytes
    val k256 = cas.putBytesAlgo("sha256", bs)
    val k512 = cas.putBytesAlgo("sha512", bs)
    assert(k256.startsWith("sha256:"))
    assert(k512.startsWith("sha512:"))
    assert(cas.getBytesKey(k256).exists(_.sameElements(bs)))
    assert(cas.getBytesKey(k512).exists(_.sameElements(bs)))
    assert(cas.getBytesKey("md5:abc").isLeft)

  test("M4: migration artifact is kernel-validated"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-mig")
    val cas = DiskCas(dir)
    val blobs = List("one", "two", "three").map(_.getBytes)
    blobs.foreach(bs => CasEffects.putBytes(cas, bs, casCtx).fold(e => fail(e.toString), identity))
    val migration = DigestMigration.build(blobs)
    val fetch = (hex: String) => Digest.parse(hex).flatMap(d =>
      CasEffects.getBytes(cas, d, casCtx).left.map(_.toString))
    assertEquals(DigestMigration.validate(migration, fetch), Right(3))
    // tampered mapping fails
    import Canon.*
    val bad = Artifact(ArtifactKind.Migration, CList(List(Canon.cmap(
      "sha256" -> CStr(Digest.ofBytes(blobs.head).hex),
      "sha512" -> CStr("0" * 128)))))
    assert(DigestMigration.validate(bad, fetch).swap.exists(_.contains("sha512 mismatch")))
    // migration is itself a publishable artifact
    assertEquals(migration.kind.name, "migration")

  test("M5: 100 MB blob streams in bounded memory with chunk dedup"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-chunk")
    val cas = DiskCas(dir)
    // 100 MiB stream: a repeating 1 MiB pseudo-random block => high dedup
    val block = new Array[Byte](1024 * 1024)
    new scala.util.Random(7).nextBytes(block)
    val totalBlocks = 100
    def stream() = new java.io.InputStream:
      private var i = 0L
      private val total = totalBlocks.toLong * block.length
      def read(): Int =
        if i >= total then -1
        else { val b = block(((i % block.length)).toInt) & 0xff; i += 1; b }
      override def read(b: Array[Byte], off: Int, len: Int): Int =
        if i >= total then -1
        else
          val n = math.min(len.toLong, math.min(block.length - (i % block.length), total - i)).toInt
          System.arraycopy(block, (i % block.length).toInt, b, off, n); i += n; n
    val manifest = Chunker.putStream(cas, stream(), casCtx).fold(e => fail(e), identity)
    // round-trip: count bytes and re-hash while streaming out
    val outMd = java.security.MessageDigest.getInstance("SHA-256")
    val counter = new java.io.OutputStream:
      def write(b: Int): Unit = outMd.update(b.toByte)
      override def write(b: Array[Byte], off: Int, len: Int): Unit = outMd.update(b, off, len)
    assertEquals(Chunker.getStream(cas, manifest, counter, casCtx), Right(totalBlocks.toLong * block.length))
    // content identical to the source stream
    val srcMd = java.security.MessageDigest.getInstance("SHA-256")
    val s = stream(); val buf = new Array[Byte](1 << 16)
    var r = s.read(buf, 0, buf.length)
    while r >= 0 do { srcMd.update(buf, 0, r); r = s.read(buf, 0, buf.length) }
    assert(srcMd.digest.sameElements(outMd.digest))
    // dedup: repeating content must NOT store ~100 distinct MiBs of chunks
    val stats = CasAdminEffects.stats(dir, casCtx).fold(e => fail(e.toString), identity)
    assert(stats.bytes < 60L * 1024 * 1024, s"dedup failed: ${stats.bytes} bytes stored")
