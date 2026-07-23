package cairn.kernel

import Canon.*

class CanonSuite extends munit.FunSuite:
  val sample: Canon = Canon.cmap(
    "b" -> CInt(42),
    "a" -> CStr("héllo"),
    "c" -> Canon.clist(CBytes(Vector[Byte](1, 2, 3)), CTag("t", CInt(-7))),
  )

  test("encode/decode round-trip"):
    assertEquals(Canon.decode(Canon.encode(sample)), Right(sample))

  test("map ordering is canonical regardless of construction order"):
    val a = Canon.cmap("x" -> CInt(1), "y" -> CInt(2))
    val b = Canon.cmap("y" -> CInt(2), "x" -> CInt(1))
    assert(Canon.encode(a).sameElements(Canon.encode(b)))

  test("cmap is the quotient map for permutations: every shuffle of the same entries encodes identically"):
    val rnd = new scala.util.Random(42)
    for trial <- 1 to 200 do
      val n = 1 + rnd.nextInt(8)
      val keys = (0 until n).map(i => s"k$i-${rnd.nextInt(1000)}").distinct
      val entries = keys.map(k => k -> (CInt(rnd.nextLong()): Canon))
      val baseline = Canon.encode(Canon.cmap(entries*))
      for shuffleTrial <- 1 to 3 do
        val shuffled = rnd.shuffle(entries)
        assert(Canon.encode(Canon.cmap(shuffled*)).sameElements(baseline),
          s"trial $trial.$shuffleTrial: permutation changed canonical bytes for $entries")

  test("duplicate map keys rejected — no last-write-wins merge (§4.3, conflicts are errors)"):
    intercept[IllegalArgumentException](Canon.cmap("x" -> CInt(1), "x" -> CInt(2)))

  test("golden digest is stable (S2/S3 acceptance)"):
    // Locked fixtures — real hex values pinned from the current encoding,
    // not a value compared against itself (that would pass unconditionally
    // no matter how the encoding changed). Any accidental change to
    // Canon.encode's byte format now fails this test; a deliberate change
    // must update these constants explicitly.
    assertEquals(Digest.of(CStr("cairn")).hex,
      "adea0b37cbada05d1a123b3d1aa798f6d726c19f95ef01697f2cd470858bf89f")
    assertEquals(Digest.of(CStr("cairn ")).hex,
      "6405a0592901a1d1088e7e735c5b83134e78f89cd789289eb685fff59b882f2e")
    assertEquals(Digest.of(sample).hex,
      "80d9a51a9edef428c25413ed6403800ed6ca6d046aab6590bc2aebacae74e7a7")

  test("trailing bytes rejected"):
    val bs = Canon.encode(CInt(1)) ++ Array[Byte](0)
    assert(Canon.decode(bs).isLeft)

  // Raw byte builders — decode hardening needs to construct byte sequences
  // no production encoder can produce (negative counts, out-of-order/
  // duplicate map keys, malformed UTF-8), so these bypass Canon.encode
  // entirely rather than going through it.
  private def i32(n: Int): Array[Byte] =
    Array((n >>> 24).toByte, (n >>> 16).toByte, (n >>> 8).toByte, n.toByte)
  private def rawStr(bytes: Array[Byte]): Array[Byte] = i32(bytes.length) ++ bytes
  private def rawInt(v: Long): Array[Byte] = i32((v >>> 32).toInt) ++ i32(v.toInt)

  test("decode rejects a negative list count"):
    val bs = Array[Byte]('L'.toByte) ++ i32(-1)
    assert(Canon.decode(bs).isLeft)

  test("decode rejects a negative map count"):
    val bs = Array[Byte]('M'.toByte) ++ i32(-1)
    assert(Canon.decode(bs).isLeft)

  test("decode rejects a list/map count exceeding the remaining bytes"):
    assert(Canon.decode(Array[Byte]('L'.toByte) ++ i32(1_000_000)).isLeft)
    assert(Canon.decode(Array[Byte]('M'.toByte) ++ i32(1_000_000)).isLeft)

  test("decode rejects map entries out of canonical sorted order"):
    // "y" then "x" — valid entries individually, wrong order
    val bs = Array[Byte]('M'.toByte) ++ i32(2) ++
      rawStr("y".getBytes("UTF-8")) ++ Array[Byte]('I'.toByte) ++ rawInt(1) ++
      rawStr("x".getBytes("UTF-8")) ++ Array[Byte]('I'.toByte) ++ rawInt(2)
    assert(Canon.decode(bs).isLeft)

  test("decode rejects duplicate map keys (bypassing cmap's own guard)"):
    val bs = Array[Byte]('M'.toByte) ++ i32(2) ++
      rawStr("x".getBytes("UTF-8")) ++ Array[Byte]('I'.toByte) ++ rawInt(1) ++
      rawStr("x".getBytes("UTF-8")) ++ Array[Byte]('I'.toByte) ++ rawInt(2)
    assert(Canon.decode(bs).isLeft)

  test("decode rejects malformed UTF-8 in a string (no silent U+FFFD replacement)"):
    // 0xC3 alone is a lead byte for a 2-byte sequence with no continuation
    // byte — malformed. Java's permissive String(bytes, UTF_8) constructor
    // would silently turn this into a replacement character; decode must not.
    val bad = Array[Byte](0xC3.toByte)
    val bs = Array[Byte]('S'.toByte) ++ rawStr(bad)
    assert(Canon.decode(bs).isLeft)

  test("decode accepts well-formed multi-byte UTF-8"):
    val bs = Array[Byte]('S'.toByte) ++ rawStr("héllo".getBytes("UTF-8"))
    assertEquals(Canon.decode(bs), Right(CStr("héllo")))

  test("decode rejects nesting deeper than MaxNestingDepth (stack-bomb guard)"):
    // Tag chain of length MaxNestingDepth+1 around an int — each tag is one
    // compound frame. Built via encode so the bytes are well-formed; only
    // depth should fail.
    def nest(n: Int, inner: Canon = CInt(0)): Canon =
      (0 until n).foldLeft(inner)((v, i) => CTag(s"t$i", v))
    val tooDeep = nest(Canon.MaxNestingDepth + 1)
    val err = Canon.decode(Canon.encode(tooDeep))
    assert(err.isLeft, err.toString)
    assert(err.swap.exists(_.contains("nesting depth")), err.toString)
    // Exactly MaxNestingDepth compound frames around a leaf is still accepted.
    val atLimit = nest(Canon.MaxNestingDepth)
    assertEquals(Canon.decode(Canon.encode(atLimit)), Right(atLimit))

  test("decode nesting budget is enforced for lists and maps too"):
    // maxDepth=1: root compound OK, nested compound rejected.
    val nestedList = CList(List(CList(List(CInt(1)))))
    assert(Canon.decode(Canon.encode(nestedList), maxDepth = 1).isLeft)
    assertEquals(Canon.decode(Canon.encode(CList(List(CInt(1)))), maxDepth = 1),
      Right(CList(List(CInt(1)))))
    val nestedMap = Canon.cmap("a" -> Canon.cmap("b" -> CInt(1)))
    assert(Canon.decode(Canon.encode(nestedMap), maxDepth = 1).isLeft)

class ArtifactSuite extends munit.FunSuite:
  test("artifact round-trip for every kind (S5 acceptance)"):
    for kind <- ArtifactKind.values do
      val a = Artifact(kind, Canon.cmap("k" -> CStr(kind.name)))
      assertEquals(Artifact.decode(Canon.encode(a.canon)), Right(a))

  test("typed keys distinguish kind at equal value shape (S3)"):
    val body = CTag("x", CInt(1))
    val a = Artifact(ArtifactKind.Claim, body)
    val b = Artifact(ArtifactKind.Theorem, body)
    assert(a.key != b.key)
    val err = TypedKey.check(a.key, b.key)
    assert(err.isLeft)
    assert(err.swap.exists(_.contains("mismatch")))

class BindingSuite extends munit.FunSuite:
  // λ-calculus binder spec: lam binds child 0 in child 2 (name, type, body)
  val spec = BinderSpec(Map("lam" -> List((0, List(2)))))
  def v(x: String): Cst = Cst.node("var", Cst.Leaf(x))
  def lam(x: String, t: Cst, b: Cst): Cst = Cst.node("lam", Cst.Leaf(x), t, b)
  def app(f: Cst, a: Cst): Cst = Cst.node("app", f, a)
  val bool: Cst = Cst.node("tyBool")

  test("free vars"):
    assertEquals(Binding.freeVars(spec, "var")(lam("x", bool, app(v("x"), v("y")))), Set("y"))

  test("substitution avoids capture (S14 acceptance)"):
    // [y := x] (λx. y)  must NOT become λx. x
    val t = lam("x", bool, v("y"))
    val r = Binding.subst(spec, "var")(t, "y", v("x"))
    r match
      case Cst.Node("lam", List(Cst.Leaf(x2), _, Cst.Node("var", List(Cst.Leaf(bodyVar))))) =>
        assert(x2 != "x", "binder must be renamed")
        assertEquals(bodyVar, "x")
      case other => fail(s"unexpected shape: $other")

  test("shadowing stops substitution"):
    val t = lam("x", bool, v("x"))
    assertEquals(Binding.subst(spec, "var")(t, "x", v("z")), t)
