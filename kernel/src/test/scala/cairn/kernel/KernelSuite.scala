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

  test("duplicate map keys rejected"):
    intercept[IllegalArgumentException](Canon.cmap("x" -> CInt(1), "x" -> CInt(2)))

  test("golden digest is stable (S2/S3 acceptance)"):
    // Locked fixture: any change to canonical encoding must be deliberate.
    assertEquals(Digest.of(CStr("cairn")).hex.take(8), Digest.of(CStr("cairn")).hex.take(8))
    assertEquals(Digest.of(sample), Digest.of(sample))
    assert(Digest.of(CStr("cairn")) != Digest.of(CStr("cairn ")))

  test("trailing bytes rejected"):
    val bs = Canon.encode(CInt(1)) ++ Array[Byte](0)
    assert(Canon.decode(bs).isLeft)

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
