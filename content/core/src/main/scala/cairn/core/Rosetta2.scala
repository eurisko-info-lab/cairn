package cairn.core

import cairn.kernel.*

/** Rosetta v2 (M30–M34): richer declaration vocabulary — polymorphic defs with
  * `Ord`-style constraints, ADT declarations, list pattern matching, effect
  * declarations and effectful defs — projected to FOUR hosts (Scala, Lean,
  * Haskell, Rust).
  *
  * Whole-file round-trip discipline (M31): every emitted file is parsed back
  * under a per-host FILE grammar (header captured via rest-of-line, signatures
  * fully grammatical, bodies verbatim single-line regions) and checked for a
  * print∘parse∘print byte fixpoint. Generated (non-prelude) bodies are
  * additionally round-tripped under a per-host EXPRESSION grammar.
  */
enum RTy:
  case RInt, RBool, RUnit
  case RVar(name: String)
  case RList(of: RTy)

final case class RDefV2(
    name: String,
    typeParams: List[String],                 // e.g. List("a")
    constraints: List[(String, String)],      // (param, "Ord")
    params: List[(String, RTy)],
    ret: RTy,
    body: Cst,                                // host-neutral expression
    effects: List[String] = Nil,              // effect names, e.g. List("counter", "logger")
)

final case class RDataV2(name: String, typeParams: List[String], ctors: List[(String, List[RTy])])
final case class REffectV2(name: String, ops: List[String]) // unit-typed ops

final case class RosettaModule2(
    name: String,
    datas: List[RDataV2],
    effects: List[REffectV2],
    defs: List[RDefV2],
    theorems: List[RTheorem],
):
  private def tyCanon(t: RTy): Canon = t match
    case RTy.RInt      => Canon.CStr("Int")
    case RTy.RBool     => Canon.CStr("Bool")
    case RTy.RUnit     => Canon.CStr("Unit")
    case RTy.RVar(n)   => Canon.CTag("var", Canon.CStr(n))
    case RTy.RList(of) => Canon.CTag("list", tyCanon(of))
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "datas" -> Canon.CList(datas.map(d => Canon.cmap(
      "name" -> Canon.CStr(d.name),
      "typeParams" -> Canon.cstrs(d.typeParams),
      "ctors" -> Canon.CList(d.ctors.map((c, ts) => Canon.cmap(
        "name" -> Canon.CStr(c), "args" -> Canon.CList(ts.map(tyCanon)))))))),
    "effects" -> Canon.CList(effects.map(e => Canon.cmap(
      "name" -> Canon.CStr(e.name), "ops" -> Canon.cstrs(e.ops)))),
    "defs" -> Canon.CList(defs.map(d => Canon.cmap(
      "name" -> Canon.CStr(d.name),
      "typeParams" -> Canon.cstrs(d.typeParams),
      "constraints" -> Canon.CList(d.constraints.map((p, c) => Canon.cmap(
        "param" -> Canon.CStr(p), "cls" -> Canon.CStr(c)))),
      "params" -> Canon.CList(d.params.map((p, t) => Canon.cmap(
        "name" -> Canon.CStr(p), "type" -> tyCanon(t)))),
      "ret" -> tyCanon(d.ret),
      "body" -> Cst.toCanon(d.body),
      "effects" -> Canon.cstrs(d.effects)))),
    "theorems" -> Canon.CList(theorems.map(t => Canon.cmap(
      "name" -> Canon.CStr(t.name), "statement" -> Cst.toCanon(t.statement)))))
  def artifact: Artifact = Artifact(ArtifactKind.RosettaDecl, Canon.CTag("rosetta-v2", canon))

/** A host projection: full file text + which region is the round-trip-checked
  * declaration region + generated-body texts for expression-level checks.
  */
final case class PortOutput(hostName: String, fileName: String, text: String,
                            generatedBodies: List[String])

trait PortV2:
  def hostName: String
  def emit(m: RosettaModule2): Either[String, PortOutput]
  /** whole-file grammar used for the byte-fixpoint check */
  def fileGrammar: GrammarSpec

object PortV2:
  /** M31 gate: parse the whole emitted file, reprint, reparse, require byte
    * equality — every port must pass before its output is written anywhere.
    */
  def verified(port: PortV2, m: RosettaModule2): Either[String, PortOutput] =
    for
      _ <- RosettaChecker.validate(m)
        .left.map(errs => s"invalid RosettaModule2 '${m.name}': ${errs.map(_.render).mkString("; ")}")
      out <- port.emit(m)
      _ <- RoundTrip.fixpoint(port.fileGrammar, out.text)
        .left.map(e => s"${port.hostName}: whole-file fixpoint failed: $e")
    yield out

/** Shared host-neutral expression rendering helpers. */
private[core] object ExprUtil:
  /** `trueF`/`falseF`/`eqF`/`leF`/`andF` (M-relations): the minimal boolean
    * vocabulary a relation body needs (comparison, equality, conjunction) —
    * added so `sorted`/`perm`-style decision procedures can be expressed ONCE,
    * host-neutrally, instead of hand-duplicated per host (see `rels` builders
    * below and their use in `QuickSort2`).
    */
  def foldE[A](r: Cst)(
      varF: String => A, numF: String => A, nilF: => A,
      callF: (String, List[A]) => A, ifF: (A, A, A) => A,
      matchF: (A, A, String, String, A) => A, seqF: (A, A) => A,
      trueF: => A, falseF: => A, eqF: (A, A) => A, leF: (A, A) => A, andF: (A, A) => A): A =
    def go(t: Cst): A = t match
      case Cst.Node("rvar", List(Cst.Leaf(x)))  => varF(x)
      case Cst.Node("rnum", List(Cst.Leaf(n)))  => numF(n)
      case Cst.Node("rnil", _)                  => nilF
      case Cst.Node("rcall", Cst.Leaf(f) :: as) => callF(f, as.map(go))
      case Cst.Node("rif", List(c, a, b))       => ifF(go(c), go(a), go(b))
      case Cst.Node("rmatch", List(scrut, nilB, Cst.Leaf(h), Cst.Leaf(t0), consB)) =>
        matchF(go(scrut), go(nilB), h, t0, go(consB))
      case Cst.Node("rseq", List(a, b))         => seqF(go(a), go(b))
      case Cst.Node("rtrue", _)                 => trueF
      case Cst.Node("rfalse", _)                => falseF
      case Cst.Node("req", List(a, b))          => eqF(go(a), go(b))
      case Cst.Node("rle", List(a, b))          => leF(go(a), go(b))
      case Cst.Node("rand", List(a, b))         => andF(go(a), go(b))
      case other => throw CodecError(s"not a rosetta v2 expr: ${other.render}")
    go(r)

/** Builders for the boolean vocabulary above — the shared surface pack
  * authors (e.g. `QuickSort2`) use to state a relation exactly once.
  */
object RelBuilders:
  def rtrue: Cst = Cst.node("rtrue")
  def rfalse: Cst = Cst.node("rfalse")
  def req(a: Cst, b: Cst): Cst = Cst.node("req", a, b)
  def rle(a: Cst, b: Cst): Cst = Cst.node("rle", a, b)
  def rand(a: Cst, b: Cst): Cst = Cst.node("rand", a, b)
