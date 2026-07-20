package cairn.examples.pki

import cairn.kernel.*
import cairn.workbench.*
import cairn.proof.{CheckerCfg, Checker, Search, Derivation}
import cairn.ledger.{Ed25519, Keypair}

/** PKI maximal (M46): validity windows, CRLs as signed artifacts, and — the
  * centerpiece — chain validation as DECLARATIVE inference-rule data checked
  * by the SAME kernel checker as STLC typing. Ed25519 verification and
  * anchor membership are injected side-condition evaluators (M19): the
  * checker stays the only certifier; crypto only answers its questions.
  *
  * Certificate term: cert(name, keyHex, issuer, sigHex, notBefore, notAfter)
  * Registry: a ctxCons/ctxNil context of (name -> cert) — so lookups are the
  * checker's built-in computational premise, shadowing rules included.
  */
object PkiMax:
  private def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)
  private def mv(x: String): Cst = Cst.Leaf(s"$$$x")

  /** The chain-validation judgment as declarative rule DATA. */
  val chainJudgment: JudgmentDef = JudgmentDef("chainOk", List(
    InferRule("chain-anchor",
      premises = List(n("$ctx-lookup", mv("reg"), mv("n"),
        n("cert", mv("n"), mv("k"), mv("n"), mv("sig"), mv("nb"), mv("na")))),
      conclusion = n("chainOk", mv("reg"), mv("n"), mv("now")),
      conditions = List(
        n("$anchor", mv("n")),
        n("$sig-ok", mv("n"), mv("k"), mv("k"), mv("sig")),
        n("$le", mv("nb"), mv("now")),
        n("$le", mv("now"), mv("na")))),
    InferRule("chain-step",
      premises = List(
        n("$ctx-lookup", mv("reg"), mv("n"),
          n("cert", mv("n"), mv("k"), mv("iss"), mv("sig"), mv("nb"), mv("na"))),
        n("$ctx-lookup", mv("reg"), mv("iss"),
          n("cert", mv("iss"), mv("ik"), mv("i2"), mv("s2"), mv("nb2"), mv("na2"))),
        n("chainOk", mv("reg"), mv("iss"), mv("now"))),
      conclusion = n("chainOk", mv("reg"), mv("n"), mv("now")),
      conditions = List(
        n("$neq", mv("n"), mv("iss")),
        n("$sig-ok", mv("n"), mv("k"), mv("ik"), mv("sig")),
        n("$le", mv("nb"), mv("now")),
        n("$le", mv("now"), mv("na"))))))

  private def hex(bs: Array[Byte]): String = bs.map(b => f"${b & 0xff}%02x").mkString
  private def unhex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def signedPayload(name: String, keyHex: String): Array[Byte] =
    Canon.encode(Canon.cmap("name" -> Canon.CStr(name), "key" -> Canon.CStr(keyHex)))

  /** Side-condition evaluators injected into the kernel checker. */
  def extensions(anchors: Set[String]): Map[String, List[Cst] => Either[String, Boolean]] = Map(
    "$anchor" -> {
      case List(Cst.Leaf(name)) => Right(anchors.contains(name))
      case other => Left(s"$$anchor: bad args $other")
    },
    "$sig-ok" -> {
      case List(Cst.Leaf(name), Cst.Leaf(keyHex), Cst.Leaf(issuerKeyHex), Cst.Leaf(sigHex)) =>
        Right(Ed25519.verify(unhex(issuerKeyHex).toVector, signedPayload(name, keyHex), unhex(sigHex).toVector))
      case other => Left(s"$$sig-ok: bad args $other")
    })

  def checkerCfg(anchors: Set[String]): CheckerCfg =
    CheckerCfg(List(chainJudgment), extensions = extensions(anchors))

  def certTerm(name: String, subject: Keypair, issuer: Keypair, notBefore: Long, notAfter: Long): Cst =
    val keyHex = hex(subject.publicBytes.toArray)
    val sig = Ed25519.sign(issuer.privateKey, signedPayload(name, keyHex))
    n("cert", Cst.Leaf(name), Cst.Leaf(keyHex), Cst.Leaf(issuer.name),
      Cst.Leaf(hex(sig.toArray)), Cst.Leaf(notBefore.toString), Cst.Leaf(notAfter.toString))

  /** Registry (name -> cert) as a checker context. */
  def registryCtx(entries: List[(String, Cst)]): Cst =
    entries.foldRight(n("ctxNil")) { case ((name, cert), acc) => n("ctxCons", Cst.Leaf(name), cert, acc) }

  def goal(reg: Cst, name: String, now: Long): Cst =
    n("chainOk", reg, Cst.Leaf(name), Cst.Leaf(now.toString))

  /** Untrusted proposer: search for a chain derivation. Certification is the
    * checker's, not the search's.
    */
  def validate(reg: Cst, name: String, now: Long, anchors: Set[String]): Either[String, Derivation] =
    val cfg = checkerCfg(anchors)
    Search.infer(cfg, goal(reg, name, now)).flatMap { d =>
      Checker.check(cfg, d).left.map(_.render).map(_ => d) }

  // ---- CRLs as signed artifacts, anchored on the ledger ----

  final case class Crl(issuer: String, revoked: List[String], sig: Vector[Byte]):
    def canon: Canon = Canon.CTag("crl", Canon.cmap(
      "issuer" -> Canon.CStr(issuer),
      "revoked" -> Canon.cstrs(revoked.sorted),
      "sig" -> Canon.CBytes(sig)))
    def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)

  object Crl:
    def payload(issuer: String, revoked: List[String]): Array[Byte] =
      Canon.encode(Canon.cmap("issuer" -> Canon.CStr(issuer), "revoked" -> Canon.cstrs(revoked.sorted)))
    def issue(issuer: Keypair, revoked: List[String]): Crl =
      Crl(issuer.name, revoked.sorted, Ed25519.sign(issuer.privateKey, payload(issuer.name, revoked)))
    def verify(crl: Crl, issuerKey: Vector[Byte]): Boolean =
      Ed25519.verify(issuerKey, payload(crl.issuer, crl.revoked), crl.sig)

  /** Apply a verified CRL to a registry context: revoked entries vanish. */
  def applyCrl(reg: Cst, crl: Crl): Cst = reg match
    case Cst.Node("ctxCons", List(Cst.Leaf(name), cert, rest)) =>
      if crl.revoked.contains(name) then applyCrl(rest, crl)
      else n("ctxCons", Cst.Leaf(name), cert, applyCrl(rest, crl))
    case other => other
