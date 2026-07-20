package cairn.examples.pki

import cairn.kernel.*
import cairn.workbench.*
import cairn.ledger.{Ed25519, Keypair}

/** PKI pack (S47, §5b): Cairn's first domain application, mirroring GRANITE's
  * PKI: a certificate-registry object language whose ΔPKI (issue/revoke) is
  * the GENERIC free changes language ΔL — issue = `add`, revoke = `remove` —
  * plus a chain-validation judgment over real Ed25519 signatures, and ledger
  * trust-anchor publication. Nothing here touches the kernel (§4.11).
  *
  * Surface:  cert alice key "<hex>" issuer root sig "<hex>"
  * A root (trust anchor) is self-issued: issuer = its own name.
  */
object Pki:
  val certs: Fragment = Fragment(
    name = "certs",
    provides = List("cert"),
    requires = Nil,
    sorts = List(SortDef("Cert", SortMode.Tree)),
    constructors = List(
      CtorDef("cert", "Cert", List("Name", "Hex", "Name", "Hex"))),
    varCtor = Some("issuerRef"), // references between defs use issuer names
    grammar = GrammarPart(
      keywords = List("cert", "key", "issuer", "sig"),
      categories = List(CategorySpec("certTerm", List(
        ConstructorSpec("cert", List(
          Elem.Tok("cert"), Elem.NameLeaf, Elem.Tok("key"), Elem.StrLeaf,
          Elem.Tok("issuer"), Elem.NameLeaf, Elem.Tok("sig"), Elem.StrLeaf))))),
      printRules = List(
        PrintRule("cert", List(
          PrintSeg.Lit("cert"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("key"), PrintSeg.Space, PrintSeg.StrField(1), PrintSeg.Space,
          PrintSeg.Lit("issuer"), PrintSeg.Space, PrintSeg.Field(2), PrintSeg.Space,
          PrintSeg.Lit("sig"), PrintSeg.Space, PrintSeg.StrField(3)))),
      top = Some("certTerm")))

  val fragments: List[Fragment] = List(certs)

  def language: ComposedLanguage =
    Compose.compose("pki", fragments) match
      case Right(l)   => l
      case Left(errs) => throw RuntimeException(errs.map(_.render).mkString("\n"))

  // -- certificate construction (untrusted proposer side) --

  private def hex(bs: Array[Byte]): String = bs.map(b => f"${b & 0xff}%02x").mkString
  private def unhex(s: String): Array[Byte] =
    s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def signedPayload(name: String, keyHex: String): Array[Byte] =
    Canon.encode(Canon.cmap("name" -> Canon.CStr(name), "key" -> Canon.CStr(keyHex)))

  def certTerm(name: String, subject: Keypair, issuer: Keypair): Cst =
    val keyHex = hex(subject.publicBytes.toArray)
    val sig = Ed25519.sign(issuer.privateKey, signedPayload(name, keyHex))
    Cst.node("cert", Cst.Leaf(name), Cst.Leaf(keyHex), Cst.Leaf(issuer.name), Cst.Leaf(hex(sig.toArray)))

  def rootTerm(root: Keypair): Cst = certTerm(root.name, root, root)

  // -- chain validation judgment (S47) --

  final case class ValidationError(cert: String, reason: String):
    def render: String = s"chain validation failed at '$cert': $reason"

  /** Validate that `name`'s certificate chains to a trust anchor within the
    * registry, with every link's Ed25519 signature verifying against the
    * issuer's registered key, and no revoked (absent) issuer.
    */
  def validateChain(registry: Module, name: String, anchors: Set[String]): Either[ValidationError, List[String]] =
    def lookup(n: String): Either[ValidationError, (String, String, String)] =
      registry.get(n) match
        case Some(Cst.Node("cert", List(Cst.Leaf(cn), Cst.Leaf(key), Cst.Leaf(issuer), Cst.Leaf(sig)))) =>
          Right((key, issuer, sig))
        case Some(other) => Left(ValidationError(n, s"not a certificate term: ${other.render}"))
        case None        => Left(ValidationError(n, "no certificate in registry (revoked or never issued)"))
    def walk(n: String, seen: Set[String]): Either[ValidationError, List[String]] =
      if seen.contains(n) then Left(ValidationError(n, "issuer cycle"))
      else
        lookup(n).flatMap { (keyHex, issuer, sigHex) =>
          lookup(issuer).flatMap { (issuerKeyHex, _, _) =>
            val ok = Ed25519.verify(unhex(issuerKeyHex).toVector, signedPayload(n, keyHex), unhex(sigHex).toVector)
            if !ok then Left(ValidationError(n, s"signature by '$issuer' does not verify"))
            else if anchors.contains(issuer) then
              if issuer == n then Right(List(n)) // self-signed anchor
              else Right(List(n, issuer))
            else if issuer == n then Left(ValidationError(n, "self-signed but not a trust anchor"))
            else walk(issuer, seen + n).map(n :: _)
          }
        }
    walk(name, Set.empty)

  /** Trust-anchor publication: the anchor cert digest recorded on the ledger. */
  def anchorCertificateDigest(registry: Module, anchor: String): Either[String, Digest] =
    registry.get(anchor).toRight(s"anchor '$anchor' not in registry")
      .map(t => Artifact(ArtifactKind.Certificate, Cst.toCanon(t)).digest)
