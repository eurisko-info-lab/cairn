package cairn.examples.pki

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*
import cairn.systemhandler.{Ed25519, Keypair}

/** PKI composition-root glue: pack façade from [[cairn.user.pki.Pki]] plus
  * Ed25519 / chain validation (generic crypto effects — not the object language).
  *
  * Object language: [[languages/pki.cairn]]. Changes: free ΔL only.
  */
final class Pki(packs: PackAccess):
  private val pack = cairn.user.pki.Pki(packs)

  export pack.{fragments, language, revokedNames}

  private def hex(bs: Array[Byte]): String = bs.map(b => f"${b & 0xff}%02x").mkString
  private def unhex(s: String): Array[Byte] =
    s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def signedPayload(name: String, keyHex: String): Array[Byte] =
    Canon.encode(Canon.cmap("name" -> Canon.CStr(name), "key" -> Canon.CStr(keyHex)))

  def certTerm(
      name: String,
      subject: Keypair,
      issuer: Keypair,
      notBefore: Long = 0L,
      notAfter: Long = Long.MaxValue
  ): Cst =
    val keyHex = hex(subject.publicBytes.toArray)
    val sig = Ed25519.sign(issuer.privateKey, signedPayload(name, keyHex))
    Cst.node(
      "cert",
      Cst.Leaf(name),
      Cst.Leaf(keyHex),
      Cst.Leaf(issuer.name),
      Cst.Leaf(hex(sig.toArray)),
      Cst.Leaf(notBefore.toString),
      Cst.Leaf(notAfter.toString))

  def rootTerm(root: Keypair): Cst = certTerm(root.name, root, root)

  final case class ValidationError(cert: String, reason: String):
    def render: String = s"chain validation failed at '$cert': $reason"

  private def certFields(term: Cst): Option[(String, String, String)] = term match
    case Cst.Node("cert", List(Cst.Leaf(_), Cst.Leaf(key), Cst.Leaf(issuer), Cst.Leaf(sig), _, _)) =>
      Some((key, issuer, sig))
    case Cst.Node("cert", List(Cst.Leaf(_), Cst.Leaf(key), Cst.Leaf(issuer), Cst.Leaf(sig))) =>
      Some((key, issuer, sig))
    case _ => None

  /** Validate that `name`'s certificate chains to a trust anchor within the
    * registry, with every link's Ed25519 signature verifying against the
    * issuer's registered key, and no soft-revoked name on the path.
    */
  def validateChain(registry: Module, name: String, anchors: Set[String]): Either[ValidationError, List[String]] =
    val revoked = revokedNames(registry)
    def lookup(n: String): Either[ValidationError, (String, String, String)] =
      if revoked.contains(n) then Left(ValidationError(n, "certificate is revoked"))
      else registry.get(n) match
        case Some(term) =>
          certFields(term).toRight(ValidationError(n, s"not a certificate term: ${term.render}"))
        case None => Left(ValidationError(n, "no certificate in registry (never issued)"))
    def walk(n: String, seen: Set[String]): Either[ValidationError, List[String]] =
      if seen.contains(n) then Left(ValidationError(n, "issuer cycle"))
      else
        lookup(n).flatMap { (keyHex, issuer, sigHex) =>
          lookup(issuer).flatMap { (issuerKeyHex, _, _) =>
            val ok = Ed25519.verify(unhex(issuerKeyHex).toVector, signedPayload(n, keyHex), unhex(sigHex).toVector)
            if !ok then Left(ValidationError(n, s"signature by '$issuer' does not verify"))
            else if anchors.contains(issuer) then
              if issuer == n then Right(List(n))
              else Right(List(n, issuer))
            else if issuer == n then Left(ValidationError(n, "self-signed but not a trust anchor"))
            else walk(issuer, seen + n).map(n :: _)
          }
        }
    walk(name, Set.empty)

  def anchorCertificateDigest(registry: Module, anchor: String): Either[String, Digest] =
    registry.get(anchor).toRight(s"anchor '$anchor' not in registry")
      .map(t => Artifact(ArtifactKind.Certificate, Cst.toCanon(t)).digest)
