package cairn.examples.pki

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*
import cairn.systemhandler.{Ed25519, Keypair}

/** PKI composition-root glue: pack façade from [[cairn.user.pki.Pki]] plus
  * Ed25519 (generic crypto effects — not the object language). Chain
  * validation is [[PkiMax.validate]] (declarative `chainOk`,
  * [[languages/pki.cairn]]) — the only certifier, for both revocation
  * mechanisms (CRL and in-registry `revocation` defs via
  * [[PkiMax.moduleRegistryCtx]]).
  *
  * Object language: [[languages/pki.cairn]]. Changes: free ΔL only.
  */
final class Pki(packs: PackAccess):
  private val pack = cairn.user.pki.Pki(packs)

  export pack.{fragments, language}

  private def hex(bs: Array[Byte]): String = bs.map(b => f"${b & 0xff}%02x").mkString

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

  def anchorCertificateDigest(registry: Module, anchor: String): Either[String, Digest] =
    registry.get(anchor).toRight(s"anchor '$anchor' not in registry")
      .map(t => Artifact(ArtifactKind.Certificate, Cst.toCanon(t)).digest)
