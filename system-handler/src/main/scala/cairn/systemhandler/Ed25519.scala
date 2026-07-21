package cairn.systemhandler

import cairn.kernel.*
import java.security.{KeyFactory, KeyPairGenerator, PrivateKey, PublicKey, Signature}
import java.security.spec.X509EncodedKeySpec

/** Ed25519 signing (Phase 3 crypto family). Moved from `ledger.Ed25519`. */
object Ed25519:
  def generate(): (PublicKey, PrivateKey) =
    val kp = KeyPairGenerator.getInstance("Ed25519").generateKeyPair()
    (kp.getPublic, kp.getPrivate)

  def publicBytes(pk: PublicKey): Vector[Byte] = pk.getEncoded.toVector

  def sign(sk: PrivateKey, msg: Array[Byte]): Vector[Byte] =
    val s = Signature.getInstance("Ed25519")
    s.initSign(sk); s.update(msg); s.sign().toVector

  val verify: LedgerKernel.Verify = (pkBytes, msg, sig) =>
    try
      val kf = KeyFactory.getInstance("Ed25519")
      val pk = kf.generatePublic(X509EncodedKeySpec(pkBytes.toArray))
      val s = Signature.getInstance("Ed25519")
      s.initVerify(pk); s.update(msg); s.verify(sig.toArray)
    catch case _: Exception => false

final case class Keypair(name: String, publicKey: PublicKey, privateKey: PrivateKey):
  def publicBytes: Vector[Byte] = Ed25519.publicBytes(publicKey)
  def signTx(tx: Tx): SignedTx =
    SignedTx(tx, name, Ed25519.sign(privateKey, Canon.encode(Tx.toCanon(tx))))

object Keypair:
  def dev(name: String): Keypair =
    val (pub, priv) = Ed25519.generate()
    Keypair(name, pub, priv)
