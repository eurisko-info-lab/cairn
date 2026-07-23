package cairn.systemhandler

import cairn.kernel.*
import java.nio.file.{Files, Path}
import java.security.{KeyFactory, KeyPairGenerator, PrivateKey, PublicKey, Signature}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}

/** Ed25519 signing (Phase 3 crypto family). Moved from `ledger.Ed25519`. */
object Ed25519:
  def generate(): (PublicKey, PrivateKey) =
    val kp = KeyPairGenerator.getInstance("Ed25519").generateKeyPair()
    (kp.getPublic, kp.getPrivate)

  def publicBytes(pk: PublicKey): Vector[Byte] = pk.getEncoded.toVector

  def privateBytes(sk: PrivateKey): Vector[Byte] = sk.getEncoded.toVector

  def fromEncoded(pub: Vector[Byte], priv: Vector[Byte]): (PublicKey, PrivateKey) =
    val kf = KeyFactory.getInstance("Ed25519")
    (kf.generatePublic(X509EncodedKeySpec(pub.toArray)),
      kf.generatePrivate(PKCS8EncodedKeySpec(priv.toArray)))

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
  def privateBytes: Vector[Byte] = Ed25519.privateBytes(privateKey)
  def signTx(tx: Tx): SignedTx =
    SignedTx(tx, name, Ed25519.sign(privateKey, Canon.encode(Tx.toCanon(tx))))

object Keypair:
  def dev(name: String): Keypair =
    val (pub, priv) = Ed25519.generate()
    Keypair(name, pub, priv)

  def fromEncoded(name: String, pub: Vector[Byte], priv: Vector[Byte]): Keypair =
    val (pk, sk) = Ed25519.fromEncoded(pub, priv)
    Keypair(name, pk, sk)

  def canon(kp: Keypair): Canon = Canon.CTag("keypair", Canon.cmap(
    "name" -> Canon.CStr(kp.name),
    "public" -> Canon.CBytes(kp.publicBytes),
    "private" -> Canon.CBytes(kp.privateBytes)))

  def fromCanon(c: Canon): Either[String, Keypair] =
    import Canon.*
    c match
      case CTag("keypair", m) =>
        (m.field("public"), m.field("private")) match
          case (CBytes(pub), CBytes(priv)) =>
            try Right(fromEncoded(m.field("name").asStr, pub, priv))
            catch case e: Exception => Left(e.getMessage)
          case _ => Left("keypair: bad key material")
      case other => Left(s"not a keypair: $other")

  /** Load or create a durable keypair under `$root/replicas/<name>.canon`. */
  def loadOrCreate(root: Path, name: String): Either[String, Keypair] =
    val dir = root.resolve("replicas")
    val path = dir.resolve(s"$name.canon")
    if Files.exists(path) then
      Canon.decode(Files.readAllBytes(path)).flatMap(fromCanon).flatMap { kp =>
        if kp.name == name then Right(kp)
        else Left(s"keypair name mismatch: file has '${kp.name}', expected '$name'")
      }
    else
      try
        Files.createDirectories(dir)
        val kp = dev(name)
        Files.write(path, Canon.encode(canon(kp)))
        Right(kp)
      catch case e: Exception => Left(e.getMessage)
