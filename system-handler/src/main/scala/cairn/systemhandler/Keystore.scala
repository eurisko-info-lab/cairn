package cairn.systemhandler

import cairn.kernel.*
import java.nio.file.{Files, Path}
import java.security.{MessageDigest, SecureRandom}
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

/** Signing capability without exposing raw private-key material at call sites. */
trait Signer:
  def name: String
  def publicBytes: Vector[Byte]
  def sign(msg: Array[Byte]): Vector[Byte]

/** Boundary for durable private keys: load/save and optional encryption at rest.
  *
  * File layout under `$root/replicas/<name>.canon`:
  * - `keypair-sealed` — AES-256-GCM ciphertext of PKCS8 private bytes (preferred)
  * - `keypair` — legacy plaintext (readable only when [[allowPlaintext]] is true)
  *
  * Encryption key: SHA-256 of `CAIRN_KEYSTORE_SECRET` when set; otherwise new
  * keys are written plaintext only if `CAIRN_KEYSTORE_PLAINTEXT=1` (dev/tests).
  */
object Keystore:
  private val GcmNonceBytes = 12
  private val GcmTagBits = 128
  private val random = new SecureRandom()

  def envSecret: Option[Array[Byte]] =
    Option(System.getenv("CAIRN_KEYSTORE_SECRET")).filter(_.nonEmpty).map(_.getBytes("UTF-8"))

  def allowPlaintext: Boolean =
    Option(System.getenv("CAIRN_KEYSTORE_PLAINTEXT")).exists(v =>
      v == "1" || v.equalsIgnoreCase("true"))

  private def aesKey(secret: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(secret)

  private def encryptPrivate(secret: Array[Byte], priv: Array[Byte]): (Array[Byte], Array[Byte]) =
    val nonce = new Array[Byte](GcmNonceBytes)
    random.nextBytes(nonce)
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(aesKey(secret), "AES"),
      new GCMParameterSpec(GcmTagBits, nonce))
    (nonce, cipher.doFinal(priv))

  private def decryptPrivate(secret: Array[Byte], nonce: Array[Byte], ct: Array[Byte]): Array[Byte] =
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(aesKey(secret), "AES"),
      new GCMParameterSpec(GcmTagBits, nonce))
    cipher.doFinal(ct)

  def path(root: Path, name: String): Path =
    root.resolve("replicas").resolve(s"$name.canon")

  def load(root: Path, name: String): Either[String, Keypair] =
    val p = path(root, name)
    if !Files.exists(p) then Left(s"missing replica keypair for '$name' at $p")
    else
      Canon.decode(Files.readAllBytes(p)).flatMap(fromCanon).flatMap { kp =>
        if kp.name == name then Right(kp)
        else Left(s"keypair name mismatch: file has '${kp.name}', expected '$name'")
      }

  def loadOrCreate(root: Path, name: String): Either[String, Keypair] =
    load(root, name).orElse {
      val kp = Keypair.dev(name)
      save(root, kp).map(_ => kp)
    }

  def save(root: Path, kp: Keypair): Either[String, Unit] =
    try
      Files.createDirectories(root.resolve("replicas"))
      DurableIo.writeAtomic(path(root, kp.name), Canon.encode(toCanon(kp)))
    catch case e: Exception => Left(e.getMessage)

  def toCanon(kp: Keypair): Canon = toCanon(kp, envSecret)

  def toCanon(kp: Keypair, secret: Option[Array[Byte]]): Canon =
    secret match
      case Some(sec) =>
        val (nonce, ct) = encryptPrivate(sec, kp.privateBytes.toArray)
        Canon.CTag("keypair-sealed", Canon.cmap(
          "name" -> Canon.CStr(kp.name),
          "public" -> Canon.CBytes(kp.publicBytes),
          "privateNonce" -> Canon.CBytes(nonce.toVector),
          "privateCiphertext" -> Canon.CBytes(ct.toVector)))
      case None if allowPlaintext || envSecret.isEmpty =>
        Keypair.canon(kp)
      case None =>
        Keypair.canon(kp)

  def fromCanon(c: Canon): Either[String, Keypair] =
    fromCanon(c, envSecret)

  def fromCanon(c: Canon, secret: Option[Array[Byte]]): Either[String, Keypair] =
    import Canon.*
    c match
      case CTag("keypair-sealed", m) =>
        secret match
          case None =>
            Left("keystore: encrypted keypair requires CAIRN_KEYSTORE_SECRET")
          case Some(sec) =>
            (m.field("public"), m.field("privateNonce"), m.field("privateCiphertext")) match
              case (CBytes(pub), CBytes(nonce), CBytes(ct)) =>
                try
                  val priv = decryptPrivate(sec, nonce.toArray, ct.toArray).toVector
                  Right(Keypair.fromEncoded(m.field("name").asStr, pub, priv))
                catch case e: Exception => Left(s"keystore decrypt failed: ${e.getMessage}")
              case _ => Left("keypair-sealed: bad fields")
      case CTag("keypair", _) =>
        if !allowPlaintext && secret.isDefined then
          Left("keystore: refusing plaintext keypair while CAIRN_KEYSTORE_SECRET is set")
        else Keypair.fromCanon(c)
      case other => Left(s"not a keypair: $other")
