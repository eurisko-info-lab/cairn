package cairn.systemhandler

import cairn.kernel.*
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.{MessageDigest, SecureRandom}
import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.{GCMParameterSpec, PBEKeySpec, SecretKeySpec}

/** Signing capability without exposing raw private-key material at call sites. */
trait Signer:
  def name: String
  def publicBytes: Vector[Byte]
  def sign(msg: Array[Byte]): Vector[Byte]

/** Boundary for durable private keys.
  *
  * Rules:
  * - Create only when `$root/replicas/<name>.canon` is absent.
  * - Never overwrite on decrypt, decode, or permission failure.
  * - Refuse plaintext by default (`CAIRN_KEYSTORE_PLAINTEXT=1` for lab only).
  * - Encrypted form (`keypair-sealed`) requires `CAIRN_KEYSTORE_SECRET`.
  * - New seals use PBKDF2-HMAC-SHA256 (salt + iterations); legacy
    *   SHA-256(secret) files still decrypt when `kdf` is absent.
  */
object Keystore:
  private val GcmNonceBytes = 12
  private val GcmTagBits = 128
  private val SaltBytes = 16
  private val DefaultKdf = "pbkdf2-hmac-sha256"
  private val DefaultIterations = 210_000
  private val LegacyKdf = "sha256"
  private val random = new SecureRandom()

  def envSecret: Option[Array[Byte]] =
    Option(System.getenv("CAIRN_KEYSTORE_SECRET")).filter(_.nonEmpty).map(_.getBytes("UTF-8"))

  def allowPlaintext: Boolean =
    Option(System.getenv("CAIRN_KEYSTORE_PLAINTEXT")).exists(v =>
      v == "1" || v.equalsIgnoreCase("true"))

  /** Derive a 256-bit AES key. Legacy path is unsalted SHA-256(secret). */
  private def deriveKey(
      secret: Array[Byte],
      kdf: String,
      salt: Array[Byte],
      iterations: Int,
  ): Either[String, Array[Byte]] =
    kdf match
      case `LegacyKdf` | "sha-256" =>
        Right(MessageDigest.getInstance("SHA-256").digest(secret))
      case `DefaultKdf` =>
        if salt.isEmpty then Left("keystore: pbkdf2 requires salt")
        else if iterations < 10_000 then Left(s"keystore: iterations $iterations too low")
        else
          try
            val chars = new String(secret, StandardCharsets.UTF_8).toCharArray
            val spec = new PBEKeySpec(chars, salt, iterations, 256)
            val skf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
            Right(skf.generateSecret(spec).getEncoded)
          catch case e: Exception =>
            Left(s"keystore: KDF failed: ${e.getMessage}")
      case other => Left(s"keystore: unknown kdf '$other'")

  private def encryptPrivate(
      secret: Array[Byte],
      priv: Array[Byte],
  ): Either[String, (Array[Byte], Array[Byte], Array[Byte], Int)] =
    val salt = new Array[Byte](SaltBytes)
    random.nextBytes(salt)
    val iterations = DefaultIterations
    deriveKey(secret, DefaultKdf, salt, iterations).map { key =>
      val nonce = new Array[Byte](GcmNonceBytes)
      random.nextBytes(nonce)
      val cipher = Cipher.getInstance("AES/GCM/NoPadding")
      cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"),
        new GCMParameterSpec(GcmTagBits, nonce))
      (salt, nonce, cipher.doFinal(priv), iterations)
    }

  private def decryptPrivate(
      secret: Array[Byte],
      kdf: String,
      salt: Array[Byte],
      iterations: Int,
      nonce: Array[Byte],
      ct: Array[Byte],
  ): Either[String, Array[Byte]] =
    deriveKey(secret, kdf, salt, iterations).flatMap { key =>
      try
        val cipher = Cipher.getInstance("AES/GCM/NoPadding")
        cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"),
          new GCMParameterSpec(GcmTagBits, nonce))
        Right(cipher.doFinal(ct))
      catch case e: Exception =>
        Left(s"keystore: decrypt failed (wrong secret?): ${e.getMessage}")
    }

  def path(root: Path, name: String): Path =
    root.resolve("replicas").resolve(s"$name.canon")

  def load(root: Path, name: String): Either[String, Keypair] =
    load(root, name, envSecret)

  def load(root: Path, name: String, secret: Option[Array[Byte]]): Either[String, Keypair] =
    val p = path(root, name)
    if !Files.exists(p) then Left(s"missing replica keypair for '$name' at $p")
    else
      try
        Canon.decode(Files.readAllBytes(p)).flatMap(c => fromCanon(c, secret)).flatMap { kp =>
          if kp.name == name then Right(kp)
          else Left(s"keypair name mismatch: file has '${kp.name}', expected '$name'")
        }
      catch
        case e: java.nio.file.AccessDeniedException =>
          Left(s"keystore: permission denied reading '$name': ${e.getMessage}")
        case e: Exception =>
          Left(s"keystore: failed to load '$name': ${e.getMessage}")

  /** Load existing identity, or create once when the path is absent.
    *
    * If the path exists but cannot be decrypted/decoded, the error is returned
    * — a new key is never written over a broken or inaccessible file.
    */
  def loadOrCreate(root: Path, name: String): Either[String, Keypair] =
    loadOrCreate(root, name, envSecret)

  def loadOrCreate(root: Path, name: String, secret: Option[Array[Byte]]): Either[String, Keypair] =
    val p = path(root, name)
    if Files.exists(p) then load(root, name, secret)
    else
      for
        _ <- encodePolicy(secret)
        kp = Keypair.dev(name)
        _ <- saveCreate(root, kp, secret)
      yield kp

  /** Create-only persist. Refuses if the path already exists. */
  def save(root: Path, kp: Keypair): Either[String, Unit] =
    saveCreate(root, kp, envSecret)

  def saveCreate(root: Path, kp: Keypair, secret: Option[Array[Byte]]): Either[String, Unit] =
    for
      body <- toCanonE(kp, secret)
      _ <-
        try
          Files.createDirectories(root.resolve("replicas"))
          DurableIo.writeCreateOnly(path(root, kp.name), Canon.encode(body))
        catch case e: Exception => Left(e.getMessage)
    yield ()

  def toCanon(kp: Keypair): Canon =
    toCanonE(kp, envSecret).fold(e => throw IllegalStateException(e), identity)

  def toCanon(kp: Keypair, secret: Option[Array[Byte]]): Canon =
    toCanonE(kp, secret).fold(e => throw IllegalStateException(e), identity)

  def toCanonE(kp: Keypair, secret: Option[Array[Byte]]): Either[String, Canon] =
    secret match
      case Some(sec) =>
        encryptPrivate(sec, kp.privateBytes.toArray).map { (salt, nonce, ct, iterations) =>
          Canon.CTag("keypair-sealed", Canon.cmap(
            "name" -> Canon.CStr(kp.name),
            "public" -> Canon.CBytes(kp.publicBytes),
            "kdf" -> Canon.CStr(DefaultKdf),
            "salt" -> Canon.CBytes(salt.toVector),
            "iterations" -> Canon.CInt(iterations),
            "privateNonce" -> Canon.CBytes(nonce.toVector),
            "privateCiphertext" -> Canon.CBytes(ct.toVector)))
        }
      case None if allowPlaintext =>
        Right(Keypair.canon(kp))
      case None =>
        Left(
          "keystore: refusing plaintext key material — set CAIRN_KEYSTORE_SECRET " +
            "or CAIRN_KEYSTORE_PLAINTEXT=1 for lab-only plaintext")

  private def encodePolicy(secret: Option[Array[Byte]]): Either[String, Unit] =
    secret match
      case Some(_) => Right(())
      case None if allowPlaintext => Right(())
      case None =>
        Left(
          "keystore: refusing plaintext key material — set CAIRN_KEYSTORE_SECRET " +
            "or CAIRN_KEYSTORE_PLAINTEXT=1 for lab-only plaintext")

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
                val fields = m.asMap
                val kdf = fields.get("kdf").map(_.asStr).getOrElse(LegacyKdf)
                val salt = fields.get("salt") match
                  case Some(CBytes(bs)) => bs.toArray
                  case _                => Array.emptyByteArray
                val iterations = fields.get("iterations").map(_.asInt.toInt).getOrElse(0)
                decryptPrivate(sec, kdf, salt, iterations, nonce.toArray, ct.toArray).map { priv =>
                  Keypair.fromEncoded(m.field("name").asStr, pub, priv.toVector)
                }
              case _ => Left("keypair-sealed: bad fields")
      case CTag("keypair", _) =>
        if allowPlaintext then Keypair.fromCanon(c)
        else
          Left(
            "keystore: refusing plaintext keypair — set CAIRN_KEYSTORE_PLAINTEXT=1 " +
              "or re-provision as keypair-sealed")
      case other => Left(s"not a keypair: $other")
