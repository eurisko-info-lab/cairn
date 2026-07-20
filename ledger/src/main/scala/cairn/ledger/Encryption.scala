package cairn.ledger

import java.security.{KeyFactory, KeyPair, KeyPairGenerator, PrivateKey, PublicKey, SecureRandom}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import javax.crypto.{Cipher, KeyAgreement, Mac}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import java.util.Base64

/** L5 hybrid envelope encryption — on par with GRANITE `sharing/encryption`.
  *
  * X25519 ECDH + HKDF-SHA256 + AES-256-GCM multi-recipient sealing. Distinct
  * from Ed25519 signing keys: encryption certificates use X25519 subject keys
  * (see PKI KeyEncipherment). Used by SDS composition sealing so confidential
  * ingredients are recoverable only by holders of matching private keys.
  */
object Encryption:
  private val AesKeyBytes = 32
  private val GcmNonceBytes = 12
  private val GcmTagBits = 128
  private val random = new SecureRandom()

  def generateKeyPair(): KeyPair = KeyPairGenerator.getInstance("X25519").generateKeyPair()

  def encodePublicKey(pub: PublicKey): String = Base64.getEncoder.encodeToString(pub.getEncoded)
  def decodePublicKey(base64: String): PublicKey =
    KeyFactory.getInstance("X25519").generatePublic(X509EncodedKeySpec(Base64.getDecoder.decode(base64)))

  def encodePrivateKey(priv: PrivateKey): String = Base64.getEncoder.encodeToString(priv.getEncoded)
  def decodePrivateKey(base64: String): PrivateKey =
    KeyFactory.getInstance("X25519").generatePrivate(PKCS8EncodedKeySpec(Base64.getDecoder.decode(base64)))

  final case class WrappedKey(recipientKeyId: String, wrapNonceBase64: String, wrappedKeyBase64: String)

  final case class SealedEnvelope(
      ephemeralPublicKeyBase64: String,
      nonceBase64: String,
      ciphertextBase64: String,
      wrappedKeys: List[WrappedKey]
  ):
    def recipientIds: Set[String] = wrappedKeys.map(_.recipientKeyId).toSet
    def canon: cairn.kernel.Canon =
      import cairn.kernel.Canon
      Canon.CTag("sealed-envelope", Canon.cmap(
        "ephemeral" -> Canon.CStr(ephemeralPublicKeyBase64),
        "nonce" -> Canon.CStr(nonceBase64),
        "ciphertext" -> Canon.CStr(ciphertextBase64),
        "wrapped" -> Canon.CList(wrappedKeys.map(w => Canon.cmap(
          "id" -> Canon.CStr(w.recipientKeyId),
          "wrapNonce" -> Canon.CStr(w.wrapNonceBase64),
          "wrappedKey" -> Canon.CStr(w.wrappedKeyBase64))))))

  /** Seal plaintext so only holders of a matching recipient private key can open it. */
  def seal(plaintext: Array[Byte], recipients: List[(String, PublicKey)]): SealedEnvelope =
    require(recipients.nonEmpty, "seal requires at least one recipient")
    val contentKey = randomBytes(AesKeyBytes)
    val nonce = randomBytes(GcmNonceBytes)
    val ciphertext = aesGcmEncrypt(contentKey, nonce, plaintext)
    val ephemeralKeyPair = generateKeyPair()
    val wrappedKeys = recipients.map { (recipientKeyId, recipientPublicKey) =>
      val sharedSecret = ecdh(ephemeralKeyPair.getPrivate, recipientPublicKey)
      val wrapKey = hkdf(sharedSecret, info = s"cairn-envelope:$recipientKeyId")
      val wrapNonce = randomBytes(GcmNonceBytes)
      val wrappedKeyBytes = aesGcmEncrypt(wrapKey, wrapNonce, contentKey)
      WrappedKey(
        recipientKeyId,
        Base64.getEncoder.encodeToString(wrapNonce),
        Base64.getEncoder.encodeToString(wrappedKeyBytes))
    }
    SealedEnvelope(
      encodePublicKey(ephemeralKeyPair.getPublic),
      Base64.getEncoder.encodeToString(nonce),
      Base64.getEncoder.encodeToString(ciphertext),
      wrappedKeys)

  /** Fail-closed open: wrong audience and wrong key both yield None. */
  def open(envelope: SealedEnvelope, recipientKeyId: String, recipientPrivateKey: PrivateKey): Option[Array[Byte]] =
    envelope.wrappedKeys.find(_.recipientKeyId == recipientKeyId).flatMap { wk =>
      try
        val ephemeralPublicKey = decodePublicKey(envelope.ephemeralPublicKeyBase64)
        val sharedSecret = ecdh(recipientPrivateKey, ephemeralPublicKey)
        val wrapKey = hkdf(sharedSecret, info = s"cairn-envelope:$recipientKeyId")
        val wrapNonce = Base64.getDecoder.decode(wk.wrapNonceBase64)
        val wrappedKeyBytes = Base64.getDecoder.decode(wk.wrappedKeyBase64)
        val contentKey = aesGcmDecrypt(wrapKey, wrapNonce, wrappedKeyBytes)
        val nonce = Base64.getDecoder.decode(envelope.nonceBase64)
        val ciphertext = Base64.getDecoder.decode(envelope.ciphertextBase64)
        Some(aesGcmDecrypt(contentKey, nonce, ciphertext))
      catch case _: Exception => None
    }

  private def ecdh(privateKey: PrivateKey, publicKey: PublicKey): Array[Byte] =
    val ka = KeyAgreement.getInstance("X25519")
    ka.init(privateKey)
    ka.doPhase(publicKey, true)
    ka.generateSecret()

  private def hkdf(ikm: Array[Byte], info: String, length: Int = AesKeyBytes): Array[Byte] =
    val prk = hmacSha256(new Array[Byte](32), ikm)
    val infoBytes = info.getBytes("UTF-8")
    var t = Array.emptyByteArray
    var okm = Array.emptyByteArray
    var counter = 1
    while okm.length < length do
      t = hmacSha256(prk, t ++ infoBytes ++ Array(counter.toByte))
      okm = okm ++ t
      counter += 1
    okm.take(length)

  private def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(key, "HmacSHA256"))
    mac.doFinal(data)

  private def aesGcmEncrypt(key: Array[Byte], nonce: Array[Byte], plaintext: Array[Byte]): Array[Byte] =
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"), new GCMParameterSpec(GcmTagBits, nonce))
    cipher.doFinal(plaintext)

  private def aesGcmDecrypt(key: Array[Byte], nonce: Array[Byte], ciphertext: Array[Byte]): Array[Byte] =
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"), new GCMParameterSpec(GcmTagBits, nonce))
    cipher.doFinal(ciphertext)

  private def randomBytes(n: Int): Array[Byte] =
    val bytes = new Array[Byte](n)
    random.nextBytes(bytes)
    bytes
