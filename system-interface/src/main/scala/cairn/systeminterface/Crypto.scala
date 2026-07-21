package cairn.systeminterface

/** Effect family: cryptographic operations (Phase 3). Key material never
  * crosses into Kernel/Core — only opaque handles and public bytes.
  */
object Crypto:
  opaque type KeyHandle = String
  object KeyHandle:
    def apply(id: String): KeyHandle = id
    extension (k: KeyHandle) def id: String = k

  enum Request:
    case GenerateEd25519(name: String)
    case GenerateX25519(name: String)
    case Sign(key: KeyHandle, message: Array[Byte])
    case Verify(publicKey: Vector[Byte], message: Array[Byte], signature: Vector[Byte])
    case Seal(plaintext: Array[Byte], recipients: List[(String, Vector[Byte])])
    case Open(envelopeCanon: Array[Byte], recipientKeyId: String, key: KeyHandle)

  enum Response:
    case KeyPair(handle: KeyHandle, publicKey: Vector[Byte])
    case Signature(bytes: Vector[Byte])
    case Verified(ok: Boolean)
    case Sealed(envelopeBytes: Array[Byte])
    case Plaintext(bytes: Array[Byte])

  enum Error:
    case UnknownKey(handle: KeyHandle)
    case CryptoFailure(message: String)
