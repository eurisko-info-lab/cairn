package cairn.examples.pki

import cairn.kernel.*
import cairn.ledger.{Ed25519, Encryption, Keypair, Node}
import java.security.KeyPair

/** Demo PKI hierarchy on par with GRANITE `authorities/DemoPki`: root →
  * intermediate → leaf signing chain, plus a distinct X25519 encryption
  * certificate for confidential SDS composition sealing.
  */
object DemoPki:
  final case class Issued(
      name: String,
      signing: Keypair,
      cert: Cst,
      encryption: Option[KeyPair] = None,
      encCert: Option[Cst] = None
  )

  final case class Hierarchy(
      root: Issued,
      intermediate: Issued,
      leaf: Issued,
      regulatorEnc: Issued
  ):
    def registry: Cst =
      PkiMax.registryCtx(List(
        root.name -> root.cert,
        intermediate.name -> intermediate.cert,
        leaf.name -> leaf.cert,
        regulatorEnc.name -> regulatorEnc.cert))

    def allNames: List[String] =
      List(root.name, intermediate.name, leaf.name, regulatorEnc.name)

  /** Build a fresh hierarchy with validity windows covering `now`. */
  def hierarchy(now: Long = 1000L, window: Long = 10000L): Hierarchy =
    val rootKp = Keypair.dev("root")
    val interKp = Keypair.dev("inter")
    val leafKp = Keypair.dev("leaf")
    val regSign = Keypair.dev("regulator")
    val regEnc = Encryption.generateKeyPair()
    val root = Issued("root", rootKp, PkiMax.certTerm("root", rootKp, rootKp, now - window, now + window))
    val inter = Issued("inter", interKp, PkiMax.certTerm("inter", interKp, rootKp, now - window, now + window))
    val leaf = Issued("leaf", leafKp, PkiMax.certTerm("leaf", leafKp, interKp, now - window / 2, now + window / 2))
    // Encryption cert: subject key is X25519 (stored as base64), still signed by Ed25519 issuer.
    val encKeyHex = Encryption.encodePublicKey(regEnc.getPublic)
    val encPayload = PkiMax.signedPayload("regulator-enc", encKeyHex)
    val encSig = Ed25519.sign(interKp.privateKey, encPayload)
    val encCert = Cst.node(
      "cert",
      Cst.Leaf("regulator-enc"),
      Cst.Leaf(encKeyHex),
      Cst.Leaf("inter"),
      Cst.Leaf(encSig.map(b => f"${b & 0xff}%02x").mkString),
      Cst.Leaf((now - window).toString),
      Cst.Leaf((now + window).toString))
    val regulator = Issued(
      "regulator-enc",
      regSign,
      encCert,
      encryption = Some(regEnc),
      encCert = Some(encCert))
    Hierarchy(root, inter, leaf, regulator)

/** End-to-end PKI tutorial matching GRANITE `PkiTutorial`: issue → validate →
  * revoke → tamper → publish trust anchor on the PoA ledger.
  */
object PkiTutorial:
  final case class Report(
      issuedNames: List[String],
      validationBeforeRevoke: Boolean,
      validationAfterRevoke: Boolean,
      validationAfterTamper: Boolean,
      trustAnchorDigest: Digest,
      ledgerHeads: Set[String],
      encryptionOpenOk: Boolean
  )

  def run(work: java.nio.file.Path): Report =
    val now = 1000L
    val h = DemoPki.hierarchy(now)
    val reg0 = h.registry
    val before = PkiMax.validate(reg0, "leaf", now, Set("root")).isRight

    // Revoke intermediate via CRL — leaf chain must break
    val crl = PkiMax.Crl.issue(h.root.signing, List("inter"))
    val revoked = PkiMax.applyCrl(reg0, crl)
    val afterRevoke = PkiMax.validate(revoked, "leaf", now, Set("root")).isLeft

    // Tamper: forge leaf signature while claiming intermediate as issuer
    val forgedLeaf = PkiMax.certTerm("leaf", h.leaf.signing, h.root.signing.copy(name = "inter"), 0, 2000)
    val tampered = PkiMax.registryCtx(List(
      "root" -> h.root.cert,
      "inter" -> h.intermediate.cert,
      "leaf" -> forgedLeaf,
      "regulator-enc" -> h.regulatorEnc.cert))
    val afterTamper = PkiMax.validate(tampered, "leaf", now, Set("root")).isLeft

    // Publish trust-anchor certificate digest on ledger
    val node = Node(work)
    val alice = h.root.signing
    val auth = Map(alice.name -> alice.publicBytes)
    val anchorArt = Artifact(ArtifactKind.Certificate, Cst.toCanon(h.root.cert))
    node.cas.put(anchorArt)
    node.append(alice, auth, List(
      alice.signTx(Tx.RegisterIdentity(alice.name, alice.publicBytes)),
      alice.signTx(Tx.PublishArtifact(anchorArt.key)),
      alice.signTx(Tx.SetBranchHead("trust-anchor:root", anchorArt.key)))).fold(e => throw RuntimeException(e), identity)
    val heads = node.state(auth).fold(e => throw RuntimeException(e), _.heads.keySet)

    // Encryption sealing smoke: seal to regulator X25519, open with matching key
    val encKp = h.regulatorEnc.encryption.get
    val env = Encryption.seal("secret-pct|42.5".getBytes("UTF-8"), List("regulator" -> encKp.getPublic))
    val opened = Encryption.open(env, "regulator", encKp.getPrivate).exists(bs => new String(bs, "UTF-8") == "secret-pct|42.5")
    val wrongOpen = Encryption.open(env, "regulator", Encryption.generateKeyPair().getPrivate).isEmpty

    Report(
      issuedNames = h.allNames,
      validationBeforeRevoke = before,
      validationAfterRevoke = afterRevoke,
      validationAfterTamper = afterTamper,
      trustAnchorDigest = anchorArt.digest,
      ledgerHeads = heads,
      encryptionOpenOk = opened && wrongOpen)
