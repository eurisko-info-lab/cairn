package cairn.kernel

/** L0 ledger transition relation (S35–S38) — PURE, no I/O (§3).
  *
  * The ledger is the publication spine, not a runtime DB (§4.9): it records
  * artifact digests, branch heads, identities and policy certificates — never
  * artifact bodies (those live in CAS).
  */
enum Tx:
  /** Register a signer identity (name -> Ed25519 public key bytes). */
  case RegisterIdentity(name: String, publicKey: Vector[Byte])
  /** Publish an artifact digest (body must be fetchable from CAS). */
  case PublishArtifact(key: TypedKey)
  /** Move a branch head to an artifact key; `certRef` cites a recorded
    * certificate when the branch policy demands one (M37).
    */
  case SetBranchHead(branch: String, head: TypedKey, certRef: Option[Digest] = None)
  /** Record a certificate digest with its certification method (M37). */
  case RecordCertificate(cert: Digest, method: String = "unspecified")
  /** M36: quorum-governed authority-set changes. Approvals are signatures by
    * CURRENT authorities over the operation payload.
    */
  case AddAuthority(name: String, publicKey: Vector[Byte], approvals: List[(String, Vector[Byte])])
  case RemoveAuthority(name: String, approvals: List[(String, Vector[Byte])])
  /** M37: set the policy term governing a branch. */
  case SetPolicy(branch: String, policy: Cst)

object Tx:
  import Canon.*
  private def keyCanon(k: TypedKey): Canon = Canon.cmap(
    "kind" -> CStr(k.kind.name), "value" -> CStr(k.valueHash.hex), "type" -> CStr(k.typeHash.hex))
  private def keyFrom(c: Canon): TypedKey = TypedKey(
    ArtifactKind.parse(c.field("kind").asStr).fold(m => throw CodecError(m), identity),
    Digest(c.field("value").asStr), Digest(c.field("type").asStr))

  def toCanon(tx: Tx): Canon = tx match
    case RegisterIdentity(n, pk) => CTag("register-identity", Canon.cmap(
      "name" -> CStr(n), "publicKey" -> CBytes(pk)))
    case PublishArtifact(k)   => CTag("publish-artifact", keyCanon(k))
    case SetBranchHead(b, h, c) => CTag("set-branch-head", Canon.cmap(
      "branch" -> CStr(b), "head" -> keyCanon(h),
      "certRef" -> c.fold(CTag("none", CInt(0)))(d => CTag("some", CStr(d.hex)))))
    case RecordCertificate(c, m) => CTag("record-certificate", Canon.cmap(
      "cert" -> CStr(c.hex), "method" -> CStr(m)))
    case AddAuthority(n, pk, aps) => CTag("add-authority", Canon.cmap(
      "name" -> CStr(n), "publicKey" -> CBytes(pk),
      "approvals" -> CList(aps.map((s, sig) => Canon.cmap("signer" -> CStr(s), "sig" -> CBytes(sig))))))
    case RemoveAuthority(n, aps) => CTag("remove-authority", Canon.cmap(
      "name" -> CStr(n),
      "approvals" -> CList(aps.map((s, sig) => Canon.cmap("signer" -> CStr(s), "sig" -> CBytes(sig))))))
    case SetPolicy(b, p) => CTag("set-policy", Canon.cmap(
      "branch" -> CStr(b), "policy" -> Cst.toCanon(p)))

  /** Payload an authority approval signs (excludes the approvals themselves). */
  def approvalPayload(tx: Tx): Array[Byte] = tx match
    case AddAuthority(n, pk, _) => Canon.encode(CTag("approve-add", Canon.cmap(
      "name" -> CStr(n), "publicKey" -> CBytes(pk))))
    case RemoveAuthority(n, _)  => Canon.encode(CTag("approve-remove", CStr(n)))
    case other                  => Canon.encode(toCanon(other))

  def fromCanon(c: Canon): Tx = c match
    case CTag("register-identity", m)  => RegisterIdentity(m.field("name").asStr,
      m.field("publicKey") match { case CBytes(v) => v; case other => throw CodecError(s"bad key: $other") })
    case CTag("publish-artifact", k)   => PublishArtifact(keyFrom(k))
    case CTag("set-branch-head", m)    => SetBranchHead(m.field("branch").asStr, keyFrom(m.field("head")),
      m.field("certRef") match { case CTag("some", CStr(h)) => Some(Digest(h)); case _ => None })
    case CTag("record-certificate", m) => RecordCertificate(Digest(m.field("cert").asStr), m.field("method").asStr)
    case CTag("add-authority", m) => AddAuthority(m.field("name").asStr,
      m.field("publicKey") match { case CBytes(v) => v; case o => throw CodecError(s"bad key: $o") },
      m.field("approvals").asList.map(a => (a.field("signer").asStr,
        a.field("sig") match { case CBytes(v) => v; case o => throw CodecError(s"bad sig: $o") })))
    case CTag("remove-authority", m) => RemoveAuthority(m.field("name").asStr,
      m.field("approvals").asList.map(a => (a.field("signer").asStr,
        a.field("sig") match { case CBytes(v) => v; case o => throw CodecError(s"bad sig: $o") })))
    case CTag("set-policy", m) => SetPolicy(m.field("branch").asStr, Cst.fromCanon(m.field("policy")))
    case other => throw CodecError(s"not a tx: $other")

  def digest(tx: Tx): Digest = Digest.of(toCanon(tx))

/** A transaction signed by a registered identity. */
final case class SignedTx(tx: Tx, signer: String, signature: Vector[Byte]):
  def canon: Canon = Canon.cmap(
    "tx" -> Tx.toCanon(tx), "signer" -> Canon.CStr(signer), "signature" -> Canon.CBytes(signature))

object SignedTx:
  def fromCanon(c: Canon): SignedTx =
    import Canon.*
    SignedTx(Tx.fromCanon(c.field("tx")), c.field("signer").asStr,
      c.field("signature") match { case CBytes(v) => v; case o => throw CodecError(s"bad sig: $o") })

/** A PoA block: hash-linked, sealed by an authority signature over the block
  * canonical form (minus the seal).
  */
final case class Block(
    height: Long,
    parent: Digest,        // parent block digest (genesis parent = digest of "genesis")
    txs: List[SignedTx],
    stateRoot: Digest,     // state root AFTER applying txs
    authority: String,
    seal: Vector[Byte],    // signature over unsealedCanon
):
  def unsealedCanon: Canon = Canon.cmap(
    "height" -> Canon.CInt(height),
    "parent" -> Canon.CStr(parent.hex),
    "txs" -> Canon.CList(txs.map(_.canon)),
    "stateRoot" -> Canon.CStr(stateRoot.hex),
    "authority" -> Canon.CStr(authority))
  def canon: Canon = Canon.cmap(
    "block" -> unsealedCanon, "seal" -> Canon.CBytes(seal))
  def artifact: Artifact = Artifact(ArtifactKind.Block, canon)
  def digest: Digest = artifact.digest

object Block:
  def fromCanon(c: Canon): Block =
    import Canon.*
    val b = c.field("block")
    Block(
      height = b.field("height").asInt,
      parent = Digest(b.field("parent").asStr),
      txs = b.field("txs").asList.map(SignedTx.fromCanon),
      stateRoot = Digest(b.field("stateRoot").asStr),
      authority = b.field("authority").asStr,
      seal = c.field("seal") match { case CBytes(v) => v; case o => throw CodecError(s"bad seal: $o") })
