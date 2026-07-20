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
  /** Move a branch head to an artifact key. */
  case SetBranchHead(branch: String, head: TypedKey)
  /** Record a policy certificate digest. */
  case RecordCertificate(cert: Digest)

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
    case SetBranchHead(b, h)  => CTag("set-branch-head", Canon.cmap("branch" -> CStr(b), "head" -> keyCanon(h)))
    case RecordCertificate(c) => CTag("record-certificate", CStr(c.hex))

  def fromCanon(c: Canon): Tx = c match
    case CTag("register-identity", m)  => RegisterIdentity(m.field("name").asStr,
      m.field("publicKey") match { case CBytes(v) => v; case other => throw CodecError(s"bad key: $other") })
    case CTag("publish-artifact", k)   => PublishArtifact(keyFrom(k))
    case CTag("set-branch-head", m)    => SetBranchHead(m.field("branch").asStr, keyFrom(m.field("head")))
    case CTag("record-certificate", d) => RecordCertificate(Digest(d.asStr))
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

/** Ledger state: identities, published digests, branch heads, certificates.
  * The state ROOT is the digest of this canonical form (§ Phase 5).
  */
final case class LedgerState(
    identities: Map[String, Vector[Byte]],
    published: Set[String],       // typed-key renders
    heads: Map[String, TypedKey],
    certificates: Set[String],    // digest hex
):
  def canon: Canon = Canon.cmap(
    "identities" -> Canon.CMap(identities.toList.sortBy(_._1).map((n, pk) => n -> Canon.CBytes(pk))),
    "published" -> Canon.cstrs(published.toList.sorted),
    "heads" -> Canon.CMap(heads.toList.sortBy(_._1).map((b, k) =>
      b -> Canon.CStr(k.render))),
    "certificates" -> Canon.cstrs(certificates.toList.sorted))
  def root: Digest = Digest.of(canon)

object LedgerState:
  val genesis: LedgerState = LedgerState(Map.empty, Set.empty, Map.empty, Set.empty)

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

/** Pure kernel transition (S37): signature verification is injected so the
  * kernel stays free of crypto-provider I/O concerns while remaining the sole
  * certifier (§4.6).
  */
object LedgerKernel:
  val genesisParent: Digest = Digest.of(Canon.CStr("cairn-genesis"))

  type Verify = (publicKey: Vector[Byte], message: Array[Byte], signature: Vector[Byte]) => Boolean

  def applyTx(state: LedgerState, stx: SignedTx, verify: Verify): Either[String, LedgerState] =
    // Identity registration may be self-signed (bootstrap); all else needs a known signer.
    val signerKey: Either[String, Vector[Byte]] = stx.tx match
      case Tx.RegisterIdentity(name, pk) if name == stx.signer => Right(pk)
      case _ => state.identities.get(stx.signer).toRight(s"unknown signer '${stx.signer}'")
    signerKey.flatMap { pk =>
      val msg = Canon.encode(Tx.toCanon(stx.tx))
      if !verify(pk, msg, stx.signature) then Left(s"bad signature on tx ${Tx.digest(stx.tx).short} by '${stx.signer}'")
      else stx.tx match
        case Tx.RegisterIdentity(name, key) =>
          state.identities.get(name) match
            case Some(existing) if existing != key => Left(s"identity '$name' already registered with a different key")
            case _ => Right(state.copy(identities = state.identities + (name -> key)))
        case Tx.PublishArtifact(k) =>
          Right(state.copy(published = state.published + k.render))
        case Tx.SetBranchHead(branch, head) =>
          if !state.published.contains(head.render) then
            Left(s"branch '$branch' head ${head.valueHash.short} not published")
          else Right(state.copy(heads = state.heads + (branch -> head)))
        case Tx.RecordCertificate(cert) =>
          Right(state.copy(certificates = state.certificates + cert.hex))
    }

  /** Atomic: a block applies fully or not at all. Verifies parent link,
    * height, authority seal, and the claimed state root.
    */
  def applyBlock(state: LedgerState, expectedParent: Digest, expectedHeight: Long,
                 authorities: Map[String, Vector[Byte]], block: Block, verify: Verify): Either[String, LedgerState] =
    for
      _ <- if block.parent == expectedParent then Right(())
           else Left(s"block ${block.height}: parent ${block.parent.short} != expected ${expectedParent.short}")
      _ <- if block.height == expectedHeight then Right(())
           else Left(s"block height ${block.height} != expected $expectedHeight")
      authKey <- authorities.get(block.authority).toRight(s"unknown authority '${block.authority}'")
      _ <- if verify(authKey, Canon.encode(block.unsealedCanon), block.seal) then Right(())
           else Left(s"invalid authority seal on block ${block.height}")
      after <- block.txs.foldLeft[Either[String, LedgerState]](Right(state)) { (acc, stx) =>
        acc.flatMap(applyTx(_, stx, verify)) }
      _ <- if after.root == block.stateRoot then Right(())
           else Left(s"state root mismatch: computed ${after.root.short}, block claims ${block.stateRoot.short}")
    yield after

  /** Validate a whole chain from genesis. */
  def replay(authorities: Map[String, Vector[Byte]], blocks: List[Block], verify: Verify): Either[String, LedgerState] =
    blocks.zipWithIndex.foldLeft[Either[String, (LedgerState, Digest)]](
      Right((LedgerState.genesis, genesisParent))) {
      case (acc, (block, i)) =>
        acc.flatMap { (st, parent) =>
          applyBlock(st, parent, i.toLong, authorities, block, verify).map(st2 => (st2, block.digest)) }
    }.map(_._1)
