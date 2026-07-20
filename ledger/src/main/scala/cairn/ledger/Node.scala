package cairn.ledger

import cairn.kernel.*
import cairn.workbench.{Cas, DiskCas}
import java.nio.file.{Files, Path}
import java.security.{KeyFactory, KeyPairGenerator, PrivateKey, PublicKey, Signature}
import java.security.spec.X509EncodedKeySpec

/** L5 node machinery (S36, S38–S41): Ed25519 dev keys, PoA sealing, block
  * storage in CAS, publication, and pull-based sync between local nodes.
  * All validation delegates to the pure [[LedgerKernel]] (§4.6).
  */
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

/** A single-node PoA ledger over a CAS directory. Blocks are artifacts in the
  * CAS; `chain` file lists block digests in order (publication metadata only).
  */
final class Node(val root: Path):
  val cas: Cas = DiskCas(root)
  private val chainFile = root.resolve("chain")

  def chainDigests: List[Digest] =
    if !Files.exists(chainFile) then Nil
    else Files.readAllLines(chainFile).toArray.toList.map(l => Digest(l.toString.trim)).filter(_ => true)

  def blocks: Either[String, List[Block]] =
    chainDigests.foldLeft[Either[String, List[Block]]](Right(Nil)) { (acc, d) =>
      acc.flatMap(bs => cas.getByDigest(d).map(a => bs :+ Block.fromCanon(a.body))) }

  def state(authorities: Map[String, Vector[Byte]]): Either[String, LedgerState] =
    blocks.flatMap(LedgerKernel.replay(authorities, _, Ed25519.verify))

  /** Seal and append a block of txs. Atomic under kernel validation. */
  def append(authority: Keypair, authorities: Map[String, Vector[Byte]], txs: List[SignedTx]): Either[String, Block] =
    for
      bs <- blocks
      st <- LedgerKernel.replay(authorities, bs, Ed25519.verify)
      parent = bs.lastOption.map(_.digest).getOrElse(LedgerKernel.genesisParent)
      height = bs.length.toLong
      after <- txs.foldLeft[Either[String, LedgerState]](Right(st)) { (acc, stx) =>
        acc.flatMap(LedgerKernel.applyTx(_, stx, Ed25519.verify)) }
      unsealed = Block(height, parent, txs, after.root, authority.name, Vector.empty)
      seal = Ed25519.sign(authority.privateKey, Canon.encode(unsealed.unsealedCanon))
      block = unsealed.copy(seal = seal)
      // kernel gate: re-validate the sealed block before committing
      _ <- LedgerKernel.applyBlock(st, parent, height, authorities, block, Ed25519.verify)
    yield
      cas.put(block.artifact)
      Files.createDirectories(root)
      Files.writeString(chainFile, (chainDigests :+ block.digest).map(_.hex).mkString("", "\n", "\n"))
      block

object Sync:
  /** Pull-based blob sync (S41): copy blocks + published artifact bodies the
    * consumer is missing, by digest. Returns fetched digests.
    */
  def pull(from: Node, to: Node, authorities: Map[String, Vector[Byte]]): Either[String, List[Digest]] =
    for
      theirBlocks <- from.blocks
      _ <- LedgerKernel.replay(authorities, theirBlocks, Ed25519.verify) // verify before adopting
      st <- from.state(authorities)
    yield
      val fetched = List.newBuilder[Digest]
      def fetch(d: Digest): Unit =
        if !to.cas.contains(d) then
          from.cas.getBytes(d).foreach { bs => to.cas.putBytes(bs); fetched += d }
      theirBlocks.foreach(b => fetch(b.digest))
      // materialize published artifact bodies by digest
      st.published.foreach { render =>
        render.split(":") match
          case Array(_, value, _) => Digest.parse(value).foreach(fetch)
          case _                  => () }
      Files.writeString(to.root.resolve("chain"), theirBlocks.map(_.digest.hex).mkString("", "\n", "\n"))
      fetched.result()

  /** Divergence surfacing (S42): competing heads are reported, never merged
    * silently. Two chains diverge at the first differing block.
    */
  enum Comparison:
    case Same
    case Ahead(by: Int)            // `other` extends `mine`
    case Behind(by: Int)           // `mine` extends `other`
    case Diverged(atHeight: Int, mineHead: Digest, otherHead: Digest)

  def compare(mine: List[Digest], other: List[Digest]): Comparison =
    val common = mine.zip(other).takeWhile(_ == _).length
    if common == mine.length && common == other.length then Comparison.Same
    else if common == mine.length then Comparison.Ahead(other.length - common)
    else if common == other.length then Comparison.Behind(mine.length - common)
    else Comparison.Diverged(common, mine.last, other.last)
