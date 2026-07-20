package cairn.kernel

import java.io.{ByteArrayOutputStream, DataOutputStream}
import java.nio.charset.StandardCharsets.UTF_8

/** L0 canonical value model (S2).
  *
  * Deterministic bytes: same semantic value => same bytes => same digest (§4.12).
  * Normalization rules:
  *   - integers: signed 64-bit, big-endian
  *   - strings: UTF-8 bytes, 4-byte big-endian length prefix
  *   - lists: 4-byte big-endian count, then items
  *   - maps: entries sorted by unsigned-lexicographic UTF-8 key order; duplicate keys rejected
  *   - tags: tag string then single value
  */
enum Canon:
  case CInt(v: Long)
  case CStr(v: String)
  case CBytes(v: Vector[Byte])
  case CList(items: List[Canon])
  case CMap(entries: List[(String, Canon)]) // always kept sorted; build via Canon.cmap
  case CTag(tag: String, value: Canon)

object Canon:
  import Canon.*

  private def keyLt(a: String, b: String): Boolean =
    val (x, y) = (a.getBytes(UTF_8), b.getBytes(UTF_8))
    val n = math.min(x.length, y.length)
    var i = 0
    while i < n do
      val (xa, ya) = (x(i) & 0xff, y(i) & 0xff)
      if xa != ya then return xa < ya
      i += 1
    x.length < y.length

  /** Smart constructor: sorts entries, rejects duplicate keys. */
  def cmap(entries: (String, Canon)*): Canon =
    val ks = entries.map(_._1)
    require(ks.distinct.sizeIs == ks.size, s"duplicate map keys: ${ks.diff(ks.distinct).mkString(",")}")
    CMap(entries.toList.sortWith((a, b) => keyLt(a._1, b._1)))

  def clist(items: Canon*): Canon = CList(items.toList)
  def cstrs(items: List[String]): Canon = CList(items.map(CStr.apply))

  def encode(c: Canon): Array[Byte] =
    val bo = ByteArrayOutputStream()
    val o = DataOutputStream(bo)
    def str(s: String): Unit =
      val bs = s.getBytes(UTF_8); o.writeInt(bs.length); o.write(bs)
    def go(c: Canon): Unit = c match
      case CInt(v)   => o.writeByte('I'); o.writeLong(v)
      case CStr(v)   => o.writeByte('S'); str(v)
      case CBytes(v) => o.writeByte('B'); o.writeInt(v.length); o.write(v.toArray)
      case CList(xs) => o.writeByte('L'); o.writeInt(xs.length); xs.foreach(go)
      case CMap(es) =>
        val sorted = es.sortWith((a, b) => keyLt(a._1, b._1))
        o.writeByte('M'); o.writeInt(sorted.length)
        sorted.foreach { (k, v) => str(k); go(v) }
      case CTag(t, v) => o.writeByte('T'); str(t); go(v)
    go(c); o.flush(); bo.toByteArray

  def decode(bs: Array[Byte]): Either[String, Canon] =
    var i = 0
    def fail(msg: String): Nothing = throw new RuntimeException(s"canon decode at $i: $msg")
    def byte(): Int = { if i >= bs.length then fail("eof"); val b = bs(i) & 0xff; i += 1; b }
    def int(): Int = { val v = (byte() << 24) | (byte() << 16) | (byte() << 8) | byte(); v }
    def long(): Long = (int().toLong << 32) | (int().toLong & 0xffffffffL)
    def str(): String =
      val n = int()
      if n < 0 || i + n > bs.length then fail("bad string length")
      val s = new String(bs, i, n, UTF_8); i += n; s
    def go(): Canon = byte() match
      case 'I' => CInt(long())
      case 'S' => CStr(str())
      case 'B' => val n = int(); if n < 0 || i + n > bs.length then fail("bad bytes length")
                  val v = bs.slice(i, i + n).toVector; i += n; CBytes(v)
      case 'L' => val n = int(); CList(List.fill(n)(go()))
      case 'M' => val n = int(); CMap(List.fill(n)((str(), go())))
      case 'T' => CTag(str(), go())
      case t   => fail(s"unknown tag byte $t")
    try
      val c = go()
      if i != bs.length then Left(s"trailing bytes after canon value ($i of ${bs.length})") else Right(c)
    catch case e: RuntimeException => Left(e.getMessage)

  // -- small accessor helpers used by codecs throughout the codebase --
  extension (c: Canon)
    def asStr: String = c match { case CStr(s) => s; case _ => throw CodecError(s"expected string, got $c") }
    def asInt: Long = c match { case CInt(v) => v; case _ => throw CodecError(s"expected int, got $c") }
    def asList: List[Canon] = c match { case CList(xs) => xs; case _ => throw CodecError(s"expected list, got $c") }
    def asMap: Map[String, Canon] = c match { case CMap(es) => es.toMap; case _ => throw CodecError(s"expected map, got $c") }
    def field(k: String): Canon =
      c.asMap.getOrElse(k, throw CodecError(s"missing field '$k' in $c"))
    def untag(expected: String): Canon = c match
      case CTag(t, v) if t == expected => v
      case CTag(t, _)                  => throw CodecError(s"expected tag '$expected', got '$t'")
      case _                           => throw CodecError(s"expected tagged value '$expected', got $c")

final case class CodecError(msg: String) extends RuntimeException(msg)
