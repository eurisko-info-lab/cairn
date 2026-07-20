package cairn.surface

import cairn.kernel.*

/** Tiny JSON helpers — no external deps (Cairn host rule). */
object Json:
  def esc(s: String): String =
    val b = StringBuilder()
    b += '"'
    var i = 0
    while i < s.length do
      s.charAt(i) match
        case '"'  => b ++= "\\\""
        case '\\' => b ++= "\\\\"
        case '\n' => b ++= "\\n"
        case '\r' => b ++= "\\r"
        case '\t' => b ++= "\\t"
        case c if c < 0x20 => b ++= f"\\u${c.toInt}%04x"
        case c => b += c
      i += 1
    b += '"'
    b.result()

  def obj(fields: (String, String)*): String =
    fields.map((k, v) => s"${esc(k)}:$v").mkString("{", ",", "}")

  def arr(items: Iterable[String]): String = items.mkString("[", ",", "]")

  def str(s: String): String = esc(s)
  def num(n: Long): String = n.toString
  def bool(b: Boolean): String = if b then "true" else "false"
  def nul: String = "null"

  def ofCanon(c: Canon): String = c match
    case Canon.CInt(v)     => num(v)
    case Canon.CStr(v)     => str(v)
    case Canon.CBytes(v)   => obj("bytes" -> str(v.map(b => f"${b & 0xff}%02x").mkString))
    case Canon.CList(xs)   => arr(xs.map(ofCanon))
    case Canon.CMap(es)    => obj(es.map((k, v) => k -> ofCanon(v))*)
    case Canon.CTag(t, v)  => obj("tag" -> str(t), "value" -> ofCanon(v))

  def ofCst(c: Cst): String = c match
    case Cst.Leaf(t)       => obj("leaf" -> str(t))
    case Cst.Node(ctor, kids) => obj("node" -> str(ctor), "kids" -> arr(kids.map(ofCst)))
