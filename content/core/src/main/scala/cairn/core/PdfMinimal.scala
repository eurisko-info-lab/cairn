package cairn.core

/** Minimal PDF 1.4 writer — pure, zero deps.
  *
  * Emits a single-page text PDF suitable as an SDS report **projection**
  * (bytes SDS workflows attach / RoundTrip against). Not a full PDF toolkit:
  * Helvetica, one page, Latin-1-ish escapes, no fonts/images/encryption.
  */
object PdfMinimal:
  /** Escape a string for a PDF literal `(...)`. */
  def escapeLit(s: String): String =
    s.flatMap {
      case '\\' => "\\\\"
      case '('  => "\\("
      case ')'  => "\\)"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case c if c >= 32 && c < 127 => c.toString
      case c => f"\\${c.toInt}%03o"
    }

  /** Build a one-page PDF whose body lines are shown top-down in Helvetica 10. */
  def writeText(title: String, lines: List[String]): Array[Byte] =
    val safeTitle = escapeLit(title.take(120))
    val contentLines = lines.take(60).zipWithIndex.map { (line, i) =>
      val y = 800 - i * 14
      s"BT /F1 10 Tf 50 $y Td (${escapeLit(line.take(90))}) Tj ET"
    }
    val stream = contentLines.mkString("\n")
    val streamBytes = stream.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)

    // Object layout: 1=Catalog, 2=Pages, 3=Page, 4=Font, 5=Content, 6=Info
    val objs = scala.collection.mutable.ArrayBuffer.empty[Array[Byte]]
    def add(s: String): Int =
      objs += s.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
      objs.length // 1-based id after add

    add("<< /Type /Catalog /Pages 2 0 R >>")
    add("<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
    add("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 5 0 R /Resources << /Font << /F1 4 0 R >> >> >>")
    add("<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
    add(s"<< /Length ${streamBytes.length} >>\nstream\n$stream\nendstream")
    add(s"<< /Title ($safeTitle) /Producer (cairn PdfMinimal) >>")

    val out = new java.io.ByteArrayOutputStream()
    val header = "%PDF-1.4\n%\u00e2\u00e3\u00cf\u00d3\n"
    out.write(header.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1))
    val offsets = new Array[Int](objs.length + 1)
    offsets(0) = 0
    var i = 0
    while i < objs.length do
      offsets(i + 1) = out.size
      val id = i + 1
      val body = objs(i)
      out.write(s"$id 0 obj\n".getBytes(java.nio.charset.StandardCharsets.US_ASCII))
      out.write(body)
      out.write("\nendobj\n".getBytes(java.nio.charset.StandardCharsets.US_ASCII))
      i += 1
    val xrefAt = out.size
    val sb = StringBuilder()
    sb ++= s"xref\n0 ${objs.length + 1}\n"
    sb ++= "0000000000 65535 f \n"
    i = 1
    while i <= objs.length do
      sb ++= f"${offsets(i)}%010d 00000 n \n"
      i += 1
    sb ++= s"trailer\n<< /Size ${objs.length + 1} /Root 1 0 R /Info 6 0 R >>\n"
    sb ++= s"startxref\n$xrefAt\n%%EOF\n"
    out.write(sb.result().getBytes(java.nio.charset.StandardCharsets.US_ASCII))
    out.toByteArray

  def isPdf(bytes: Array[Byte]): Boolean =
    bytes.length >= 8 && new String(bytes.take(8), java.nio.charset.StandardCharsets.US_ASCII).startsWith("%PDF-1.")
