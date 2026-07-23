package cairn.tests

import cairn.core.PdfMinimal

class PdfMinimalSuite extends munit.FunSuite:
  test("PdfMinimal emits %PDF header + EOF trailer"):
    val bytes = PdfMinimal.writeText("Acetone", List("SDS REPORT: Acetone", "CAS: 67-64-1", "1. Identification"))
    assert(PdfMinimal.isPdf(bytes))
    val text = new String(bytes, java.nio.charset.StandardCharsets.ISO_8859_1)
    assert(text.contains("%%EOF"))
    assert(text.contains("Acetone"))
    assert(text.contains("/Type /Catalog"))
