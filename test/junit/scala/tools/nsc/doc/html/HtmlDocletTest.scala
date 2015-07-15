package scala.tools.nsc.doc.html

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class HtmlDocletTest {
  @Test
  def testSyntaxHighlightingUnicode() {
    val in = "unicode: …"

    val out = SyntaxHigh(in).toString

    // SI-9038, this failed with
    // "unicode: …" != "unicode: ￢ﾀﾦ"
    assertEquals(in, out)
  }
}
