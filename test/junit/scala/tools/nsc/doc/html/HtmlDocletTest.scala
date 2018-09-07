package scala.tools.nsc.doc.html

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


@RunWith(classOf[JUnit4])
class HtmlDocletTest {
  @Test
  def testSyntaxHighlightingUnicode() {
    val in = "unicode: …"

    val out = SyntaxHigh(in).toString

    // scala/bug#9038, this failed with
    // "unicode: …" != "unicode: ￢ﾀﾦ"
    assertEquals(in, out)
  }

  @Test
  def escapeComment(): Unit = {
    val result = SyntaxHigh("// <foo>bar</foo> & ").toString
    val expect = """<span class="cmt">// &lt;foo&gt;bar&lt;/foo&gt; & </span>"""
    assertEquals(expect, result)
  }

  @Test
  def escapeStringLiteral(): Unit = {
    val result = SyntaxHigh(""" " <foo>bar</foo> & " """).toString
    val expect = """ <span class="lit">" &lt;foo&gt;bar&lt;/foo&gt; & "</span> """
    assertEquals(expect, result)
  }
}
