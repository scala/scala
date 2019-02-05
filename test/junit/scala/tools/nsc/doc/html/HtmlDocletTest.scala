package scala.tools.nsc.doc.html

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class HtmlDocletTest {
  @Test
  def testSyntaxHighlightingUnicode(): Unit = {
    val in = "unicode: …"

    val out = HtmlTags.textOf(SyntaxHigh(in))

    // scala/bug#9038, this failed with
    // "unicode: …" != "unicode: ￢ﾀﾦ"
    assertEquals(in, out)
  }

  @Test
  def escapeComment(): Unit = {
    val result = HtmlTags.textOf(SyntaxHigh("// <foo>bar</foo> & "))
    val expect = """<span class="cmt">// &lt;foo&gt;bar&lt;/foo&gt; & </span>"""
    assertEquals(expect, result)
  }

  @Test
  def escapeStringLiteral(): Unit = {
    val result = HtmlTags.textOf(SyntaxHigh(""" " <foo>bar</foo> & " """))
    val expect = """<span class="lit">" &lt;foo&gt;bar&lt;/foo&gt; & "</span>"""
    assertEquals(expect, result)
  }
}
