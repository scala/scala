package scala.tools.nsc.doc.html

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StringLiteralTest {
  @Test
  def testHighlightingQuote() {
    val in = "\""
    val out = SyntaxHigh(in).map(_.toText).mkString
    assertEquals("""<span class="lit">"</span>""", out)
  }

  @Test
  def testHighlightingDoubleQuotes() {
    val in = "\"content\""
    val out = SyntaxHigh(in).map(_.toText).mkString
    assertEquals("""<span class="lit">"content"</span>""", out)
  }

  @Test
  def testHighlightingQuoteFollowingBackslash() {
    val in = "\\\""
    val out = SyntaxHigh(in).map(_.toText).mkString
    assertEquals("""\<span class="lit">"</span>""", out)
  }

  @Test
  def testHighlightingQuotesIgnoringEscapedQuote() {
    val in = "\"\\\"\""
    val out = SyntaxHigh(in).map(_.toText).mkString
    assertEquals("""<span class="lit">"\""</span>""", out)
  }

  @Test
  def testHighlightingTripleQuotes() {
    val in = "\"\"\""
    val out = SyntaxHigh(in).map(_.toText).mkString
    assertEquals("<span class=\"lit\">\"\"\"</span>", out)
  }

  @Test
  def testHighlightingRawStringLiteralIgnoringQuote() {
    val in = "\"\"\"content\"\"content\"\"\""
    val out = SyntaxHigh(in).map(_.toText).mkString
    assertEquals("<span class=\"lit\">\"\"\"content\"\"content\"\"\"</span>", out)
  }
}
