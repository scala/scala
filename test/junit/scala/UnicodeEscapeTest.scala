package scala

import org.junit.Assert._
import org.junit.Test

import scala.tools.testing.RunTesting

class UnicodeEscapeTest extends RunTesting {
  @Test def singleQuotedEscapes: Unit = {
    assertEquals("abc def ghi jkl mno pqr", "\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072")
    assertEquals("\n", "\u000a")
    assertEquals("\n", "\u000A") 
  }

  //deprecated
  @Test def tripleQuotedEscapes: Unit = {
    assertEquals("abc def ghi jkl mno pqr", """\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072""")
    assertEquals("\n", """\u000a""")
    assertEquals("\n", """\u000A""")
  }

  @Test def sInterpoletorSingleQuote: Unit = {
    assertEquals("abc def ghi jkl mno pqr", s"\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072")
    assertEquals("\n", s"\u000a")
    assertEquals("\n", s"\u000A") 
  }

  @Test def sInterpoletorTripleQuote: Unit = {
    assertEquals("abc def ghi jkl mno pqr", s"""\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072""")
    assertEquals("\n", s"""\u000a""")
    assertEquals("\n", s"""\u000A""") 
  }

  @Test def rawInterpoletorSingleQuote: Unit = {
    assertEquals("abc def ghi jkl mno pqr", raw"\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072")
    assertEquals("\n", raw"\u000a")
    assertEquals("\n", raw"\u000A") 
  }

  @Test def rawInterpoletorTripleQuote: Unit = {
    assertEquals("abc def ghi jkl mno pqr", raw"""\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072""")
    assertEquals("\n", raw"""\u000a""")
    assertEquals("\n", raw"""\u000A""") 
  }

  @Test def fInterpoletorSingleQuote: Unit = {
    assertEquals("abc def ghi jkl mno pqr", f"\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072")
    assertEquals("\n", f"\u000a")
    assertEquals("\n", f"\u000A") 
  }

  @Test def fInterpoletorTripleQuote: Unit = {
    assertEquals("abc def ghi jkl mno pqr", f"""\u0061\u0062\u0063 \u0064\u0065\u0066 \u0067\u0068\u0069 \u006a\u006b\u006c \u006d\u006e\u006f \u0070\u0071\u0072""")
    assertEquals("\n", f"""\u000a""")
    assertEquals("\n", f"""\u000A""") 
  }
}