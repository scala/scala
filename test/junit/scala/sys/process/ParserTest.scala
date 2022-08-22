package scala.sys.process

import org.junit.Assert._
import org.junit.Test
import scala.tools.testkit.AssertUtil.assertThrows

class ParserTest {
  import Parser.{tokenize, ParseException}

  private def check(tokens: String*)(input: String): Unit = assertEquals(tokens, tokenize(input))

  private def checkFails(input: String, output: String): Unit = {
    var txt: String = null
    val res = tokenize(input, msg => txt = msg)
    assertTrue(s"Expected bad tokenization for [$input] but result was [$res]", txt ne null)
    assertEquals(output, txt)
  }

  @Test def parserTokenizes(): Unit = {
    check()("")
    check("x")("x")
    check("x")(" x ")
    check("x", "y")("x y")
    check("x", "y", "z")("x y z")
  }
  @Test def parserTrims(): Unit = {
    check()(" ")
    check("x")(" x ")
    check("x")("\nx\n")
    check("x", "y", "z")(" x y z ")
  }
  @Test def parserQuotes(): Unit = {
    check("x")("'x'")
    check("x")(""""x"""")
    check("x", "y", "z")("x 'y' z")
    check("x", " y ", "z")("x ' y ' z")
    check("x", "y", "z")("""x "y" z""")
    check("x", " y ", "z")("""x " y " z""")
    // interior quotes
    check("x y z")("x' y 'z")   // was assertEquals(List("x'","y","'z"), tokenize("x' y 'z"))
    check("x\ny\nz")("x'\ny\n'z")
    check("x'y'z")("""x"'y'"z""")
    check("abcxyz")(""""abc"xyz""")
    // missing quotes
    checkFails(""""x""", "Unmatched quote [0](\")")  // was assertEquals(List("\"x"), tokenize(""""x"""))
    checkFails("""x'""", "Unmatched quote [1](')")
    assertThrows[ParseException](tokenize(""""x""")) // was assertEquals(List("\"x"), tokenize(""""x"""))
    assertThrows[ParseException](tokenize("""x'"""))
  }
  @Test def `leading space is skipped`: Unit = check("text")(" text")
  @Test def `leading quote is escaped`: Unit = {
    check("echo", "hello, world!")("""echo "hello, world!" """)
    check("echo", "hello, world!")("""echo hello,' 'world! """)
    check("echo", """"hello,""", """world!"""")("""echo \"hello, world!\" """)
    check("""a"b"c""")("""a\"b\"c""")
    check("a", "'b", "'", "c")("""a \'b \' c""")
    check("a", """\b """, "c")("""a \\'b ' c""")
  }
  /* backslash is stripped in normal shell usage.
  ➜  ~ ls \"hello world\"
  ls: cannot access '"hello': No such file or directory
  ls: cannot access 'world"': No such file or directory
  */
  @Test def `escaped quotes lose backslash`: Unit = {
    check("ls", "\"hello", "world\"")("""ls \"hello world\"""")
  }
}
