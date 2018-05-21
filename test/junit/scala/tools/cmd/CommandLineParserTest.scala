package scala.tools.cmd

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testing.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class CommandLineParserTest {
  import CommandLineParser.{tokenize, ParseException}

  @Test
  def parserTokenizes(): Unit = {
    assertEquals(Nil, tokenize(""))
    assertEquals(List("x"), tokenize("x"))
    assertEquals(List("x"), tokenize(" x "))
    assertEquals(List("x","y"), tokenize("x y"))
    assertEquals(List("x","y","z"), tokenize("x y z"))
  }
  @Test
  def parserTrims(): Unit = {
    assertEquals(Nil, tokenize(" "))
    assertEquals(List("x"), tokenize(" x "))
    assertEquals(List("x"), tokenize("\nx\n"))
    assertEquals(List("x","y","z"), tokenize(" x y z "))
  }
  @Test
  def parserQuotes(): Unit = {
    assertEquals(List("x"), tokenize("'x'"))
    assertEquals(List("x"), tokenize(""""x""""))
    assertEquals(List("x","y","z"), tokenize("x 'y' z"))
    assertEquals(List("x"," y ","z"), tokenize("x ' y ' z"))
    assertEquals(List("x","y","z"), tokenize("""x "y" z"""))
    assertEquals(List("x"," y ","z"), tokenize("""x " y " z"""))
    // interior quotes
    assertEquals(List("x y z"), tokenize("x' y 'z"))   // was assertEquals(List("x'","y","'z"), tokenize("x' y 'z"))
    assertEquals(List("x\ny\nz"), tokenize("x'\ny\n'z"))
    assertEquals(List("x'y'z"), tokenize("""x"'y'"z"""))
    assertEquals(List("abcxyz"), tokenize(""""abc"xyz"""))
    // missing quotes
    assertThrows[ParseException](tokenize(""""x"""))         // was assertEquals(List("\"x"), tokenize(""""x"""))
    assertThrows[ParseException](tokenize("""x'"""))
  }
}
