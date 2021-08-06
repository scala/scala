package scala.sys.process

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.AssertUtil.assertThrows

class ParserTest {
  import Parser.{ParseException, tokenize}

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
