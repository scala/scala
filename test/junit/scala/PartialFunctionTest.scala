package scala

import org.junit.Assert._
import org.junit.Test

class PartialFunctionTest {

  import PartialFunction.{cond, condOpt}

  @Test
  def `cond evaluates pf`(): Unit = {
    assertTrue(cond("x") { case "x" => true })
  }

  @Test
  def `cond evaluates default`(): Unit = {
    assertFalse(cond("z") { case "x" => true })
  }

  @Test
  def `condOpt evaluates pf`(): Unit = {
    assertEquals(Some("y"), condOpt("x") { case "x" => "y" })
    assertEquals(Some(null), condOpt("x") { case "x" => null case "z" => "y" })
  }

  @Test
  def `condOpt evaluates default`(): Unit = {
    assertEquals(None, condOpt("z") { case "x" => "y" })
  }
}
