package scala.tools.nsc.transform

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ParamMatchTest {
  // order of parameters is important for a failing test, as the parameter matcher for "total" takes the first
  // constructor parameter that was either "total" or started with "total$"
  class Invoice(val `total tax`: Int, val total: Int)

  @Test
  def testMatch: Unit = {
    val invoice = new Invoice(20, 120)
    assertEquals("invalid parameter copy in constructor", 120, invoice.total)
  }
}
