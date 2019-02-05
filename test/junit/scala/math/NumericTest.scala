package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class NumericTest {

  /* Test for scala/bug#8102 */
  @Test
  def testAbs: Unit = {
    assertTrue(-0.0.abs equals 0.0)
    assertTrue(-0.0f.abs equals 0.0f)
  }

  /* Test for scala/bug#9348 */
  @Test
  def testBigDecimalAsIfIntegral: Unit = {
    val num = scala.math.Numeric.BigDecimalAsIfIntegral
    assertTrue(num.quot(BigDecimal(2.5), BigDecimal(0.5)) equals BigDecimal(5.0))
    assertTrue(num.quot(BigDecimal(5.0), BigDecimal(2.0)) equals BigDecimal(2.0))
  }}

