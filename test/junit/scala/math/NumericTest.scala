package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.math.Numeric.FloatAsIfIntegral


@RunWith(classOf[JUnit4])
class NumericTest {

  /* Test for SI-8102 */
  @Test
  def testAbs {
    assertTrue(-0.0.abs equals 0.0)
    assertTrue(-0.0f.abs equals 0.0f)
  }
  
  /* Test for SI-9348 */
  @Test
  def testFloatAsIfIntegral {
    val num = scala.math.Numeric.FloatAsIfIntegral
    assertTrue(num.quot(1.0f, 0.5f) equals 2.0f)
    assertTrue(num.quot(1.0f, 0.3f) equals 3.0f)
  }

  /* Test for SI-9348 */
  @Test
  def testDoubleAsIfIntegral {
    val num = scala.math.Numeric.DoubleAsIfIntegral
    assertTrue(num.quot(1.0, 0.25) equals 4.0)
    assertTrue(num.quot(0.5, 0.15) equals 3.0)
  }

  /* Test for SI-9348 */
  @Test
  def testBigDecimalAsIfIntegral {
    val num = scala.math.Numeric.BigDecimalAsIfIntegral
    assertTrue(num.quot(BigDecimal(2.5), BigDecimal(0.5)) equals BigDecimal(5.0))
    assertTrue(num.quot(BigDecimal(5.0), BigDecimal(2.0)) equals BigDecimal(2.0))
  }}

