package scala.math

import java.lang.Double.doubleToLongBits
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class DoubleTest {

  /* Test for scala/bug#11386 */
  @Test
  def testDoubleSign(): Unit = {
    assertTrue(Double.NaN.sign.isNaN)
    assertEquals(doubleToLongBits(1.0), doubleToLongBits(Double.MaxValue.sign))
    assertEquals(doubleToLongBits(1.0), doubleToLongBits(Double.PositiveInfinity.sign))
    assertEquals(doubleToLongBits(-1.0), doubleToLongBits(Double.MinValue.sign))
    assertEquals(doubleToLongBits(-1.0), doubleToLongBits(Double.NegativeInfinity.sign))
    assertEquals(doubleToLongBits(0.0), doubleToLongBits(0.0.sign))
    assertEquals(doubleToLongBits(-0.0), doubleToLongBits(-0.0.sign))
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testDoubleSignum(): Unit = {
    assertEquals(0, Double.NaN.signum)
    assertEquals(1, Double.MaxValue.signum)
    assertEquals(1, Double.PositiveInfinity.signum)
    assertEquals(-1, Double.MinValue.signum)
    assertEquals(-1, Double.NegativeInfinity.signum)
    assertEquals(0, 0.0.signum)
    assertEquals(0, -0.0.signum)
  }
}
