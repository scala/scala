package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class DoubleTest {

  /* Test for scala/bug#11386 */
  @Test
  def tesDoubleSignum: Unit = {
    assertTrue(Double.NaN.signum.isNaN)
    assertEquals("1.0", Double.MaxValue.signum.toString)
    assertEquals("1.0", Double.PositiveInfinity.signum.toString)
    assertEquals("-1.0", Double.MinValue.signum.toString)
    assertEquals("-1.0", Double.NegativeInfinity.signum.toString)
    assertEquals("0.0", 0.0.signum.toString)
    assertEquals("-0.0", -0.0.signum.toString)
  }
}
