package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class DoubleSignumTest {

  /* Test for scala/bug#11386 */
  @Test
  def tesDoubleSignumNaN: Unit = {
    assertTrue(Double.NaN.signum.isNaN)
    assertTrue(Float.NaN.signum.isNaN)
  }
}

