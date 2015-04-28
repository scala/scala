package scala.math

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/** Test methods in the scala.math package object. */
@RunWith(classOf[JUnit4])
class PackageTest {

  @Test
  def testLogN(): Unit = {
    /* the base 10 logarithm of 100 */
    assertEquals(2.0, logN(10.0, 100.0), 0.00000001)

    /* the base 2 logarithm of 8 */
    assertEquals(3.0, logN(2.0, 8.0), 0.00000001)

    /* the base 2 logarithm of 3 */
    assertEquals(1.5849625, logN(2.0, 3.0), 0.00000001)

    /* the base 2 logarithm of 2/3 */
    assertEquals(-0.5849625, logN(2.0, 2.0 / 3.0), 0.00000001)
  }
}
