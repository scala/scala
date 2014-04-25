

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class NumericTest {

  /* Test for SI-8102 */
  @Test
  def testAbs {
    assertTrue(-0.0.abs equals 0.0)
    assertTrue(-0.0f.abs equals 0.0f)
  }
  
  /* Tests for SI-8518 and SI-4370 regressions */
  @Test
  def testDoubleRangeAccuracy {
    assertTrue((0.0 to 1.0 by 0.1).drop(3).take(3).length == 3)
    assertTrue((0.0 to 0.5 by 0.1).map(_.toString) == (0 to 5).map(x => s"0.$x"))
    assertFalse((0.0 until 0.2 by 0.5).isEmpty)
    assertTrue((0.0 until 0.3 by 0.1).length == 3)
  }
}

