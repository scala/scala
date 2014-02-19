

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
  
  def iAmA(i: Int) = "Int"
  def iAmA(l: Long) = "Long"
  def iAmA(f: Float) = "Float"
  def iAmA(d: Double) = "Double"
  
  @Test
  def testSI3235 {
    assert(123456789.round == 123456789)
    assert(1234567890123456789L.round == 1234567890123456789L)
    assert(iAmA((2.4f).rint) == "Float")
    assert(iAmA((2.4).rint) == "Double")
    assert(math.round(123456789) == 123456789)
    assert(math.round(1234567890123456789L) == 1234567890123456789L)
    assert(iAmA(math.rint(2.4)) == "Double")
  }
}

