package scala.util

import org.junit.{Assert, Test}
import scala.tools.testkit.AssertUtil.assertThrows

class RandomUtilTest {
  @Test def testBetween(): Unit = {
    val rand = new Random()
    Assert.assertTrue("Random between Int must be inclusive-exclusive", rand.between(0, 1).equals(0))
    Assert.assertTrue("Random between Long must be inclusive-exclusive", rand.between(0L, 1L).equals(0L))
    Assert.assertTrue("Random nextLong must be inclusive-exclusive", rand.nextLong(1L).equals(0L))
    val float: Float = rand.between(0.0f, 1.0f)
    Assert.assertTrue("Float Random should be inclusive-exclusive", 0.0f <= float && float < 1.0f)
    val double: Double = rand.between(0.0f.toDouble, 1.0f.toDouble)
    Assert.assertTrue("Random between Double should be inclusive-exclusive", 0.0f.toDouble <= double && double < 1.0f.toDouble)
  }

  @Test def testBetweenIntException(): Unit = assertThrows[IllegalArgumentException] {
    val rand = new Random()
    rand.between(1, 0)
  }

  @Test def testBetweenLongException(): Unit = assertThrows[IllegalArgumentException] {
    val rand = new Random()
    rand.between(1L, 0L)
  }

  @Test def testBetweenFloatException(): Unit = assertThrows[IllegalArgumentException] {
    val rand = new Random()
    rand.between(1.0f, 0.0f)
  }

  @Test def testBetweenDoubleException(): Unit = assertThrows[IllegalArgumentException] {
    val rand = new Random()
    rand.between(1.0f.toDouble, 0.0f.toDouble)
  }
}
