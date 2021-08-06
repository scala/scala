package scala.util

import org.junit.jupiter.api.{Assertions, Test}
import scala.tools.testkit.AssertUtil.assertThrows

class RandomUtilTest {
  @Test def testBetween(): Unit = {
    val rand = new Random()
    Assertions.assertTrue(rand.between(0, 1).equals(0), "Random between Int must be inclusive-exclusive")
    Assertions.assertTrue(rand.between(0L, 1L).equals(0L), "Random between Long must be inclusive-exclusive")
    Assertions.assertTrue(rand.nextLong(1L).equals(0L), "Random nextLong must be inclusive-exclusive")
    val float: Float = rand.between(0.0f, 1.0f)
    Assertions.assertTrue(0.0f <= float && float < 1.0f, "Float Random should be inclusive-exclusive")
    val double: Double = rand.between(0.0f.toDouble, 1.0f.toDouble)
    Assertions.assertTrue(0.0f.toDouble <= double && double < 1.0f.toDouble, "Random between Double should be inclusive-exclusive")
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
