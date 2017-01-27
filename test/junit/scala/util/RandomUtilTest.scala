package scala.util

import org.junit.{ Assert, Test }
import scala.util.Random

class RandomUtilTest {
  @Test def testBetween: Unit = {
	val rand = new Random()
	Assert.assertTrue("Random between Int must be inclusive-exclusive", rand.between(0, 1).equals(0))
	Assert.assertTrue("Random between Long must be inclusive-exclusive", rand.between(0L, 1L).equals(0L))
	Assert.assertTrue("Random nextLong must be inclusive-exclusive", rand.nextLong(1L).equals(0L))
	val float: Float = rand.between(0.0f, 1.0f)
	Assert.assertTrue("Float Random should be inclusive-exclusive", 0.0f <= float && float < 1.0f)
	val double: Double = rand.between(0.0f.toDouble, 1.0f.toDouble)
	Assert.assertTrue("Random between Double should be inclusive-exclusive", 0.0f.toDouble <= double && double < 1.0f.toDouble)
  }

  @Test(expected = classOf[IllegalArgumentException]) def testBetweenIntException: Unit = {
	val rand = new Random()
	rand.between(1, 0)
  }


  @Test(expected = classOf[IllegalArgumentException]) def testBetweenLongException: Unit = {
	val rand = new Random()
	rand.between(1L, 0L)
  }

  @Test(expected = classOf[IllegalArgumentException]) def testBetweenFloatException: Unit = {
	val rand = new Random()
	rand.between(1.0f, 0.0f)
  }

  @Test(expected = classOf[IllegalArgumentException]) def testBetweenDoubleException: Unit = {
	val rand = new Random()
	rand.between(1.0f.toDouble, 0.0f.toDouble)
  }

}
