package scala.collection.immutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class VectorTest {
  /**
   * Test Vector ++ with a small parallel collection concatenation (SI-9072).
   *
   */
  @Test
  def testPlusPlus(): Unit = {
    val smallVec = (0 to 1)
    val smallParVec = smallVec.par
    val testElementsSize = (0 to 1000).map( _ => Vector.empty ++ smallParVec )
    Assert.assertTrue(testElementsSize.forall( v => v.size == 2 ))
  }
}
