package scala.collection.immutable

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MapTest {

  @Test
  // Test for bug/#11017
  // Compile-time test only
  def testMapWithDefaultReassignableToMap(): Unit = {
    var map = Map.empty[Int, Int].withDefaultValue(0)
    map = map + (1 -> 1)
  }


}
