package strawman.collection
package decorators

import org.junit.{Assert, Test}

class MapDecoratorTest {

  @Test
  def zipByKeyWith(): Unit = {
    val map1 = Map(1 -> "a", 2 -> "b")
    val map2 = Map(2 -> "c")
    val zipped = map1.zipByKeyWith(map2)(_ ++ _)
    val expected = Map(2 -> "bc")
    Assert.assertEquals(expected, zipped)

    val sortedMap1 = SortedMap(2 -> "a", 1 -> "b")
    val sortedMap2 = Map(1 -> "d", 2 -> "c")
    val sortedZipped = sortedMap1.zipByKeyWith(sortedMap2)(_ ++ _)
    val sortedZippedT: SortedMap[Int, String] = sortedZipped
    val sortedExpected = SortedMap(1 -> "bd", 2 -> "ac")
    Assert.assertEquals(sortedExpected, sortedZipped)
  }

}
