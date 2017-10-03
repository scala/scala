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
    val sortedMap2 = SortedMap(1 -> "d", 2 -> "c")
    val sortedZipped = sortedMap1.zipByKeyWith(sortedMap2)(_ ++ _)
    val sortedZippedT: SortedMap[Int, String] = sortedZipped
    val sortedExpected = SortedMap(1 -> "bd", 2 -> "ac")
    Assert.assertEquals(sortedExpected, sortedZipped)
  }

  @Test
  def joins(): Unit = {
    val map1 = Map(1 -> "a", 2 -> "b")
    val map2 = Map(2 -> "c", 3 -> "d")
    locally {
      val expected = Map(
        1 -> (Some("a"), None),
        2 -> (Some("b"), Some("c")),
        3 -> (None,      Some("d"))
      )
      Assert.assertEquals(expected, map1.fullOuterJoin(map2))
    }
    locally {
      val expected = Map(
        1 -> ("a", None),
        2 -> ("b", Some("c"))
      )
      Assert.assertEquals(expected, map1.leftOuterJoin(map2))
    }
    locally {
      val expected = Map(
        2 -> (Some("b"), "c"),
        3 -> (None,      "d")
      )
      Assert.assertEquals(expected, map1.rightOuterJoin(map2))
    }

    val sortedMap1 = SortedMap(2 -> "a", 1 -> "b")
    val sortedMap2 = SortedMap(2 -> "c", 3 -> "d")
    locally {
      val expected = SortedMap(
        1 -> (Some("b"), None),
        2 -> (Some("a"), Some("c")),
        3 -> (None,      Some("d"))
      )
      val expectedT: SortedMap[Int, (Option[String], Option[String])] = expected
      Assert.assertEquals(expected, sortedMap1.fullOuterJoin(sortedMap2))
    }
  }

}
