package strawman.collection.immutable

import org.junit.Assert.{assertEquals, assertNotEquals}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SortedMapTest {
  @Test
  def testWithDefaultValueReturnsSortedMapWithDeaultValue(): Unit = {
    val tree: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    assertEquals("3 is not present in this map", tree(3))
  }

  @Test
  def testDefaultValueIsPersistedAfterAnElementIsAddedToUnderlyingMap(): Unit = {
    val tree: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two")).withDefault(defaultValueFunction)
    val newTree = tree + (3 -> "Three")

    assertEquals("5 is not present in this map", newTree(5))
  }

  @Test
  def testDefaultValueIsPersistedAfterCreatingEmptyMapFromUnderlyingSortedMap(): Unit = {
    val emptyMap: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)
      .empty

    assertEquals("1 is not present in this map", emptyMap(1))
  }

  @Test
  def testDefaultValueIsPersistedAfterRangeOperationOnSortedMap(): Unit = {
    val rangedProjectOfTreeMap: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 3 -> "Three"))
      .withDefault(defaultValueFunction)
      .range(1, 3) // range `to` parameter is not inclusive

    assertEquals("3 is not present in this map", rangedProjectOfTreeMap(3))
  }

  @Test
  def testDefaultValueIsPersistedAfterAnElementFromUnderlyingMapIsRemoved(): Unit = {
    val originalTreeMap : SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 3 -> "Three"))
      .withDefault(defaultValueFunction)

    val newTreeMap = originalTreeMap - 3

    assertEquals("3 is not present in this map", newTreeMap(3))
    assertEquals(2, newTreeMap.size)
  }

  @Test
  def testDefaultValueIsLostWhenNewSortedMapIsCreatedFromIterablesInOperationsLikeFlatMap(): Unit = {
    val originalMap: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    val newTreeMap: SortedMap[String, Int] = originalMap.flatMap((kv: (Int, String)) => TreeMap(kv._2 -> kv._1))

    try {
      newTreeMap("Three")
    } catch {
      case e: Exception => {
        assertNotEquals("Three is not present in this map", e.getMessage)
        assertEquals("key not found: Three", e.getMessage)
      }
    }
  }

  @Test
  def testDefaultValueIsPersistedWhenNewSortedMapIsCreatedFromSpecificIterableInOperationsLikeFilter(): Unit = {
    val evenNumbers: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 4 -> "Four", 5 -> "Five"))
      .withDefault(defaultValueFunction)
      .filter((kv: (Int, String)) => kv._1 % 2 == 0)

    assertEquals("5 is not present in this map", evenNumbers(5))
  }

  @Test
  def testDefaultValueIsPersistedWhenNewSpecificBuilderIsCreatedFromSortedMapInOperationsLikeGroupBy(): Unit = {
    val numbers: Map[String, SortedMap.WithDefault[Int, String]] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 4 -> "Four", 5 -> "Five"))
      .withDefault(defaultValueFunction)
      .groupBy((kv: (Int, String)) => if (kv._1 % 2 == 0) "even" else "odd")

    assertEquals("6 is not present in this map", numbers("even")(6))
    assertEquals("3 is not present in this map", numbers("odd")(3))
  }

  @Test
  def testDefaulValueIsPersistedWhenNewMapIterableIsConcatenatedToOriginalMap(): Unit = {
    val originaMap: SortedMap.WithDefault[Int, String] = TreeMap(1 -> "One", 2 -> "Two")
      .withDefaultValue("element missing")
    val newMap: SortedMap[Int, String] = originaMap ++ Map(3 -> "Three")

    assertEquals("element missing", newMap(4))
  }

  private def defaultValueFunction: Int => String = {
    i => s"$i is not present in this map"
  }
}
