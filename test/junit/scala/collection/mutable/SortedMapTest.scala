package scala.collection.mutable

import java.util.NoSuchElementException

import org.junit.Assert.{assertEquals, assertNotEquals}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.{Option, Unit}

@RunWith(classOf[JUnit4])
class SortedMapTest {
  @Test
  def testWithDefaultValueReturnsSortedMapWithDefaultValue(): Unit = {
    val tree: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    assertEquals("3 is not present in this map", tree(3))
  }

  @Test
  def testDefaultValueIsPersistedAfterAnElementIsAddedToUnderlyingMap(): Unit = {
    val tree: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    tree.addOne(3 -> "Three")

    assertEquals("5 is not present in this map", tree(5))
  }

  @Test
  def testDefaultValueIsPersistedAfterCreatingEmptyMapFromUnderlyingSortedMap(): Unit = {
    val originalMap: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    val emptyMapWithDefault = originalMap.empty

    assertEquals("1 is not present in this map", emptyMapWithDefault(1))
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
    val originalTreeMap: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 3 -> "Three"))
      .withDefault(defaultValueFunction)

    originalTreeMap.remove(3)

    assertEquals("3 is not present in this map", originalTreeMap(3))
    assertEquals(2, originalTreeMap.size)
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
  def testDefaulValueIsNotPersistedWhenNewMapIterableIsConcatenatedToOriginalMutableMap(): Unit = {
    val originaMap: SortedMap.WithDefault[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefaultValue("element missing")

    val newMap: SortedMap[Int, String] = originaMap ++ SortedMap.from(Map(3 -> "Three")).withDefaultValue("foobar")

    try {
      newMap(4)
    } catch {
      case e: Exception => {
        assertNotEquals("element missing", e.getMessage)
        assertEquals("key not found: 4", e.getMessage)
      }
    }
  }

  private def defaultValueFunction: Int => String = {
    i => s"$i is not present in this map"
  }
}
