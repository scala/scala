package scala.collection
package mutable

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.testkit.AssertUtil.assertThrows

class SortedMapTest {
  @Test
  def testWithDefaultValueReturnsSortedMapWithDefaultValue(): Unit = {
    val tree: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    assertEquals("3 is not present in this map", tree(3))
  }

  @Test
  def testDefaultValueIsPersistedAfterAnElementIsAddedToUnderlyingMap(): Unit = {
    val tree: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    tree.addOne(3 -> "Three")

    assertEquals("5 is not present in this map", tree(5))
  }

  @Test
  def testDefaultValueIsPersistedAfterCreatingEmptyMapFromUnderlyingSortedMap(): Unit = {
    val originalMap: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    val emptyMapWithDefault = originalMap.empty

    assertEquals("1 is not present in this map", emptyMapWithDefault(1))
  }

  @Test
  def testDefaultValueIsPersistedAfterRangeOperationOnSortedMap(): Unit = {
    val rangedProjectOfTreeMap: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 3 -> "Three"))
      .withDefault(defaultValueFunction)
      .range(1, 3) // range `to` parameter is not inclusive

    assertEquals("3 is not present in this map", rangedProjectOfTreeMap(3))
  }

  @Test
  def testDefaultValueIsPersistedAfterAnElementFromUnderlyingMapIsRemoved(): Unit = {
    val originalTreeMap: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 3 -> "Three"))
      .withDefault(defaultValueFunction)

    originalTreeMap.remove(3)

    assertEquals("3 is not present in this map", originalTreeMap(3))
    assertEquals(2, originalTreeMap.size)
  }

  @Test
  def testDefaultValueIsLostWhenNewSortedMapIsCreatedFromIterablesInOperationsLikeFlatMap(): Unit = {
    val originalMap: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    val newTreeMap: SortedMap[String, Int] = originalMap.flatMap((kv: (Int, String)) => TreeMap(kv._2 -> kv._1))

    assertThrows[NoSuchElementException](newTreeMap("Three"), _ == "key not found: Three")
  }

  @Test
  def `default function survives filter`: Unit = {
    val evenNumbers: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 4 -> "Four", 5 -> "Five"))
      .withDefault(defaultValueFunction)
      .filter((kv: (Int, String)) => kv._1 % 2 == 0)

    assertEquals("5 is not present in this map", evenNumbers(5))
  }

  @Test
  def `default value survives concat`: Unit = {
    val originalMap: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefaultValue("element missing")

    val newMap: SortedMap[Int, String] = originalMap ++ SortedMap.from(Map(3 -> "Three")).withDefaultValue("foobar")

    assertEquals("element missing", newMap(4)) // see below, behavior changed for 2.13 but this test was faulty
  }

  private def defaultValueFunction: Int => String = {
    i => s"$i is not present in this map"
  }
  @Test
  def testWithDefaultValue(): Unit = {
    val m1 = mutable.SortedMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue("")

    assertEquals(m2(1), "a")
    assertEquals(m2(3), "")

    m2 += (3 -> "c")
    assertEquals(m2(3), "c")
    assertEquals(m2(4), "")

    m2 ++= List(4 -> "d", 5 -> "e", 6 -> "f")
    assertEquals(m2(3), "c")
    assertEquals(m2(4), "d")
    assertEquals(m2(5), "e")
    assertEquals(m2(6), "f")
    assertEquals(m2(7), "")

    m2 --= List(3, 4, 5)
    assertEquals(m2(3), "")
    assertEquals(m2(4), "")
    assertEquals(m2(5), "")
    assertEquals(m2(6), "f")
    assertEquals(m2(7), "")

    val m3 = m2 ++ List(3 -> "333")
    assertEquals(m2(3), "")
    assertEquals(m3(3), "333")
  }
  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testWithDefault(): Unit = {
    val m1 = mutable.SortedMap(1 -> "a", 2 -> "b")

    val m2: mutable.Map[Int, String] = m1.withDefault(i => (i + 1).toString)
    m2.update(1, "aa")
    m2.update(100, "bb")
    m2.addAll(List(500 -> "c", 501 -> "c"))

    assertEquals(m2(1), "aa")
    assertEquals(m2(2), "b")
    assertEquals(m2(3), "4")
    assertEquals(m2(4), "5")
    assertEquals(m2(500), "c")
    assertEquals(m2(501), "c")
    assertEquals(m2(502), "503")

    val m3: mutable.Map[Int, String] = m2 - 1
    assertEquals(m3(1), "2")

    val m4: mutable.Map[Int, String] = m3 -- List(2, 100)
    assertEquals(m4(2), "3")
    assertEquals(m4(100), "101")
  }
}
