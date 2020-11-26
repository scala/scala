package scala.collection.immutable

import org.junit.Assert.{assertEquals, assertNotEquals}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testkit.AllocationTest

@RunWith(classOf[JUnit4])
class SortedMapTest extends AllocationTest {
  @Test
  def testWithDefaultValueReturnsSortedMapWithDefaultValue(): Unit = {
    val tree: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)

    assertEquals("3 is not present in this map", tree(3))
  }

  @Test
  def testDefaultValueIsPersistedAfterAnElementIsAddedToUnderlyingMap(): Unit = {
    val tree: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two")).withDefault(defaultValueFunction)
    val newTree = tree + (3 -> "Three")

    assertEquals("5 is not present in this map", newTree(5))
  }

  @Test
  def testDefaultValueIsPersistedAfterCreatingEmptyMapFromUnderlyingSortedMap(): Unit = {
    val emptyMap: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
      .withDefault(defaultValueFunction)
      .empty

    assertEquals("1 is not present in this map", emptyMap(1))
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
    val originalTreeMap : SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 3 -> "Three"))
      .withDefault(defaultValueFunction)

    val newTreeMap = originalTreeMap - 3

    assertEquals("3 is not present in this map", newTreeMap(3))
    assertEquals(2, newTreeMap.size)
  }

  @Test
  def testDefaultValueIsLostWhenNewSortedMapIsCreatedFromIterablesInOperationsLikeFlatMap(): Unit = {
    val originalMap: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two"))
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
    val evenNumbers: SortedMap[Int, String] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 4 -> "Four", 5 -> "Five"))
      .withDefault(defaultValueFunction)
      .filter((kv: (Int, String)) => kv._1 % 2 == 0)

    assertEquals("5 is not present in this map", evenNumbers(5))
  }

  @Test
  def testDefaultValueIsPersistedWhenNewSpecificBuilderIsCreatedFromSortedMapInOperationsLikeGroupBy(): Unit = {
    val numbers: Map[String, SortedMap[Int, String]] = SortedMap.from(Map(1 -> "One", 2 -> "Two", 4 -> "Four", 5 -> "Five"))
      .withDefault(defaultValueFunction)
      .groupBy((kv: (Int, String)) => if (kv._1 % 2 == 0) "even" else "odd")

    assertEquals("6 is not present in this map", numbers("even")(6))
    assertEquals("3 is not present in this map", numbers("odd")(3))
  }

  @Test
  def testDefaulValueIsPersistedWhenNewMapIterableIsConcatenatedToOriginalMap(): Unit = {
    val originalMap: SortedMap[Int, String] = TreeMap(1 -> "One", 2 -> "Two")
      .withDefaultValue("element missing")
    val newMap: SortedMap[Int, String] = originalMap ++ Map(3 -> "Three")

    assertEquals("element missing", newMap(4))
  }

  @Test
  def testTransformReturnsSortedMap(): Unit = {
    val tm = SortedMap(1 -> 1).transform(_ + _)
    assert((tm: SortedMap[Int, Int]).isInstanceOf[SortedMap[_, _]])
  }

  private def defaultValueFunction: Int => String = {
    i => s"$i is not present in this map"
  }
  @Test
  def testWithDefaultValue(): Unit = {
    val m1 = SortedMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue("missing")
    assertEquals("a", m2(1))
    assertEquals("missing", m2(3))
  }
  @Test
  def testWithDefault(): Unit = {
    val m1 = SortedMap(1 -> "a", 2 -> "b")

    val m2: Map[Int, String] =
      m1.withDefault(i => (i + 1).toString)
        .updated(1, "aa")
        .updated(100, "bb")
        .concat(List(500 -> "c", 501 -> "c"))

    assertEquals(m2(1), "aa")
    assertEquals(m2(2), "b")
    assertEquals(m2(3), "4")
    assertEquals(m2(4), "5")
    assertEquals(m2(500), "c")
    assertEquals(m2(501), "c")
    assertEquals(m2(502), "503")

    val m3: Map[Int, String] = m2 - 1
    assertEquals(m3(1), "2")

    val m4: Map[Int, String] = m3 -- List(2, 100)
    assertEquals(m4(2), "3")
    assertEquals(m4(100), "101")
  }

  @Test def updatedWithReturnsSortedMap(): Unit = {
    val m1 = SortedMap(1 -> "a")
    val m2 = m1.updatedWith(2) { case Some(v) => Some(v.toUpperCase) case None => Some("DEFAULT") }
    val m3: SortedMap[Int, String] = m2 // check the type returned by `updatedWith`
    assertEquals(SortedMap(1 -> "a", 2 -> "DEFAULT"), m3)
  }

  @Test def empty(): Unit = {
    val ord = Ordering[String]
    exactAllocates(24)(SortedMap.empty[String, String](ord))
  }
  @Test def apply0(): Unit = {
    val ord = Ordering[String]
    exactAllocates(24)(SortedMap()(ord))
  }
  @Test def apply1(): Unit = {
    val ord = Ordering[String]
    onlyAllocates(200)(SortedMap(("a", "a"))(ord))
  }
  @Test def apply2(): Unit = {
    val ord = Ordering[String]
    onlyAllocates(312)(SortedMap(("a", "a"), ("b", "b"))(ord))
  }
}
