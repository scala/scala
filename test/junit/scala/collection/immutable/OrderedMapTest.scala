package scala.collection.immutable

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class OrderedMapTest {

  @Test
  def t7445(): Unit = {
    val m = OrderedMap(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4)
    assertEquals(OrderedMap(2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), m.tail)
  }

  @Test
  def hasCorrectBuilder(): Unit = {
    val m = OrderedMap("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4")
    assertEquals(List("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4"), m.toList)
  }

  @Test
  def hasCorrectHeadTailLastInitWhenOrderingByInsertion(): Unit = {
    val m = OrderedMap(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(3 -> 1, m.head)
    assertEquals(OrderedMap(2 -> 2, 1 -> 3), m.tail)
    assertEquals(1 -> 3, m.last)
    assertEquals(OrderedMap(3 -> 1, 2 -> 2), m.init)
  }

  @Test
  def hasCorrectHeadTailLastInitWhenOrderingByModification(): Unit = {
    val m = OrderedMap(3 -> 1, 2 -> 2, 1 -> 3).orderedBy(OrderedMap.OrderBy.Modification).updated(2, 4)
    assertEquals(3 -> 1, m.head)
    assertEquals(OrderedMap(1 -> 3, 2 → 4), m.tail)
    assertEquals(2 -> 4, m.last)
    assertEquals(OrderedMap(3 -> 1, 1 -> 3), m.init)
  }

  @Test
  def hasCorrectAddRemoveWhenOrderingByInsertion(): Unit = {
    val m = OrderedMap(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(OrderedMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(OrderedMap(3 -> 1, 2 -> 4, 1 -> 3), m + (2 -> 4))
    assertEquals(OrderedMap(3 -> 1, 2 -> 2, 1 -> 3), m + (2 -> 2))
    assertEquals(OrderedMap(3 -> 2, 2 -> 2, 1 -> 3), m + (3 -> 2))
    assertEquals(OrderedMap(3 -> 1, 2 -> 2), m - 1)
    assertEquals(OrderedMap(3 -> 1, 1 -> 3), m - 2)
    assertEquals(OrderedMap(3 -> 1, 2 -> 2, 1 -> 3), m - 4)
  }

  @Test
  def hasCorrectAddRemoveWhenOrderingByModification(): Unit = {
    val m = OrderedMap(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(OrderedMap(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(OrderedMap(3 -> 1, 1 -> 3, 2 -> 4), m + (2 -> 4))
    assertEquals(OrderedMap(3 -> 1, 1 -> 3, 2 -> 2), m + (2 -> 2))
    assertEquals(OrderedMap(2 -> 2, 3 -> 2, 1 -> 4), m + (3 -> 2) + (1 → 4))
    assertEquals(OrderedMap(3 -> 1, 2 -> 2), m - 1)
    assertEquals(OrderedMap(3 -> 1, 1 -> 3), m - 2)
    assertEquals(OrderedMap(3 -> 1, 2 -> 2, 1 -> 3), m - 4)
  }

  @Test
  def hasCorrectIterator: Unit = {
    val m = OrderedMap(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4)
    assertEquals(List(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), m.iterator.toList)
  }
}
