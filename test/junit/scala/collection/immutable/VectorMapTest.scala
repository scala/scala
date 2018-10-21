package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class VectorMapTest {

  @Test
  def t7445(): Unit = {
    val m = VectorMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5)
    assertEquals(VectorMap(2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5), m.tail)
  }

  @Test
  def hasCorrectBuilder(): Unit = {
    val m = VectorMap("a" -> "1", "b" -> "2", "c" -> "3", "b" -> "2.2", "d" -> "4")
    // The order here differs from the one returned by ListMap which returns b after c
    // (i.e. at the last position it is supplied), arguably the order returned here is
    // more correct for an insertion ordered map (as opposed to a modification ordered
    // one).
    assertEquals(List("a" -> "1", "b" -> "2.2", "c" -> "3", "d" -> "4"), m.toList)
  }

  @Test
  def hasCorrectHeadTailLastInit(): Unit = {
    val m = VectorMap(1 -> 1, 2 -> 2, 3 -> 3)
    assertEquals(1 -> 1, m.head)
    assertEquals(VectorMap(2 -> 2, 3 -> 3), m.tail)
    assertEquals(3 -> 3, m.last)
    assertEquals(VectorMap(1 -> 1, 2 -> 2), m.init)
  }

  @Test
  def hasCorrectAddRemove(): Unit = {
    val m = VectorMap(1 -> 1, 2 -> 2, 3 -> 3)
    assertEquals(VectorMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(VectorMap(1 -> 1, 3 -> 3, 2 -> 4), m + (2 -> 4))
    assertEquals(VectorMap(1 -> 1, 2 -> 2, 3 -> 3), m + (2 -> 2))
    assertEquals(VectorMap(2 -> 2, 3 -> 3), m - 1)
    assertEquals(VectorMap(1 -> 1, 3 -> 3), m - 2)
    assertEquals(VectorMap(1 -> 1, 2 -> 2, 3 -> 3), m - 4)
  }

  @Test
  def hasCorrectiterator: Unit = {
    val m = VectorMap(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4)
    assertEquals(List(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4), m.iterator.toList)
  }

  @Test
  def keysShouldPreserveOrderAsInserted: Unit = {
    val m = VectorMap("a" -> "1", "b" -> "2", "c" -> "3", "d" -> "4", "e" -> "5")
    assertEquals(List("A", "B", "C", "D", "E"), m.keys.map(_.toUpperCase).toList)
  }

  @Test
  def handlesNullKeys_t11217: Unit = {
    val m = VectorMap((null, 1), ("a", 2))
    assertEquals(List(null, "a"), m.keys.toList)
  }

  @Test
  def hasCorrectInit_t11218: Unit = {
    val m = VectorMap(1 -> "a", 2 -> "b", 3 -> "c").remove(2).init
    assertEquals(List(1 -> "a"), m.toList)
  }

  @Test
  def removeAllIsEmpty_t11220: Unit = {
    val m = VectorMap(1 -> "a") - 1 + (2 -> "b") - 2
    assertEquals(List(), m.toList)
  }
}
