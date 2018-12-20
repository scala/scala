package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ListMapTest {

  @Test
  def t7445(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5)
    assertEquals(ListMap(2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5), m.tail)
  }

  /*
  Modifying this test to make up for the fix (bug #11306)
  where updating existing key retains its position
   */
  @Test
  def hasCorrectBuilder(): Unit = {
    val m = ListMap("a" -> "1", "b" -> "2", "c" -> "3", "b" -> "2.2", "d" -> "4")
    assertEquals(List("a" -> "1","b" -> "2.2", "c" -> "3",  "d" -> "4"), m.toList)
  }

  @Test
  def hasCorrectHeadTailLastInit(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    assertEquals(1 -> 1, m.head)
    assertEquals(ListMap(2 -> 2, 3 -> 3), m.tail)
    assertEquals(3 -> 3, m.last)
    assertEquals(ListMap(1 -> 1, 2 -> 2), m.init)
  }

  @Test
  def hasCorrectAddRemove(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(ListMap(1 -> 1, 3 -> 3, 2 -> 4), m + (2 -> 4))
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3), m + (2 -> 2))
    assertEquals(ListMap(2 -> 2, 3 -> 3), m - 1)
    assertEquals(ListMap(1 -> 1, 3 -> 3), m - 2)
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3), m - 4)
  }

  @Test
  def hasCorrectiterator: Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4)
    assertEquals(List(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4), m.iterator.toList)
  }

  @Test
  def keysShouldPreserveOrderAsInserted: Unit = {
    val m = ListMap("a" -> "1", "b" -> "2", "c" -> "3", "d" -> "4", "e" -> "5")
    assertEquals(List("A", "B", "C", "D", "E"), m.keys.map(_.toUpperCase).toList)
  }

  @Test
  def updatingExistingKeyShouldRetainItsPosition : Unit = {
    val m = ListMap(1 -> 100, 2 -> 200, 3 -> 500, 10 -> 455)
    val newNode1 = (2 -> 2343)
    val result1 = m + newNode1
    assertEquals(result1, ListMap(1 -> 100, newNode1, 3 -> 500, 10 -> 455))
    val newNode2 = (1 -> 101)
    val result2 = result1 + newNode2
    assertEquals(result2, ListMap(1 -> 101, newNode1, 3 -> 500, 10 -> 455))
  }
}
