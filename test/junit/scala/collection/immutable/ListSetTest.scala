package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ListSetTest {

  @Test
  def t7445(): Unit = {
    val s = ListSet(1, 2, 3, 4, 5)
    assertEquals(ListSet(2, 3, 4, 5), s.tail)
  }

  @Test
  def hasCorrectBuilder(): Unit = {
    val m = ListSet("a", "b", "c", "b", "d")
    assertEquals(List("a", "b", "c", "d"), m.toList)
  }

  @Test
  def hasTailRecursiveDelete(): Unit = {
    val s = ListSet(1 to 50000: _*)
    try s - 25000 catch { case e: StackOverflowError => fail("A stack overflow occurred") }
  }

  @Test
  def hasCorrectHeadTailLastInit(): Unit = {
    val m = ListSet(1, 2, 3)
    assertEquals(1, m.head)
    assertEquals(ListSet(2, 3), m.tail)
    assertEquals(3, m.last)
    assertEquals(ListSet(1, 2), m.init)
  }

  @Test
  def hasCorrectAddRemove(): Unit = {
    val m = ListSet(1, 2, 3)
    assertEquals(ListSet(1, 2, 3, 4), m + 4)
    assertEquals(ListSet(1, 2, 3), m + 2)
    assertEquals(ListSet(2, 3), m - 1)
    assertEquals(ListSet(1, 3), m - 2)
    assertEquals(ListSet(1, 2, 3), m - 4)
  }

  @Test
  def hasCorrectIterator(): Unit = {
    val s = ListSet(1, 2, 3, 5, 4)
    assertEquals(List(1, 2, 3, 5, 4), s.iterator.toList)
  }
}
