package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.runtime.AbstractFunction2
import scala.tools.testkit.AllocationTest

@RunWith(classOf[JUnit4])
class ListMapTest extends AllocationTest {

  @Test
  def t7445(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5)
    assertEquals(ListMap(2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5), m.tail)
  }

  @Test
  def hasCorrectBuilder(): Unit = {
    val m = ListMap("a" -> "1", "b" -> "2.2", "c" -> "3", "b" -> "2.2", "d" -> "4")
    assertEquals(List("a" -> "1", "b" -> "2.2", "c" -> "3", "d" -> "4"), m.toList)
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
    assertEquals(ListMap(1 -> 1, 2 -> 4, 3 -> 3), m + (2 -> 4))
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3), m + (2 -> 2))
    assertEquals(ListMap(2 -> 2, 3 -> 3), m - 1)
    assertEquals(ListMap(1 -> 1, 3 -> 3), m - 2)
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3), m - 4)
  }

  @Test
  def hasCorrectIterator(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4)
    assertEquals(List(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4), m.iterator.toList)
  }

  @Test
  def keysShouldPreserveOrderAsInserted(): Unit = {
    val m = ListMap("a" -> "1", "b" -> "2", "c" -> "3", "d" -> "4", "e" -> "5")
    assertEquals(List("A", "B", "C", "D", "E"), m.keys.map(_.toUpperCase).toList)
  }

  @Test
  def foldRightAllocations(): Unit = {
    var m = ListMap.empty[Int, Int]
    object acc extends AbstractFunction2[(Int, Int), String, String] {
      def dataFrom(m: ListMap[Int, Int]): Unit = {
        data = m.toArray.reverse
        strings = data map (x => x.toString+"XX")
      }

      var data = m.toArray
      var strings = new Array[String](0)
      var index = -1
      var last = ""

      def reset = {
        index = -1
        last = "INIT"
        last
      }
      override def apply(v1: (Int, Int), v2: String): String = {
        assert (v2 eq last)
        index += 1
        if (v1 != data(index))
          throw new AssertionError(s"index: $index expected ${data(index)} but was $v1")
        last = strings(index)
        last
      }
    }
    acc.dataFrom(m)
    assertEquals("INIT", nonAllocating(m.foldRight(acc.reset)(acc)))

    m = ListMap(1 -> 1)
    acc.dataFrom(m)
    assertEquals("(1,1)XX", onlyAllocates(tuples(m.size))(m.foldRight(acc.reset)(acc)))

    m = ListMap(1 -> 1, 2 -> 2)
    acc.dataFrom(m)
    assertEquals("(1,1)XX", onlyAllocates(tuples(m.size))(m.foldRight(acc.reset)(acc)))

    m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    acc.dataFrom(m)
    assertEquals("(1,1)XX", onlyAllocates(tuples(m.size))(m.foldRight(acc.reset)(acc)))
  }

  /** we base the result on tuples of strings as ListMap is not specialised */
  private def tuples(n: Int) = ListMapTest.tupleStringCost * n
}

object ListMapTest {
  val tupleStringCost = AllocationTest.sizeOf(("1","1"), "a tuple of Strings")
}
