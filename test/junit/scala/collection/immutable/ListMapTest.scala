package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.runtime.AbstractFunction2
import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class ListMapTest extends AllocationTest {

  @Test
  def t7445(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5)
    assertEquals(ListMap(2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5), m.tail)
  }

  @Test
  def hasCorrectBuilder(): Unit = {
    val m = ListMap("a" -> "1", "b" -> "2", "c" -> "3", "b" -> "2.2", "d" -> "4")
    assertEquals(List("a" -> "1", "c" -> "3", "b" -> "2.2", "d" -> "4"), m.toList)
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
  def hasCorrectIterator(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4)
    assertEquals(List(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4), m.iterator.toList)
  }

  @Test
  def lastNonAllocating(): Unit = {
    var m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4)
    assertEquals((4 -> 4), exactAllocates(tuples(1))(m.last))
    assertEquals(Some((4 -> 4)), exactAllocates(someTuples(1))(m.lastOption))

    m = ListMap.empty
    assertEquals(None, nonAllocating(m.lastOption))

  }
  @Test
  def iteratorAllocations(): Unit = {
    val m = ListMap()
    assertSame(Iterator.empty, nonAllocating(m.iterator))
  }
  @Test
  def foldRightAllocations(): Unit = {
    var m = ListMap.empty[Int, Int]
    object acc extends AbstractFunction2[(Int, Int), String, String] {
      def dataFrom(m: ListMap[Int, Int]): Unit = {
        data = m.toArray.reverse
        strings = data map (x => x.toString + "XX")
      }

      var data    = m.toArray
      var strings = new Array[String](0)
      var index   = -1
      var last    = ""

      def reset = {
        index = -1
        last = "INIT"
        last
      }
      override def apply(v1: (Int, Int), v2: String): String = {
        assert(v2 eq last)
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
    assertEquals("(1,1)XX", exactAllocates(tuples(m.size))(m.foldRight(acc.reset)(acc)))

    m = ListMap(1 -> 1, 2 -> 2)
    acc.dataFrom(m)
    assertEquals("(1,1)XX", exactAllocates(tuples(m.size))(m.foldRight(acc.reset)(acc)))

    m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    acc.dataFrom(m)
    assertEquals("(1,1)XX", exactAllocates(tuples(m.size))(m.foldRight(acc.reset)(acc)))

  }
  @Test def removeMissingNonAllocating: Unit = {
    var m           = ListMap.empty[Any, Int]
    val boxedRemove = 999.asInstanceOf[AnyRef]
    assertSame(m, nonAllocating(m - boxedRemove))

    for (i <- 1 to 100) {
      m += (i -> i)
      assertEquals(i, m.size)
      assertSame(m, nonAllocating(m - boxedRemove))
    }
  }
  @Test def removeLastNonAllocating: Unit = {
    var m = ListMap(1 -> 1)
    assertSame(ListMap.empty, nonAllocating(m - 1))

    for (i <- 2 to 100) {
      val oldM = m
      m += (i -> i)
      assertEquals(i, m.size)
      assertSame(oldM, nonAllocating(m - i))
    }
  }
  @Test def removeNotLastAllocations: Unit = {
    var underTest = ListMap(1 -> 1, 2 -> 2)
    var compare   = HashMap(1 -> 1, 2 -> 2)
    assertEquals((compare - 1).toList.sorted, exactAllocates(nodes(1))(underTest - 1).toList.sorted)

    for (i <- 3 to 100) {
      underTest += (i -> i)
      compare += (i -> i)
      assertEquals(i, underTest.size)

      for (removeIndex <- List(1, 2, i / 2, i - 2, i - 1)) {
        val internalIndexToRemove = i - removeIndex
        val allocationExpected    = if (internalIndexToRemove < 8) nodes(internalIndexToRemove) else (nodes(internalIndexToRemove) + array(internalIndexToRemove))
        assertEquals((compare - removeIndex).toList.sorted,
                     (exactAllocates(allocationExpected, s"i = $i, removeIndex = $removeIndex, internalIndexToRemove = $internalIndexToRemove")
                     (underTest - removeIndex)).toList.sorted)
      }
    }
  }

  /** we base the result on tuples of strings as ListMap is not specialised */
  private def tuples(n: Int) = ListMapTest.tupleStringCost * n
  private def someTuples(n: Int) = ListMapTest.someTupleStringCost * n
  private def nodes(n: Int) = ListMapTest.nodeStringCost * n
  private def array(n: Int) = ListMapTest.nodeArray0Cost + ((n+1) / 2) * (ListMapTest.nodeArray2Cost - ListMapTest.nodeArray0Cost)
}

object ListMapTest {
  val tupleStringCost     = AllocationTest.sizeOf(("1", "1"), "a tuple of Strings")
  val someTupleStringCost = AllocationTest.sizeOf(Some("1", "1"), "a option of tuple of Strings")
  val nodeStringCost      = AllocationTest.sizeOf(ListMap.empty.updated("1", "1"), "a ListMap.Node of Strings")
  val nodeArray0Cost      = AllocationTest.sizeOf(new Array[String](0), "empty array", true)
  val nodeArray1Cost      = AllocationTest.sizeOf(new Array[String](1), "1 array", true)
  val nodeArray2Cost      = AllocationTest.sizeOf(new Array[String](2), "2 array", true)

}
