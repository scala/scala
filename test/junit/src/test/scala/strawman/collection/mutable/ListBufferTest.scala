package strawman.collection.mutable

import org.junit.Assert.{assertEquals, assertTrue}
import strawman.collection.immutable.Nil
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ListBufferTest {

  @Test
  def testClear(): Unit = {
    val b = ListBuffer.empty[String]
    b += "a"
    assertTrue(b.sameElements("a" :: Nil))
    b.clear()
    assertEquals(ListBuffer.empty[String], b)
    b += "b"
    assertTrue(b.sameElements("b" :: Nil))

    val b2 = ListBuffer.empty[String]
    b2 += "a"
    val _ = b2.toList
    b2.clear()
    b2 += "b"
    assertTrue(b2.sameElements("b" :: Nil))
  }

  @Test
  def testFilterInPlace: Unit = {
    assertEquals(ListBuffer(), ListBuffer.range(0, 100).filterInPlace(_ => false))
    assertEquals(ListBuffer.range(0, 100), ListBuffer.range(0, 100).filterInPlace(_ => true))
    assertEquals(ListBuffer.range(start = 0, end = 100, step = 2), ListBuffer.range(0, 100).filterInPlace(_ % 2 == 0))
    assertEquals(ListBuffer.range(start = 1, end = 100, step = 2), ListBuffer.range(0, 100).filterInPlace(_ % 2 != 0))
  }

  @Test
  def testTakeInPlace: Unit = {
    assertEquals(ListBuffer(), ListBuffer().takeInPlace(10))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 10).takeInPlace(10))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 100).takeInPlace(10))
  }

  @Test
  def testRemoveMany: Unit = {
    def testRemoveMany(idx: Int, count: Int, expectation: ListBuffer[Int]): Unit = {
      val buffer = ListBuffer(0, 1, 2)
      buffer.remove(idx, count)
      assertEquals(expectation, buffer)
    }

    testRemoveMany(idx = 0, count = 0, expectation = ListBuffer(0, 1, 2))
    testRemoveMany(idx = 0, count = 1, expectation = ListBuffer(1, 2))
    testRemoveMany(idx = 0, count = 2, expectation = ListBuffer(2))
    testRemoveMany(idx = 0, count = 3, expectation = ListBuffer())
    testRemoveMany(idx = 1, count = 1, expectation = ListBuffer(0, 2))
    testRemoveMany(idx = 1, count = 2, expectation = ListBuffer(0))
    testRemoveMany(idx = 2, count = 1, expectation = ListBuffer(0, 1))
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def removeManyWithNegativeIndex: Unit = {
    ListBuffer(0, 1, 2).remove(idx = -1, count = 1)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def removeManyWithTooLargeIndex: Unit = {
    ListBuffer(0).remove(idx = 1, count = 1)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def removeManyWithNegativeCount: Unit = {
    ListBuffer(0).remove(idx = 0, count = -1)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def removeManyWithTooLargeCount: Unit = {
    ListBuffer(0).remove(idx = 0, count = 100)
  }
}
