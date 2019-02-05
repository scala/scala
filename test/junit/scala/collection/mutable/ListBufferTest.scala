package scala.collection.mutable

import org.junit.Assert.{assertEquals, assertTrue}
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
  def testFlatMapInPlace: Unit = {
    val xs = ListBuffer(3, 4, 5)
    val ys = List(-1, -2, -3, -4, -5, -6)

    val res = xs.flatMapInPlace(i => ys take i)

    assertEquals(ListBuffer(-1, -2, -3, -1, -2, -3, -4, -1, -2, -3, -4, -5), res)
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
    assertEquals(ListBuffer(), ListBuffer.range(0, 10).takeInPlace(-1))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 10).takeInPlace(10))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 100).takeInPlace(10))
  }

  @Test
  def testTakeRightInPlace: Unit = {
    assertEquals(ListBuffer(), ListBuffer().takeRightInPlace(10))
    assertEquals(ListBuffer(), ListBuffer.range(0, 10).takeRightInPlace(-1))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 10).takeRightInPlace(10))
    assertEquals(ListBuffer.range(90, 100), ListBuffer.range(0, 100).takeRightInPlace(10))
  }

  @Test
  def testTakeWhileInPlace: Unit = {
    assertEquals(ListBuffer(), ListBuffer[Int]().takeWhileInPlace(_ < 50))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 10).takeWhileInPlace(_ < 50))
    assertEquals(ListBuffer.range(0, 50), ListBuffer.range(0, 100).takeWhileInPlace(_ < 50))
  }

  @Test
  def testDropInPlace: Unit = {
    assertEquals(ListBuffer(), ListBuffer().dropInPlace(10))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 10).dropInPlace(-1))
    assertEquals(ListBuffer(), ListBuffer.range(0, 10).dropInPlace(10))
    assertEquals(ListBuffer.range(10, 100), ListBuffer.range(0, 100).dropInPlace(10))
  }

  @Test
  def testDropRightInPlace: Unit = {
    assertEquals(ListBuffer(), ListBuffer().dropRightInPlace(10))
    assertEquals(ListBuffer.range(0, 10), ListBuffer.range(0, 10).dropRightInPlace(-1))
    assertEquals(ListBuffer(), ListBuffer.range(0, 10).dropRightInPlace(10))
    assertEquals(ListBuffer.range(0, 90), ListBuffer.range(0, 100).dropRightInPlace(10))
  }

  @Test
  def testDropWhileInPlace: Unit = {
    assertEquals(ListBuffer(), ListBuffer[Int]().dropWhileInPlace(_ < 50))
    assertEquals(ListBuffer(), ListBuffer.range(0, 10).dropWhileInPlace(_ < 50))
    assertEquals(ListBuffer.range(50, 100), ListBuffer.range(0, 100).dropWhileInPlace(_ < 50))
  }

  @Test
  def testRemove: Unit = {
    val b1 = ListBuffer(0, 1, 2)
    assertEquals(0, b1.remove(0))
    assertEquals(ListBuffer(1, 2), b1)

    val b2 = ListBuffer(0, 1, 2)
    assertEquals(1, b2.remove(1))
    assertEquals(ListBuffer(0, 2), b2)

    val b3 = ListBuffer(0, 1, 2)
    assertEquals(2, b3.remove(2))
    assertEquals(ListBuffer(0, 1), b3)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def removeWithNegativeIndex: Unit = {
    ListBuffer(0, 1, 2).remove(-1)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def removeWithTooLargeIndex: Unit = {
    ListBuffer(0).remove(1)
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

  @Test
  def testTrimStart: Unit = {
    val b1 = ListBuffer()
    b1.trimStart(10)
    assertEquals(ListBuffer(), b1)

    val b2 = ListBuffer.range(0, 10)
    b2.trimStart(-1)
    assertEquals(ListBuffer.range(0, 10), b2)

    val b3 = ListBuffer.range(0, 10)
    b3.trimStart(10)
    assertEquals(ListBuffer(), b3)

    val b4 = ListBuffer.range(0, 100)
    b4.trimStart(10)
    assertEquals(ListBuffer.range(10, 100), b4)
  }

  @Test
  def testTrimEnd: Unit = {
    val b1 = ListBuffer()
    b1.trimEnd(10)
    assertEquals(ListBuffer(), b1)

    val b2 = ListBuffer.range(0, 10)
    b2.trimEnd(-1)
    assertEquals(ListBuffer.range(0, 10), b2)

    val b3 = ListBuffer.range(0, 10)
    b3.trimEnd(10)
    assertEquals(ListBuffer(), b3)

    val b4 = ListBuffer.range(0, 100)
    b4.trimEnd(10)
    assertEquals(ListBuffer.range(0, 90), b4)
  }

  @Test
  def testPatch: Unit = {
    val buffer = ListBuffer(0, 1, 2, 3)
    val patch = List(-3, -2, -1)
    assertEquals(ListBuffer(-3, -2, -1, 0, 1, 2, 3), buffer.patch(from = -1, patch, replaced = -1))
    assertEquals(ListBuffer(-3, -2, -1, 0, 1, 2, 3), buffer.patch(from = 0, patch, replaced = 0))
    assertEquals(ListBuffer(0, -3, -2, -1, 2, 3), buffer.patch(from = 1, patch, replaced = 1))
    assertEquals(ListBuffer(0, -3, -2, -1), buffer.patch(from = 1, patch, replaced = 3))
    assertEquals(ListBuffer(0, 1, -3, -2, -1), buffer.patch(from = 2, patch, replaced = 2))
    assertEquals(ListBuffer(0, 1, 2, 3, -3, -2, -1), buffer.patch(from = 10, patch, replaced = 10))
    assertEquals(ListBuffer(-3, -2, -1), buffer.patch(from = 0, patch, replaced = 100))
  }

  @Test
  def testPatchInPlace: Unit = {
    def testPatchInPlace(from: Int, replaced: Int, expectation: ListBuffer[Int]) =
      assertEquals(expectation, ListBuffer(0, 1, 2).patchInPlace(from, patch = List(-3, -2, -1), replaced))

    testPatchInPlace(from = -1, replaced = -1, expectation = ListBuffer(-3, -2, -1, 0, 1, 2))
    testPatchInPlace(from = 0, replaced = 0, expectation = ListBuffer(-3, -2, -1, 0, 1, 2))
    testPatchInPlace(from = 1, replaced = 1, expectation = ListBuffer(0, -3, -2, -1, 2))
    testPatchInPlace(from = 1, replaced = 2, expectation = ListBuffer(0, -3, -2, -1))
    testPatchInPlace(from = 2, replaced = 1, expectation = ListBuffer(0, 1, -3, -2, -1))
    testPatchInPlace(from = 10, replaced = 10, expectation = ListBuffer(0, 1, 2, -3, -2, -1))
    testPatchInPlace(from = 0, replaced = 100, expectation = ListBuffer(-3, -2, -1))
  }
}
