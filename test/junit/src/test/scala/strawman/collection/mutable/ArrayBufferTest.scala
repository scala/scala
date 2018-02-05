package strawman.collection.mutable

import org.junit.Assert.assertEquals
import strawman.collection.immutable.List
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.tools.testing.AssertUtil

/* Test for scala/bug#9043 */
@RunWith(classOf[JUnit4])
class ArrayBufferTest {
  @Test
  def testInsertAll: Unit = {
    val traver = ArrayBuffer(2, 4, 5, 7)
    val testSeq = List(1, 3, 6, 9)

    def insertAt(x: Int) = {
      val clone = traver.clone()
      clone.insertAll(x, testSeq)
      clone
    }

    // Just insert some at position 0
    assertEquals(ArrayBuffer(1, 3, 6, 9, 2, 4, 5, 7), insertAt(0))

    // Insert in the middle
    assertEquals(ArrayBuffer(2, 4, 1, 3, 6, 9, 5, 7), insertAt(2))

    // No strange last position weirdness
    assertEquals(ArrayBuffer(2, 4, 5, 7, 1, 3, 6, 9), insertAt(traver.size))

    // Overflow is caught
    AssertUtil.assertThrows[IndexOutOfBoundsException] { insertAt(-1) }
    AssertUtil.assertThrows[IndexOutOfBoundsException] { insertAt(traver.size + 10) }
  }

  @Test
  def testInsertTop: Unit = {
    val buffer = ArrayBuffer.empty[Int]
    val els = 0 until 100

    for (i <- els) buffer.insert(0, i)

    assertEquals(ArrayBuffer(els.reverse: _*), buffer)
  }

  @Test
  def testInsertEnd: Unit = {
    val buffer = ArrayBuffer.empty[Int]
    val els = 0 until 100

    for (i <- els) buffer.insert(i, i)

    assertEquals(ArrayBuffer(els: _*), buffer)
  }

  @Test
  def testPrepend: Unit = {
    val buffer = ArrayBuffer.empty[Int]
    val els = 0 until 100

    for (i <- els) i +=: buffer

    assertEquals(ArrayBuffer(els.reverse: _*), buffer)
  }

  @Test
  def testFilterInPlace: Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 100).filterInPlace(_ => false))
    assertEquals(ArrayBuffer.range(0, 100), ArrayBuffer.range(0, 100).filterInPlace(_ => true))
    assertEquals(ArrayBuffer.range(start = 0, end = 100, step = 2), ArrayBuffer.range(0, 100).filterInPlace(_ % 2 == 0))
    assertEquals(ArrayBuffer.range(start = 1, end = 100, step = 2), ArrayBuffer.range(0, 100).filterInPlace(_ % 2 != 0))
  }

  @Test
  def testTakeInPlace: Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().takeInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).takeInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 100).takeInPlace(10))
  }
}
