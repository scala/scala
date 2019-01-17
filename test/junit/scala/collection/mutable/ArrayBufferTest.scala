package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert.assertEquals


import scala.tools.testkit.AssertUtil
import scala.tools.testkit.AssertUtil.assertThrows

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
  def testFlatMapInPlace: Unit = {
    val xs = ArrayBuffer(3, 4, 5)
    val ys = List(-1, -2, -3, -4, -5, -6)

    val res = xs.flatMapInPlace(i => ys take i)

    assertEquals(ArrayBuffer(-1, -2, -3, -1, -2, -3, -4, -1, -2, -3, -4, -5), res)
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
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).takeInPlace(-1))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).takeInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 100).takeInPlace(10))
  }

  @Test
  def testTakeRightInPlace: Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().takeRightInPlace(10))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).takeRightInPlace(-1))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).takeRightInPlace(10))
    assertEquals(ArrayBuffer.range(90, 100), ArrayBuffer.range(0, 100).takeRightInPlace(10))
  }

  @Test
  def testTakeWhileInPlace: Unit = {
    assertEquals(ArrayBuffer(), ListBuffer[Int]().takeWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer.range(0, 10), ListBuffer.range(0, 10).takeWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer.range(0, 50), ListBuffer.range(0, 100).takeWhileInPlace(_ < 50))
  }

  @Test
  def testDropInPlace: Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().dropInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).dropInPlace(-1))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).dropInPlace(10))
    assertEquals(ArrayBuffer.range(10, 100), ArrayBuffer.range(0, 100).dropInPlace(10))
  }

  @Test
  def testDropRightInPlace: Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().dropRightInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).dropRightInPlace(-1))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).dropRightInPlace(10))
    assertEquals(ArrayBuffer.range(0, 90), ArrayBuffer.range(0, 100).dropRightInPlace(10))
  }

  @Test
  def testDropWhileInPlace: Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer[Int]().dropWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).dropWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer.range(50, 100), ArrayBuffer.range(0, 100).dropWhileInPlace(_ < 50))
  }

  @Test
  def testRemove: Unit = {
    val b1 = ArrayBuffer(0, 1, 2)
    assertEquals(0, b1.remove(0))
    assertEquals(ArrayBuffer(1, 2), b1)

    val b2 = ArrayBuffer(0, 1, 2)
    assertEquals(1, b2.remove(1))
    assertEquals(ArrayBuffer(0, 2), b2)

    val b3 = ArrayBuffer(0, 1, 2)
    assertEquals(2, b3.remove(2))
    assertEquals(ArrayBuffer(0, 1), b3)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testRemoveWithNegativeIndex: Unit = {
    ArrayBuffer(0, 1, 2).remove(-1)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testRemoveWithTooLargeIndex: Unit = {
    ArrayBuffer(0).remove(1)
  }

  @Test
  def testRemoveMany: Unit = {
    def testRemoveMany(index: Int, count: Int, expectation: ArrayBuffer[Int]): Unit = {
      val buffer = ArrayBuffer(0, 1, 2)
      buffer.remove(index, count)
      assertEquals(expectation, buffer)
    }

    testRemoveMany(index = 0, count = 0, expectation = ArrayBuffer(0, 1, 2))
    testRemoveMany(index = 0, count = 1, expectation = ArrayBuffer(1, 2))
    testRemoveMany(index = 0, count = 2, expectation = ArrayBuffer(2))
    testRemoveMany(index = 0, count = 3, expectation = ArrayBuffer())
    testRemoveMany(index = 1, count = 1, expectation = ArrayBuffer(0, 2))
    testRemoveMany(index = 1, count = 2, expectation = ArrayBuffer(0))
    testRemoveMany(index = 2, count = 1, expectation = ArrayBuffer(0, 1))
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testRemoveManyWithNegativeIndex: Unit = {
    ArrayBuffer(0, 1, 2).remove(index = -1, count = 1)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testRemoveManyWithTooLargeIndex: Unit = {
    ArrayBuffer(0).remove(index = 1, count = 1)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testRemoveManyWithNegativeCount: Unit = {
    ArrayBuffer(0).remove(index = 0, count = -1)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testRemoveManyWithTooLargeCount: Unit = {
    ArrayBuffer(0).remove(index = 0, count = 100)
  }

  @Test
  def testTrimStart: Unit = {
    val b1 = ArrayBuffer()
    b1.trimStart(10)
    assertEquals(ArrayBuffer(), b1)

    val b2 = ArrayBuffer.range(0, 10)
    b2.trimStart(-1)
    assertEquals(ArrayBuffer.range(0, 10), b2)

    val b3 = ArrayBuffer.range(0, 10)
    b3.trimStart(10)
    assertEquals(ArrayBuffer(), b3)

    val b4 = ArrayBuffer.range(0, 100)
    b4.trimStart(10)
    assertEquals(ArrayBuffer.range(10, 100), b4)
  }

  @Test
  def testTrimEnd: Unit = {
    val b1 = ArrayBuffer()
    b1.trimEnd(10)
    assertEquals(ArrayBuffer(), b1)

    val b2 = ArrayBuffer.range(0, 10)
    b2.trimEnd(-1)
    assertEquals(ArrayBuffer.range(0, 10), b2)

    val b3 = ArrayBuffer.range(0, 10)
    b3.trimEnd(10)
    assertEquals(ArrayBuffer(), b3)

    val b4 = ArrayBuffer.range(0, 100)
    b4.trimEnd(10)
    assertEquals(ArrayBuffer.range(0, 90), b4)
  }

  @Test
  def testPatch: Unit = {
    val buffer = ArrayBuffer(0, 1, 2, 3)
    val patch = List(-3, -2, -1)
    assertEquals(ArrayBuffer(-3, -2, -1, 0, 1, 2, 3), buffer.patch(from = -1, patch, replaced = -1))
    assertEquals(ArrayBuffer(-3, -2, -1, 0, 1, 2, 3), buffer.patch(from = 0, patch, replaced = 0))
    assertEquals(ArrayBuffer(0, -3, -2, -1, 2, 3), buffer.patch(from = 1, patch, replaced = 1))
    assertEquals(ArrayBuffer(0, -3, -2, -1), buffer.patch(from = 1, patch, replaced = 3))
    assertEquals(ArrayBuffer(0, 1, -3, -2, -1), buffer.patch(from = 2, patch, replaced = 2))
    assertEquals(ArrayBuffer(0, 1, 2, 3, -3, -2, -1), buffer.patch(from = 10, patch, replaced = 10))
    assertEquals(ArrayBuffer(-3, -2, -1), buffer.patch(from = 0, patch, replaced = 100))
  }

  @Test
  def testPatchInPlace: Unit = {
    def testPatchInPlace(from: Int, replaced: Int, expectation: ArrayBuffer[Int]) =
      assertEquals(expectation, ArrayBuffer(0, 1, 2).patchInPlace(from, patch = List(-3, -2, -1), replaced))

    testPatchInPlace(from = -1, replaced = -1, expectation = ArrayBuffer(-3, -2, -1, 0, 1, 2))
    testPatchInPlace(from = 0, replaced = 0, expectation = ArrayBuffer(-3, -2, -1, 0, 1, 2))
    testPatchInPlace(from = 1, replaced = 1, expectation = ArrayBuffer(0, -3, -2, -1, 2))
    testPatchInPlace(from = 1, replaced = 2, expectation = ArrayBuffer(0, -3, -2, -1))
    testPatchInPlace(from = 2, replaced = 1, expectation = ArrayBuffer(0, 1, -3, -2, -1))
    testPatchInPlace(from = 10, replaced = 10, expectation = ArrayBuffer(0, 1, 2, -3, -2, -1))
    testPatchInPlace(from = 0, replaced = 100, expectation = ArrayBuffer(-3, -2, -1))
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testApplyWhenEmpty: Unit = {
    new ArrayBuffer().apply(0)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testApplyAfterClearing: Unit = {
    val buffer = ArrayBuffer(1, 2, 3)
    buffer.clear()

    buffer(0)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testUpdateWhenEmpty: Unit = {
    new ArrayBuffer().update(0, 100)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testUpdateAfterClearing: Unit = {
    val buffer = ArrayBuffer(1, 2, 3)
    buffer.clear()

    buffer.update(0, 100)
  }

  @Test
  def testClear: Unit = {
    val buffer = ArrayBuffer(1, 2, 3)
    buffer.clear()

    assertEquals(0, buffer.size)
  }

  @Test
  def testSortInPlace: Unit = {
    val buffer = ArrayBuffer(3, 2, 1)
    buffer.sortInPlace()

    assertEquals(ArrayBuffer(1, 2, 3), buffer)
  }
  @Test
  def testMapResult: Unit = {
    val buffer = ArrayBuffer(3, 2, 1)

    val builder = buffer.mapResult(_.mkString(","))

    buffer.prepend(4)

    assertEquals(builder.result(), "4,3,2,1")
  }

  @Test
  def emptyIteratorDropOneMustBeEmpty: Unit = {
    assertThrows[NoSuchElementException](new ArrayBuffer[Int].iterator.drop(1).next())
  }

  @Test
  def t11114_ArrayBufferPatch: Unit = {
    {
      def newBuf = ArrayBuffer(1, 2, 3, 4, 5)
      assertEquals(ArrayBuffer(1, 2, 3, 10, 11), newBuf.patchInPlace(3, List(10, 11), 4))
      assertEquals(ArrayBuffer(1, 2, 3, 10, 11), newBuf.patchInPlace(3, List(10, 11), 10))
      assertEquals(ArrayBuffer(10, 11), newBuf.patchInPlace(0, List(10, 11), 10))
      assertEquals(ArrayBuffer(1, 2, 3, 10, 11, 12), newBuf.patchInPlace(3, List(10, 11, 12), 4))
    }

    for {
      size <- 0 to 10
      patchSize <- 0 to 12
      patchRange = 100 until (100 + patchSize)
      patch <- Seq(() => patchRange.toVector, () => patchRange.iterator)
      from <- -1 to 11
      replaced <- -1 to 13
    } {
      def createBuf = (0 until size).to(ArrayBuffer)

      val fromPatch = createBuf.patch(from, patch(), replaced)
      val fromPatchInPlace = createBuf.patchInPlace(from, patch(), replaced)

      assert(fromPatch == fromPatchInPlace,
        s"""Failed on:
           |  size: $size
           |  targetBuffer: $createBuf
           |  from: $from
           |  patch sequence: ${patch()} (${patch().toVector})
           |  replaced: $replaced
           |  patch returned: $fromPatch
           |  patchInPlace returned: $fromPatchInPlace
         """.stripMargin
      )
    }
  }

  @Test def t11417_sortInPlace: Unit = {
    val a = ArrayBuffer(5,4,3,2,1)
    a.trimEnd(2)
    a.sortInPlace
    assertEquals(List(3,4,5), a)
  }

  @Test def trimToSize: Unit = {
    val b = ArrayBuffer(1,2,3)
    assertEquals(8, b.array.length)
    b ++= (1 to 1000)
    assertEquals(1024, b.array.length)
    b.remove(200, 803)
    b.trimToSize()
    assertEquals(256, b.array.length)
  }
}
