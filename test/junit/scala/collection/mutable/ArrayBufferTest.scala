package scala.collection.mutable

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import java.lang.reflect.InvocationTargetException
import scala.annotation.nowarn
import scala.runtime.PStatics.VM_MaxArraySize
import scala.tools.testkit.AssertUtil.{assertSameElements, assertThrows, fail}
import scala.tools.testkit.ReflectUtil.{getMethodAccessible, _}
import scala.util.chaining._

class ArrayBufferTest {

  /* Test for scala/bug#9043 */
  @Test
  def testInsertAll: Unit = {
    def traver = ArrayBuffer(2, 4, 5, 7)
    val testSeq = List(1, 3, 6, 9)

    def insertAt(x: Int) = traver.tap(_.insertAll(x, testSeq))

    // Just insert some at position 0
    assertEquals(ArrayBuffer(1, 3, 6, 9, 2, 4, 5, 7), insertAt(0))

    // Insert in the middle
    assertEquals(ArrayBuffer(2, 4, 1, 3, 6, 9, 5, 7), insertAt(2))

    // No strange last position weirdness
    assertEquals(ArrayBuffer(2, 4, 5, 7, 1, 3, 6, 9), insertAt(traver.size))

    // Overflow is caught
    assertThrows[IndexOutOfBoundsException] { insertAt(-1) }
    assertThrows[IndexOutOfBoundsException] { insertAt(traver.size + 10) }

    val xs = new Iterable[Int] { def iterator = Iterator(42); override def knownSize = 10 }
    assertThrows[IllegalStateException] { traver.tap(_.insertAll(0, xs)) }
  }

  @Test
  def testInsertTop(): Unit = {
    val buffer = ArrayBuffer.empty[Int]
    val els = 0 until 100

    for (i <- els) buffer.insert(0, i)

    assertEquals(ArrayBuffer(els.reverse: _*), buffer)
  }

  @Test
  def testInsertEnd(): Unit = {
    val buffer = ArrayBuffer.empty[Int]
    val els = 0 until 100

    for (i <- els) buffer.insert(i, i)

    assertEquals(ArrayBuffer(els: _*), buffer)
  }

  @Test
  def testPrepend(): Unit = {
    val buffer = ArrayBuffer.empty[Int]
    val els = 0 until 100

    for (i <- els) i +=: buffer

    assertEquals(ArrayBuffer(els.reverse: _*), buffer)
  }

  @Test
  def testFlatMapInPlace(): Unit = {
    val xs = ArrayBuffer(3, 4, 5)
    val ys = List(-1, -2, -3, -4, -5, -6)

    val res = xs.flatMapInPlace(i => ys take i)

    assertEquals(ArrayBuffer(-1, -2, -3, -1, -2, -3, -4, -1, -2, -3, -4, -5), res)
  }

  @Test
  def testFilterInPlace(): Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 100).filterInPlace(_ => false))
    assertEquals(ArrayBuffer.range(0, 100), ArrayBuffer.range(0, 100).filterInPlace(_ => true))
    assertEquals(ArrayBuffer.range(start = 0, end = 100, step = 2), ArrayBuffer.range(0, 100).filterInPlace(_ % 2 == 0))
    assertEquals(ArrayBuffer.range(start = 1, end = 100, step = 2), ArrayBuffer.range(0, 100).filterInPlace(_ % 2 != 0))
  }

  @Test
  def testTakeInPlace(): Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().takeInPlace(10))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).takeInPlace(-1))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).takeInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 100).takeInPlace(10))
  }

  @Test
  def testTakeRightInPlace(): Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().takeRightInPlace(10))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).takeRightInPlace(-1))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).takeRightInPlace(10))
    assertEquals(ArrayBuffer.range(90, 100), ArrayBuffer.range(0, 100).takeRightInPlace(10))
  }

  @Test
  def testTakeWhileInPlace(): Unit = {
    assertEquals(ArrayBuffer(), ListBuffer[Int]().takeWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer.range(0, 10), ListBuffer.range(0, 10).takeWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer.range(0, 50), ListBuffer.range(0, 100).takeWhileInPlace(_ < 50))
  }

  @Test
  def testDropInPlace(): Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().dropInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).dropInPlace(-1))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).dropInPlace(10))
    assertEquals(ArrayBuffer.range(10, 100), ArrayBuffer.range(0, 100).dropInPlace(10))
  }

  @Test
  def testDropRightInPlace(): Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer().dropRightInPlace(10))
    assertEquals(ArrayBuffer.range(0, 10), ArrayBuffer.range(0, 10).dropRightInPlace(-1))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).dropRightInPlace(10))
    assertEquals(ArrayBuffer.range(0, 90), ArrayBuffer.range(0, 100).dropRightInPlace(10))
  }

  @Test
  def testDropWhileInPlace(): Unit = {
    assertEquals(ArrayBuffer(), ArrayBuffer[Int]().dropWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer(), ArrayBuffer.range(0, 10).dropWhileInPlace(_ < 50))
    assertEquals(ArrayBuffer.range(50, 100), ArrayBuffer.range(0, 100).dropWhileInPlace(_ < 50))
  }

  @Test
  def testRemove(): Unit = {
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

  @Test
  def testRemoveWithNegativeIndex(): Unit =
    assertThrows[IndexOutOfBoundsException](ArrayBuffer(0, 1, 2).remove(-1))

  @Test
  def testRemoveWithTooLargeIndex(): Unit =
    assertThrows[IndexOutOfBoundsException](ArrayBuffer(0).remove(1))

  @Test
  def testRemoveMany(): Unit = {
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

  @Test
  def testRemoveManyWithNegativeIndex(): Unit =
    assertThrows[IndexOutOfBoundsException](ArrayBuffer(0, 1, 2).remove(index = -1, count = 1))

  @Test
  def testRemoveManyWithTooLargeIndex(): Unit =
    assertThrows[IndexOutOfBoundsException](ArrayBuffer(0).remove(index = 1, count = 1))

  @Test
  def testRemoveManyWithNegativeCount(): Unit =
    assertThrows[IllegalArgumentException](ArrayBuffer(0).remove(index = 0, count = -1))

  @Test
  def testRemoveManyWithTooLargeCount(): Unit =
    assertThrows[IndexOutOfBoundsException](ArrayBuffer(0).remove(index = 0, count = 100))

  @nowarn("cat=deprecation")
  @Test
  def testTrimStart(): Unit = {
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

  @nowarn("cat=deprecation")
  @Test
  def testTrimEnd(): Unit = {
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
  def testPatch(): Unit = {
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
  def testPatchInPlace(): Unit = {
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

  @Test
  def testApplyWhenEmpty(): Unit =
    assertThrows[IndexOutOfBoundsException](new ArrayBuffer().apply(0))

  @Test
  def testApplyAfterClearing(): Unit = assertThrows[IndexOutOfBoundsException] {
    val buffer = ArrayBuffer(1, 2, 3)
    buffer.clear()
    buffer(0)
  }

  @Test
  def testUpdateWhenEmpty(): Unit =
    assertThrows[IndexOutOfBoundsException](new ArrayBuffer().update(0, 100))

  @Test
  def testUpdateAfterClearing(): Unit = assertThrows[IndexOutOfBoundsException] {
    val buffer = ArrayBuffer(1, 2, 3)
    buffer.clear()
    buffer.update(0, 100)
  }

  @Test
  def testClear(): Unit = {
    val buffer = ArrayBuffer(1, 2, 3)
    buffer.clear()

    assertEquals(0, buffer.size)
  }

  @Test
  def testSortInPlace(): Unit = {
    val buffer = ArrayBuffer(3, 2, 1)
    buffer.sortInPlace()

    assertEquals(ArrayBuffer(1, 2, 3), buffer)
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testMapResult(): Unit = {
    val buffer = ArrayBuffer(3, 2, 1)

    val builder = buffer.mapResult(_.mkString(","))

    buffer.prepend(4)

    assertEquals(builder.result(), "4,3,2,1")
  }

  @Test
  def emptyIteratorDropOneMustBeEmpty(): Unit = {
    assertThrows[NoSuchElementException](new ArrayBuffer[Int].iterator.drop(1).next())
  }

  @Test
  def t11114_ArrayBufferPatch(): Unit = {
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

  @Test def t11417_sortInPlace(): Unit = {
    val a = ArrayBuffer(5,4,3,2,1)
    a.dropRightInPlace(2)
    a.sortInPlace()
    assertEquals(List(3,4,5), a)
  }

  @Test def trimToSize(): Unit = {
    val b = ArrayBuffer(1,2,3)
    assertEquals(16, b.array.length)
    b ++= (1 to 1000)
    assertEquals(1024, b.array.length)
    b.remove(200, 803)
    b.trimToSize()
    assertEquals(200, b.array.length)
  }

  @Test def t11482_allowNegativeInitialSize(): Unit =
    new ArrayBuffer(-1)

  @Test def t12176_indexOutOfBoundsExceptionMessage(): Unit = {
    def newAB = ArrayBuffer('a', 'b', 'c')
    def iiobe[A](f: => A, msg: String) =
      try {
        f; fail("Did not throw IndexOutOfBoundsException")
      } catch {
        case ex: IndexOutOfBoundsException => assertEquals(ex.getMessage, msg)
      }

    iiobe( newAB.insert(-1, 'x'), "-1 is out of bounds (min 0, max 2)" )
    iiobe( newAB.insertAll(-2, Array('x', 'y', 'z')), "-2 is out of bounds (min 0, max 2)" )
    iiobe( newAB.update(-3, 'u'), "-3 is out of bounds (min 0, max 2)" )
    iiobe( newAB.update(-1, 'u'), "-1 is out of bounds (min 0, max 2)" )
    iiobe( newAB.update(3, 'u'), "3 is out of bounds (min 0, max 2)" )
    iiobe( newAB(3), "3 is out of bounds (min 0, max 2)" )
    iiobe( newAB.remove(3), "3 is out of bounds (min 0, max 2)" )
    iiobe( newAB.remove(2, 2), "3 is out of bounds (min 0, max 2)" )
  }

  // scala/bug#7880 and scala/bug#12464
  @Test def `ensureSize must terminate and have limits`(): Unit = {
    val sut = getMethodAccessible[ArrayBuffer.type]("resizeUp")
    def resizeUp(arrayLen: Int, targetLen: Int): Int = sut.invoke(ArrayBuffer, arrayLen, targetLen).asInstanceOf[Int]

    // check termination and correctness
    assertTrue(7 < ArrayBuffer.DefaultInitialSize) // condition of test
    assertEquals(-1, resizeUp(7, 0))
    assertEquals(-1, resizeUp(Int.MaxValue, 0))
    assertEquals(ArrayBuffer.DefaultInitialSize, resizeUp(-1, 0)) // no check of arrayLen
    assertEquals(ArrayBuffer.DefaultInitialSize, resizeUp(7, 10))
    assertEquals(VM_MaxArraySize, resizeUp(Int.MaxValue / 2, Int.MaxValue / 2 + 1)) // `MaxValue / 2` cannot be doubled
    assertEquals(VM_MaxArraySize / 2 * 2, resizeUp(VM_MaxArraySize / 2, VM_MaxArraySize / 2 + 1)) // `VM_MaxArraySize / 2` can be doubled
    assertEquals(VM_MaxArraySize, resizeUp(Int.MaxValue / 2, Int.MaxValue / 2 + 2))
    assertEquals(-1, resizeUp(Int.MaxValue / 2 + 1, Int.MaxValue / 2 + 1)) // no resize needed
    assertEquals(VM_MaxArraySize, resizeUp(Int.MaxValue / 2 + 1, Int.MaxValue / 2 + 2))
    assertEquals(VM_MaxArraySize, resizeUp(Int.MaxValue / 2, VM_MaxArraySize))
    assertEquals(123456*2+33, resizeUp(123456, 123456*2+33)) // use targetLen if it's larger than double the current

    // check limits
    def rethrow(op: => Any): Unit =
      try op catch { case e: InvocationTargetException => throw e.getCause }
    def checkExceedsMaxInt(targetLen: Int): Unit = {
      assertThrows[Exception](rethrow(resizeUp(0, targetLen)),
        _ == s"Overflow while resizing array of array-backed collection. Requested length: $targetLen; current length: 0; increase: $targetLen")
    }
    def checkExceedsVMArrayLimit(targetLen: Int): Unit =
      assertThrows[Exception](rethrow(resizeUp(0, targetLen)),
        _ == s"Array of array-backed collection exceeds VM length limit of $VM_MaxArraySize. Requested length: $targetLen; current length: 0")

    checkExceedsMaxInt(-1)
    checkExceedsMaxInt(Int.MaxValue + 1: @nowarn)
    checkExceedsVMArrayLimit(Int.MaxValue)
    checkExceedsVMArrayLimit(Int.MaxValue - 1)
  }

  @Test def `array capacity must follow sizing`(): Unit = {
    val sut = getMethodAccessible[ArrayBuffer[_]]("array")
    def array[A](buf: ArrayBuffer[A]) = sut.invokeAs[Array[AnyRef]](buf)

    assertEquals(16, array(ArrayBuffer(1, 2, 3)).length)
    locally {
      val buf = new ArrayBuffer(42)
      assertEquals(42, array(buf).length)
    }
    // hint over 2x grows to exact
    locally {
      val buf = new ArrayBuffer(42)
      buf.sizeHint(100)
      assertEquals(100, array(buf).length)
    }
    // hint under 2x grows by 2x
    locally {
      val buf = new ArrayBuffer(42)
      buf.sizeHint(64)
      assertEquals(84, array(buf).length)
    }
    locally {
      val buf = new ArrayBuffer[Int](42)
      Iterator.continually(42).take(42).foreach(buf.addOne)
      buf.remove(buf.size - 1)
      assertEquals(42, array(buf).length)
    }
    // double capacity
    locally {
      val buf = new ArrayBuffer[Int](42)
      Iterator.continually(42).take(42).foreach(buf.addOne)
      buf.addOne(17)
      assertEquals(84, array(buf).length)
    }
    // exact capacity larger than double
    locally {
      val buf = new ArrayBuffer[Int](42)
      Iterator.continually(42).take(42).foreach(buf.addOne)
      buf.addAll(ArrayBuffer.from(1 to 43))
      assertEquals(85, array(buf).length)
    }
    locally {
      val buf = new ArrayBuffer[Int](42)
      Iterator.continually(42).take(42).foreach(buf.addOne)
      buf.clearAndShrink(42)
      assertEquals(42, array(buf).length)
    }
    // exact shrink
    locally {
      val buf = new ArrayBuffer[Int](42)
      Iterator.continually(42).take(42).foreach(buf.addOne)
      buf.clearAndShrink(17)
      assertEquals(17, array(buf).length)
    }
  }

  @Test def `downsizing must size down`(): Unit = {
    val sut = getMethodAccessible[ArrayBuffer.type]("resizeDown")
    def resizeDown(arrayLen: Int, targetLen: Int): Int = sut.invoke(ArrayBuffer, arrayLen, targetLen).asInstanceOf[Int]
    assertTrue(17 > ArrayBuffer.DefaultInitialSize)  // condition of test
    assertEquals(3, resizeDown(17, 3))
    assertEquals(30, resizeDown(64, 30))
    assertEquals(17, resizeDown(42, 17))
    assertEquals(-1, resizeDown(42, 50))
    assertEquals(0, resizeDown(42, -1))
  }

  // scala/bug#12121
  @Test
  def insertAll_self(): Unit = {
    val buf = ArrayBuffer(1, 2, 3)
    buf.insertAll(1, buf)
    assertSameElements(List(1, 1, 2, 3, 2, 3), buf)
  }

  // scala/bug#12284
  @Test
  def viewConsistency(): Unit = {
    def check[U](op: ArrayBuffer[Int] => U): Unit = {
      val buf = ArrayBuffer.from(1 to 50)
      val view = buf.view
      op(buf)
      assertSameElements(buf, view)
    }

    check(_.clear())
    check(_.dropRightInPlace(30))
    check(_.dropInPlace(30))
    check(_ ++= (1 to 100))
    check(_.insertAll(1, 1 to 100))
  }

  @Test def `drop right when empty is empty`: Unit = {
    assertTrue(ArrayBuffer().dropRight(1).isEmpty)
  }

  @Test def `refuse to build from an overly huge thing`: Unit = {
    val bld = ArrayBuffer.newBuilder[String]
    assertThrows[Exception](bld.sizeHint(Int.MaxValue), _.contains("exceeds VM length limit"))
    bld.addOne("hello, world")
    assertTrue(bld.result().contains("hello, world"))
  }
}
