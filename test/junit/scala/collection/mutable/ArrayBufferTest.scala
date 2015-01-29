package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Assert, Test}

import scala.tools.testing.AssertUtil

/* Test for SI-9043 */
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
    Assert.assertEquals(ArrayBuffer(1, 3, 6, 9, 2, 4, 5, 7), insertAt(0))

    // Insert in the middle
    Assert.assertEquals(ArrayBuffer(2, 4, 1, 3, 6, 9, 5, 7), insertAt(2))

    // No strange last position weirdness
    Assert.assertEquals(ArrayBuffer(2, 4, 5, 7, 1, 3, 6, 9), insertAt(traver.size))

    // Overflow is caught
    AssertUtil.assertThrows[IndexOutOfBoundsException] { insertAt(-1) }
    AssertUtil.assertThrows[IndexOutOfBoundsException] { insertAt(traver.size + 10) }
  }
}
