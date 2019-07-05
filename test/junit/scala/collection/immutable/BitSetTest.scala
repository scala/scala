package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BitSetTest {
  @Test
  def min(): Unit = {
    assertEquals(1, BitSet(1, 2, 3).min)
    assertEquals(3, BitSet(1, 2, 3).min(implicitly[Ordering[Int]].reverse))

    try {
      BitSet.empty.min
      fail("expect UnsupportedOperationException")
    } catch {
      case e: UnsupportedOperationException =>
        assertEquals("empty.min", e.getMessage)
    }
  }

  @Test
  def max(): Unit = {
    assertEquals(3, BitSet(1, 2, 3).max)
    assertEquals(1, BitSet(1, 2, 3).max(implicitly[Ordering[Int]].reverse))

    try {
      BitSet.empty.max
      fail("expect UnsupportedOperationException")
    } catch {
      case e: UnsupportedOperationException =>
        assertEquals("empty.max", e.getMessage)
    }
  }
}
