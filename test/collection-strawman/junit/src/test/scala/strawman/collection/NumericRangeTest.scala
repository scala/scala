package strawman.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.NumericRange

@RunWith(classOf[JUnit4])
class NumericRangeTest {

  @Test
  def emptyIterator(): Unit = {
    assertFalse(NumericRange(1, 0, 1).iterator().hasNext)
    assertFalse(NumericRange(0, 10, -1).iterator().hasNext)
  }

  @Test
  def nonEmptyIterator(): Unit = {
    val it = NumericRange(0, 3, 1).iterator()

    assertTrue(it.hasNext)
    assertEquals(0, it.next())
    assertTrue(it.hasNext)
    assertEquals(1, it.next())
    assertTrue(it.hasNext)
    assertEquals(2, it.next())
    assertFalse(it.hasNext)
  }
}