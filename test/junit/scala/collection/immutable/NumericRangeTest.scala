package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class NumericRangeTest {

  @Test
  def emptyiterator: Unit = {
    assertFalse(NumericRange(1, 0, 1).iterator.hasNext)
    assertFalse(NumericRange(0, 10, -1).iterator.hasNext)
  }

  @Test
  def nonEmptyiterator: Unit = {
    val it = NumericRange(0, 3, 1).iterator

    assertTrue(it.hasNext)
    assertEquals(0, it.next())
    assertTrue(it.hasNext)
    assertEquals(1, it.next())
    assertTrue(it.hasNext)
    assertEquals(2, it.next())
    assertFalse(it.hasNext)
  }

  @Test
  def t11163_BigDecimalSum: Unit = {
    val x = (BigDecimal(1) to BigDecimal(3) by 1).sum
    assertEquals(BigDecimal(6), x)
  }
}
