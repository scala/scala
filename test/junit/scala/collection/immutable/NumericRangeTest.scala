package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class NumericRangeTest {

  @Test
  def t11152: Unit = {
    val x = (BigDecimal(1) to BigDecimal(s"1.${"0" * 32}1") by BigDecimal(s"0.${"0" * 33}1")).iterator.toList.length
    assertEquals(11, x)
  }

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
}