package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class NumericRangeTest {

  @Test
  def emptyiterator(): Unit = {
    assertFalse(NumericRange(1, 0, 1).iterator.hasNext)
    assertFalse(NumericRange(0, 10, -1).iterator.hasNext)
  }

  @Test
  def nonEmptyiterator(): Unit = {
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
  def t11163_BigDecimalSum(): Unit = {
    val x = (BigDecimal(1) to BigDecimal(3) by 1).sum
    assertEquals(BigDecimal(6), x)
  }

  @Test
  def t11152_BigDecimalMakesProgress(): Unit = {
    // Overflow case with default MathContext
    val a = BigDecimal(1)
    val b = a + BigDecimal("1e-30")
    val c = BigDecimal("1e-38")
    assertThrows[IllegalArgumentException]((a to b by c).length < 0)    // Force evaluation of `length`

    // Same math, greater precision--should be no overflow
    val aa = BigDecimal(1, new java.math.MathContext(40))
    val bb = aa + BigDecimal("1e-30")
    assertEquals(
      Some(BigDecimal("1." + "0"*37 + "1")),
      (aa to bb by c).drop(1).headOption,
    )

    // Make sure that positive/negative switch works okay at the limit of precision
    val mc2 = new java.math.MathContext(2)
    val nr = (BigDecimal("-7", mc2) to BigDecimal("7", mc2) by BigDecimal("0.1", mc2))
    val upscale = -70 to 70 by 1
    assertEquals(nr.length, upscale.length)
    assertTrue(
      (nr zip upscale).forall{ case (x, x10) => x*10 == x10 }
    )

    // Make sure that we catch it right past the limit of precision
    assertThrows[IllegalArgumentException] {
      val mc2 = new java.math.MathContext(2)
      val nr = (BigDecimal("-7", mc2) to BigDecimal("7", mc2) by BigDecimal("0.01", mc2))
      val upscale = -700 to 700 by 1
      nr.zip(upscale).forall { case (x, x100) => x*100 == x100 }
    }
  }

  @Test
  def numericRangeIsEmpty(): Unit = {
    //Test Positive Step
    assertFalse((1 to 9).isEmpty)
    assertTrue((9 to 1).isEmpty)

    //Test Negative step
    assertFalse(Range.inclusive(9, 1, -1).isEmpty)
    assertTrue(Range.inclusive(1, 9, -1).isEmpty)

    //Test for WideNumericRanges Positive Step
    assertFalse((0L to Int.MaxValue.toLong).isEmpty)
    assertFalse((0L to Long.MaxValue).isEmpty)
    assertTrue((Long.MaxValue to Int.MaxValue.toLong).isEmpty)

    //Test for WideNumericRanges Negative Steps
    assertFalse(NumericRange.inclusive(Long.MaxValue, Int.MaxValue.toLong, -1).isEmpty)
    assertTrue(NumericRange(Long.MaxValue, Long.MaxValue, -1).isEmpty)
  }

  @Test
  def smallIncrementCount(): Unit = {
    case class TestRange(start: BigDecimal, end: BigDecimal, step: BigDecimal, inclusive: Boolean = false)
    def foldListIncrement(rangeTest: TestRange): List[BigDecimal] = {
      List.unfold(rangeTest. start) { prec =>
        Option.when(
          if (rangeTest.step > 0)
            prec < rangeTest.end
          else
            prec > rangeTest.end
          )(prec -> (prec + rangeTest.step))
      } ::: (if (rangeTest.inclusive) List(rangeTest.end) else List.empty[BigDecimal])
    }

    def createRangeFromRangeTest(rangeTest: TestRange) =
      if (rangeTest.inclusive)
        Range.BigDecimal.inclusive(rangeTest.start, rangeTest.end, rangeTest.step)
      else
        Range.BigDecimal(rangeTest.start, rangeTest.end, rangeTest.step)

    def negate(v: BigDecimal) = v.*(BigDecimal(-1))
    def double(v: BigDecimal) = v.*(BigDecimal(2))
    def negateDouble(v: BigDecimal) = negate(double(v))

    val inclusiveValue = BigDecimal(1E-34)

    List[TestRange](
      TestRange(BigDecimal(-2E-34), BigDecimal(1E-64), BigDecimal(1E-34)), //Negative to positive Range, positive step, exclusive
      TestRange(negateDouble(inclusiveValue), inclusiveValue, inclusiveValue, inclusive = true), //Negative to positive Range, positive step, inclusive
      TestRange(BigDecimal(1E-64), BigDecimal(-2E-34), BigDecimal(-1E-34)), //Positive to negative range, negative step, exclusive
      TestRange(inclusiveValue, negateDouble(inclusiveValue), negate(inclusiveValue), inclusive = true), //Positive to negative range, negative step, inclusive
      TestRange(BigDecimal(1E-64), BigDecimal(2E-34), BigDecimal(1E-34)), //Positive to positive, positive step, exclusive
      TestRange(inclusiveValue, double(inclusiveValue), inclusiveValue, inclusive = true), //Positive to positive, positive step, inclusive
      TestRange(BigDecimal(2E-34), BigDecimal(1E-64), BigDecimal(-1E-34)), //Positive to positive, negative step, exclusive
      TestRange(double(inclusiveValue), inclusiveValue, negate(inclusiveValue), inclusive = true), //Positive to positive, negative step, inclusive
      TestRange(BigDecimal(-1E-64), BigDecimal(-2E-34), BigDecimal(-1E-34)), //Negative to negative, negative step, exclusive
      TestRange(negate(inclusiveValue), negateDouble(inclusiveValue), negate(inclusiveValue), inclusive = true), //Negative to negative, negative step, inclusive
      TestRange(BigDecimal(-2E-34), BigDecimal(-1E-64), BigDecimal(1E-34)), //Negative to negative, positive step, exclusive
      TestRange(negateDouble(inclusiveValue), negate(inclusiveValue), inclusiveValue), //Negative to negative, positive step, inclusive
      TestRange(BigDecimal(9.474), BigDecimal(49.474), BigDecimal(1)) //BigDecimal in "large" increments
      ).foreach(tr => assertEquals(foldListIncrement(tr).length, createRangeFromRangeTest(tr).length))
  }
}