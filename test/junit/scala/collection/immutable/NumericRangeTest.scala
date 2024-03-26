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

  @Test
  def numericRangeContains() = {
    def check(r: Range): List[Int] = {
      r.filterNot(r.contains).toList
    }

    val testIncreaseCase = Int.MinValue until 1092265081 by 359972081

    assertEquals(Nil, check(testIncreaseCase))
    assertEquals(Nil, check(testIncreaseCase.drop(1)))
    assertEquals(Nil, check(testIncreaseCase.drop(2)))
    assertEquals(Nil, check(testIncreaseCase.drop(3)))
    assertEquals(Nil, check(testIncreaseCase.start to testIncreaseCase.last by testIncreaseCase.step))
    assertEquals(Nil, check(testIncreaseCase.inclusive))


    val testDecreaseCase = Int.MaxValue until 1092265081 by -359972081

    assertEquals(Nil, check(testDecreaseCase))
    assertEquals(Nil, check(testDecreaseCase.drop(1)))
    assertEquals(Nil, check(testDecreaseCase.drop(2)))
    assertEquals(Nil, check(testDecreaseCase.drop(3)))
    assertEquals(Nil, check(testDecreaseCase.start to testIncreaseCase.last by testIncreaseCase.step))
    assertEquals(Nil, check(testDecreaseCase.inclusive))
  }

  @Test
  def numericRangeWithMoreThanMaxIntTake() = {
    val start = BigInt(0)
    val step = BigInt(1)
    val end = BigInt(Int.MaxValue) * 5

    // evaluation of length causes IllegalArgumentException because it cannot be represented as Int number
    // Yet using `take` should be independent of evaluating length when it's not necessary.
    assertThrows[IllegalArgumentException]((start until end by step).length)
    val range = start until end by step

    val take = 100
    val smallChunkOfRange = range.take(take)
    val expected = start until BigInt(100) by step
    assertTrue(smallChunkOfRange equals expected)
    assertTrue(smallChunkOfRange.length == take)
  }

  @Test
  def numericRangeWithMoreThanMaxIntDrop() = {
    val start = BigInt(0)
    val step = BigInt(1)
    val toAddToMaxInt = 50
    val end = BigInt(Int.MaxValue) + toAddToMaxInt

    val drop = Int.MaxValue

    // evaluation of length causes IllegalArgumentException because it cannot be represented as Int number
    // Yet using `drop` should be independent of evaluating length when it's not necessary.
    assertThrows[IllegalArgumentException]((start until end by step).length)
    val range = start until end by step

    val smallChunkOfRange = range.drop(drop)
    val expected = BigInt(Int.MaxValue) until end by step
    assertTrue(smallChunkOfRange equals expected)
    assertTrue(smallChunkOfRange.length == toAddToMaxInt)
  }

  @Test
  def numericRangeSmallTypesDrop() = {
    val byteStart: Byte = Byte.MinValue
    val byteEnd: Byte = Byte.MaxValue
    val drop = 10

    val byteRange = NumericRange(byteStart, byteEnd, (1: Byte))
    val byteRangeChunk = byteRange.drop(drop)
    assertTrue(byteRangeChunk.length == byteRange.length - drop)
    assertTrue(byteRangeChunk.end == byteEnd)
    assertTrue(byteRangeChunk.start == (byteStart + drop.toByte))

    val shortStart: Short = Short.MinValue
    val shortEnd: Short = Short.MaxValue

    val shortRange = NumericRange(shortStart, shortEnd, (1: Short))
    val shortRangeChunk = shortRange.drop(drop)
    assertTrue(shortRangeChunk.length == shortRange.length - drop)
    assertTrue(shortRangeChunk.end == shortEnd)
    assertTrue(shortRangeChunk.start == (shortStart + drop.toShort))
  }

  @Test
  def numericRangeSmallTypesTake() = {
    val byteStart: Byte = Byte.MinValue
    val byteEnd: Byte = Byte.MaxValue
    val take = 10

    val byteRange = NumericRange(byteStart, byteEnd, (1: Byte))
    val byteRangeChunk = byteRange.take(take)
    assertTrue(byteRangeChunk.length == take)
    assertTrue(byteRangeChunk.end == byteStart + take.toByte - 1)
    assertTrue(byteRangeChunk.start == byteStart)

    val shortStart: Short = Short.MinValue
    val shortEnd: Short = Short.MaxValue

    val shortRange = NumericRange(shortStart, shortEnd, (1: Short))
    val shortRangeChunk = shortRange.take(take)
    assertTrue(shortRangeChunk.length == take)
    assertTrue(shortRangeChunk.end == shortStart + take.toShort - 1)
    assertTrue(shortRangeChunk.start == shortStart)
  }

  @Test
  def takeAndDropForCustomTypes() = {
    import NumericRangeTest._

    // smaller than Int
    val start = NumericWrapper(Byte.MinValue)
    val step = NumericWrapper(1: Byte)
    val end = NumericWrapper(Byte.MaxValue)
    val range = NumericRange.inclusive(start, end, step)

    val amount = 20

    val taken = range.take(amount)
    val dropped = range.drop(amount)

    assertTrue(taken.start == range.start && taken.length == amount)
    assertTrue(dropped.end == range.end && dropped.length == range.length - amount)

    // Int
    val startInt = NumericWrapper(Int.MinValue)
    val endInt = NumericWrapper(Int.MaxValue)
    val stepInt = NumericWrapper(1)

    val intRange = NumericRange.inclusive(startInt, endInt, stepInt)

    val amountForInts = 40

    val takenInts = intRange.take(amountForInts)
    val droppedInts = intRange.drop(amountForInts)

    assertTrue(takenInts.start == intRange.start && takenInts.length == amountForInts)
    assertTrue(droppedInts.end == intRange.end && droppedInts.start.value == intRange.start.value + (amountForInts * intRange.step.value))

    // Larger than int

    val startLong = NumericWrapper(Long.MinValue)
    val stepLong = NumericWrapper(1L)
    val endLong = NumericWrapper(Long.MaxValue)

    val longRange = NumericRange.inclusive(startLong, endLong, stepLong)

    val amountForLongs = 50

    val takenLongs = longRange.take(amountForLongs)
    val droppedLongs = longRange.drop(amountForLongs)

    assertTrue(takenLongs.start == longRange.start && takenLongs.length == amountForLongs)
    assertTrue(droppedLongs.end == longRange.end && droppedLongs.start.value == longRange.start.value + (amountForLongs * longRange.step.value))
  }

  @Test
  def wideIntNumericRangeTakeAndDrop() = {
    val start = Int.MinValue
    val end = Int.MaxValue
    val step = 1

    val amount = 50

    val range = NumericRange(start, end, step)

    assertThrows[IllegalArgumentException](range.length)
    assertTrue(range.take(amount).length == amount)
    val dropped = range.drop(amount)
    assertTrue(dropped.end == range.end && dropped.step == range.step && dropped.start == (amount * step) + range.start)
  }

  @Test
  def wideNegativeNumericRangeUntilZeroShouldNotOverflow() = {
    val start = Int.MinValue
    val end = 0
    val step = 1

    val amount = 50

    val range = NumericRange.inclusive(start, end, step)

    assertThrows[IllegalArgumentException](range.length)

    val taken = range.take(amount)
    assertTrue(taken.length == amount)
    assertTrue(taken.start == range.start && taken.step == range.step)


    val dropped = range.drop(amount)
    assertTrue(dropped.end == range.end && dropped.step == range.step && dropped.start == (amount * step) + range.start)
  }

  @Test
  def indexOfAndLastIndexOfShouldBeCorrect(): Unit = {

    // For a range, because every value exists at most once, without the optional from/end param,
    // indexOf and lastIndexOf should be equal.

    // General checks.
    assertEquals(0, NumericRange(0, 1, 1).indexOf(0))
    assertEquals(0, NumericRange(0, 1, 1).lastIndexOf(0))

    assertEquals(0, NumericRange.inclusive(0, 0, 1).indexOf(0))
    assertEquals(0, NumericRange.inclusive(0, 0, 1).lastIndexOf(0))

    assertEquals(300, NumericRange(0, Int.MaxValue, 1).indexOf(300))
    assertEquals(300, NumericRange(0, Int.MaxValue, 1).lastIndexOf(300))

    assertEquals(Int.MaxValue - 1, NumericRange(0, Int.MaxValue, 1).indexOf(Int.MaxValue - 1))
    assertEquals(Int.MaxValue - 1, NumericRange(0, Int.MaxValue, 1).lastIndexOf(Int.MaxValue - 1))

    assertEquals(300 / 5, NumericRange(0, Int.MaxValue, 5).indexOf(300))
    assertEquals(300 / 5, NumericRange(0, Int.MaxValue, 5).lastIndexOf(300))

    assertEquals(700, NumericRange(-1000, 0, 1).indexOf(-300))
    assertEquals(700, NumericRange(-1000, 0, 1).lastIndexOf(-300))

    assertEquals(350, NumericRange(-1000, 0, 2).indexOf(-300))
    assertEquals(350, NumericRange(-1000, 0, 2).lastIndexOf(-300))

    // If a value is below or over the limits of the range, or fall between steps, should be -1
    assertEquals(-1, NumericRange(0, 10, 1).indexOf(20))
    assertEquals(-1, NumericRange(0, 10, 1).lastIndexOf(20))

    assertEquals(-1, NumericRange(0, 10, 2).indexOf(20))
    assertEquals(-1, NumericRange(0, 10, 2).lastIndexOf(20))

    assertEquals(-1, NumericRange(0, 10, 1).indexOf(-5))
    assertEquals(-1, NumericRange(0, 10, 1).lastIndexOf(-5))

    assertEquals(-1, NumericRange(0, 10, 2).indexOf(1))
    assertEquals(-1, NumericRange(0, 10, 2).lastIndexOf(1))

    assertEquals(-1, NumericRange(0, 10, 2).indexOf(3))
    assertEquals(-1, NumericRange(0, 10, 2).lastIndexOf(3))

    // indexOf should return -1 if the index is less than the from value
    assertEquals(300, NumericRange(0, Int.MaxValue, 1).indexOf(300, 100))
    assertEquals(-1, NumericRange(0, Int.MaxValue, 1).indexOf(300, 400))
    assertEquals(300, NumericRange(0, Int.MaxValue, 1).indexOf(300, 300))

    assertEquals(150, NumericRange(0, Int.MaxValue, 2).indexOf(300, 140))
    assertEquals(-1, NumericRange(0, Int.MaxValue, 2).indexOf(300, 160))

    // lastIndexOf should return -1 if the index is more than the end value
    assertEquals(-1, NumericRange(0, Int.MaxValue, 1).lastIndexOf(300, 100))
    assertEquals(300, NumericRange(0, Int.MaxValue, 1).lastIndexOf(300, 400))
    assertEquals(300, NumericRange(0, Int.MaxValue, 1).lastIndexOf(300, 300))

    assertEquals(-1, NumericRange(0, Int.MaxValue, 2).lastIndexOf(300, 140))
    assertEquals(150, NumericRange(0, Int.MaxValue, 2).lastIndexOf(300, 160))

    // Sanity check - does it work with other types.
    assertEquals(300 / 5, NumericRange(0L, Int.MaxValue.toLong, 5L).indexOf(300L))
    assertEquals(300 / 5, NumericRange(0L, Int.MaxValue.toLong, 5L).lastIndexOf(300L))

    assertEquals(300 / 5, NumericRange(BigInt(0), BigInt(Int.MaxValue), BigInt(5)).indexOf(BigInt(300)))
    assertEquals(300 / 5, NumericRange(BigInt(0), BigInt(Int.MaxValue), BigInt(5)).lastIndexOf(BigInt(300)))

    // indexOf different type returns -1
    assertEquals(-1, NumericRange(0L, Int.MaxValue.toLong, 5L).indexOf[AnyVal](300))
    assertEquals(-1, NumericRange(0L, Int.MaxValue.toLong, 5L).lastIndexOf[AnyVal](300))

    /* Attempting an indexOf of a Range with more than Int.MaxValue elements always throws an error.
    Not ideal, but this tests whether backwards compatibility is conserved. */
    assertThrows[IllegalArgumentException](NumericRange(0L, Int.MaxValue.toLong + 1, 1L).indexOf(300L))
    assertThrows[IllegalArgumentException](NumericRange(0L, Int.MaxValue.toLong + 1, 1L).lastIndexOf(300L))

    assertThrows[IllegalArgumentException](NumericRange(Int.MinValue, Int.MaxValue, 1).indexOf(300))
    assertThrows[IllegalArgumentException](NumericRange(Int.MinValue, Int.MaxValue, 1).lastIndexOf(300))
  }

}

object NumericRangeTest {

  private case class NumericWrapper[T](value: T)
  private object NumericWrapper {
    implicit def isIntegral[T](implicit tNum: Integral[T]): Integral[NumericWrapper[T]] = new Integral[NumericWrapper[T]] {
      override def quot(x: NumericWrapper[T], y: NumericWrapper[T]): NumericWrapper[T] =
        NumericWrapper(tNum.quot(x.value, y.value))

      override def rem(x: NumericWrapper[T], y: NumericWrapper[T]): NumericWrapper[T] =
        NumericWrapper(tNum.rem(x.value, y.value))

      override def plus(x: NumericWrapper[T], y: NumericWrapper[T]): NumericWrapper[T] =
        NumericWrapper(tNum.plus(x.value, y.value))

      override def minus(x: NumericWrapper[T], y: NumericWrapper[T]): NumericWrapper[T] =
        NumericWrapper(tNum.minus(x.value, y.value))

      override def times(x: NumericWrapper[T], y: NumericWrapper[T]): NumericWrapper[T] =
        NumericWrapper(tNum.times(x.value, y.value))

      override def negate(x: NumericWrapper[T]): NumericWrapper[T] =
        NumericWrapper(tNum.negate(x.value))

      override def fromInt(x: Int): NumericWrapper[T] =
        NumericWrapper(tNum.fromInt(x))

      override def parseString(str: String): Option[NumericWrapper[T]] =
        tNum.parseString(str).map(NumericWrapper.apply)

      override def toInt(x: NumericWrapper[T]): Int =
        tNum.toInt(x.value)

      override def toLong(x: NumericWrapper[T]): Long =
        tNum.toLong(x.value)

      override def toFloat(x: NumericWrapper[T]): Float =
        tNum.toFloat(x.value)

      override def toDouble(x: NumericWrapper[T]): Double =
        tNum.toDouble(x.value)

      override def compare(x: NumericWrapper[T], y: NumericWrapper[T]): Int = tNum.compare(x.value, y.value)
    }
  }
}
