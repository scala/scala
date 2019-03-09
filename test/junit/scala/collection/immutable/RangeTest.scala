package scala.collection.immutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testing.AssertUtil

@RunWith(classOf[JUnit4])
class RangeTest {
  import Assert._
  import AssertUtil._

  @Test
  def sorted(): Unit = {
    val reverseOrd = Ordering.Int.reverse

    val x1 = 1 to 10 by 2
    assertSame(x1, x1.sorted)
    assertEquals(List(9, 7, 5, 3, 1), x1.sorted(reverseOrd))

    val x2 = 10 to 1 by -3
    assertEquals(List(1, 4, 7, 10), x2.sorted)
    assertEquals(List(10, 7, 4, 1), x2.sorted(reverseOrd))
  }

  @Test
  def test_SI10060_numeric_range_min_max(): Unit = {
    assertEquals(Range.Long.inclusive(1, 9, 1).min, 1)
    assertEquals(Range.Long.inclusive(1, 9, 1).max, 9)
    assertEquals(Range.Long.inclusive(9, 1, -1).min, 1)
    assertEquals(Range.Long.inclusive(9, 1, -1).max, 9)
    assertThrows[java.util.NoSuchElementException](Range.Long.inclusive(1, 9, -1).min)
    assertThrows[java.util.NoSuchElementException](Range.Long.inclusive(1, 9, -1).max)
    assertThrows[java.util.NoSuchElementException](Range.Long.inclusive(9, 1, 1).min)
    assertThrows[java.util.NoSuchElementException](Range.Long.inclusive(9, 1, 1).max)

    assertEquals(Range.Int.inclusive(1, 9, 1).min, 1)
    assertEquals(Range.Int.inclusive(1, 9, 1).max, 9)
    assertEquals(Range.Int.inclusive(9, 1, -1).min, 1)
    assertEquals(Range.Int.inclusive(9, 1, -1).max, 9)
    assertThrows[java.util.NoSuchElementException](Range.Int.inclusive(1, 9, -1).min)
    assertThrows[java.util.NoSuchElementException](Range.Int.inclusive(1, 9, -1).max)
    assertThrows[java.util.NoSuchElementException](Range.Int.inclusive(9, 1, 1).min)
    assertThrows[java.util.NoSuchElementException](Range.Int.inclusive(9, 1, 1).max)

    assertEquals(Range.inclusive(1, 9, 1).min, 1)
    assertEquals(Range.inclusive(1, 9, 1).max, 9)
    assertEquals(Range.inclusive(9, 1, -1).min, 1)
    assertEquals(Range.inclusive(9, 1, -1).max, 9)
    assertThrows[java.util.NoSuchElementException](Range.inclusive(1, 9, -1).min)
    assertThrows[java.util.NoSuchElementException](Range.inclusive(1, 9, -1).max)
    assertThrows[java.util.NoSuchElementException](Range.inclusive(9, 1, 1).min)
    assertThrows[java.util.NoSuchElementException](Range.inclusive(9, 1, 1).max)
  }

  @Test
  def dropToEnd(): Unit = {
    val test = 10 to 11
    val it = test.iterator
    it.drop(1)

    assertEquals(11, it.next)
  }
  @Test
  def dropToEnd2(): Unit = {
    val test = 10 until 11
    val it = test.iterator
    it.drop(0)

    assertEquals(10, it.next)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def largeRangeMap(): Unit = {
    Int.MinValue to Int.MaxValue map identity
  }

  @Test
  def rangeMinMax(): Unit = {
    assertEquals(1, (1 to 20).min)
    assertEquals(20, (1 to 20).max)

    assertEquals(1, (20 to 1 by -1).min)
    assertEquals(20, (20 to 1 by -1).max)

    assertEquals(20, (1 to 20).min(Ordering.Int.reverse))
    assertEquals(1, (1 to 20).max(Ordering.Int.reverse))

    assertEquals(20, (20 to 1 by -1).min(Ordering.Int.reverse))
    assertEquals(1, (20 to 1 by -1).max(Ordering.Int.reverse))
  }

  @Test
  def testRangeEndsWith(): Unit = {
    assertFalse((0 until 0).endsWith(List(-4, -3, -2, -1)))
    assertTrue((-8 until -1).endsWith((-8 until -1).takeRight(1)))
    assertTrue((0 until 0).endsWith(0 until 0))
  }

  @Test
  def testRangeDrop(): Unit = {
    assertTrue((0 until 0).iterator.drop(-4).toList.isEmpty)
    assertEquals(4, (1 to 4).iterator.drop(-4).toList.size)
  }

  @Test
  def `startsWith should not throw an exception when out of range`(): Unit = {
    assertTrue((1 to 5).startsWith(Nil, 7))
    assertFalse((1 to 5).startsWith(1 to 1, 8))
  }

}
