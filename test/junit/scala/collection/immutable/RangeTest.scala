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
}
