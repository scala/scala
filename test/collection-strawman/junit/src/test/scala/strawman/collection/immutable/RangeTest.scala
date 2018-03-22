package strawman.collection.immutable

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
}
