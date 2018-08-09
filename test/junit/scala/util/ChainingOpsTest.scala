package scala.util

import org.junit.Assert._
import org.junit.Test

class ChainingOpsTest {
  import scala.util.chaining._

  @Test
  def testAnyTap: Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tap(xs => x = xs.head)

    assertEquals(1, x)
    assertEquals(List(1, 2, 3), result)
  }

  @Test
  def testAnyPipe: Unit = {
    val times6 = (_: Int) * 6
    val result = (1 - 2 - 3)
      .pipe(times6)
      .pipe(scala.math.abs)

    assertEquals(24, result)
  }
}
