package scala.util

import org.junit.Assert._
import org.junit.Test

import scala.tools.reflect.ToolBoxError
import scala.tools.testkit.AssertUtil.assertThrows
import scala.tools.testkit.RunTesting

class ChainingOpsTest extends RunTesting {
  import scala.util.chaining._

  @Test
  def testAnyTap(): Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tap(xs => x = xs.head)

    assertEquals(1, x)
    assertEquals(List(1, 2, 3), result)
  }

  @Test def testAnyValTap(): Unit = assertEquals(42.tap(x => x), 42)

  @Test
  def testAnyPipe(): Unit = {
    val times6 = (_: Int) * 6
    val result = (1 - 2 - 3)
      .pipe(times6)
      .pipe(scala.math.abs)

    assertEquals(24, result)
  }

  @Test def testNoSelf(): Unit =
    assertThrows[ToolBoxError] {
      runner.run("import scala.util.chaining._; Nil.self")
    }
}
