package scala.util

import org.junit.Assert._
import org.junit.Test

import scala.tools.reflect.ToolBoxError
import scala.tools.testing.RunTesting

class ChainingOpsTest extends RunTesting {
  import scala.util.chaining._

  @Test
  def testAnyTap: Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tap(xs => x = xs.head)

    assertEquals(1, x)
    assertEquals(List(1, 2, 3), result)
  }

  @Test def testAnyValTap: Unit = assertEquals(42.tap(x => x), 42)

  @Test
  def testAnyPipe: Unit = {
    val plus3 = (_: Int) + 3
    val result = (1 - 2) |> plus3 |> scala.math.abs

    assertEquals(2, result)
  }

  @Test(expected = classOf[ToolBoxError]) def testNoSelf: Unit =
    runner.run("import scala.util.chaining._; Nil.self")
}
