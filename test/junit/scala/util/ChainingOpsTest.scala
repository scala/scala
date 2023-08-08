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

  @Test
  def testAnyTapIfFalse(): Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tapIf(false)(xs => x = xs.head)

    assertEquals(0, x)
    assertEquals(List(1, 2, 3), result)
  }

  @Test
  def testAnyTapIfFalseLambda(): Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tapIf(_.size > 4)(xs => x = xs.head)

    assertEquals(0, x)
    assertEquals(List(1, 2, 3), result)
  }

  @Test
  def testAnyTapIfTrue(): Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tapIf(true)(xs => x = xs.head)

    assertEquals(1, x)
    assertEquals(List(1, 2, 3), result)
  }

  @Test
  def testAnyTapIfTrueLambda(): Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tapIf(_.size < 4)(xs => x = xs.head)

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

  @Test
  def testAnyPipeIfFalse(): Unit = {
    val times6 = (_: Int) * 6
    val result = (1 - 2 - 3)
      .pipeIf(_ => false)(times6, identity)
      .pipe(scala.math.abs)

    assertEquals(4, result)
  }

  @Test
  def testAnyPipeIfFalseLambda(): Unit = {
    val times6 = (_: Int) * 6
    val result = (1 - 2 - 3)
      .pipeIf(_ > 0)(times6, identity)
      .pipe(scala.math.abs)

    assertEquals(4, result)
  }

  @Test
  def testAnyPipeIfTrue(): Unit = {
    val times6 = (_: Int) * 6
    val result = (1 - 2 - 3)
      .pipeIf(_ => false)(times6, identity)
      .pipe(scala.math.abs)

    assertEquals(24, result)
  }

  @Test
  def testAnyPipeIfTrueLambda(): Unit = {
    val times6 = (_: Int) * 6
    val result = (1 - 2 - 3)
      .pipeIf(_ < 0)(times6, identity)
      .pipe(scala.math.abs)

    assertEquals(24, result)
  }

  //TODO: testPipeIf where [A] != [B]

  @Test def testNoSelf(): Unit =
    assertThrows[ToolBoxError] {
      runner.run("import scala.util.chaining._; Nil.self")
    }
}
