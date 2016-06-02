package scala.lang.primitives

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.RunTesting

@RunWith(classOf[JUnit4])
class NaNTest extends RunTesting {

  @Test
  def compNaNFalse(): Unit = {
    def code(tp: String) =
      s"""val n = $tp.NaN
          |def ne(x: $tp, y: $tp) = x != y
          |val fs: List[($tp, $tp) => Boolean] = List(_ < _, _ <= _, _ > _, _ >= _,  _ == _, (x, y) => !ne(x, y))
          |val vs = List[$tp](n, 1, -1, 0)
          |for (f <- fs; v <- vs; (x, y) <- List((n, v), (v, n))) yield f(x, y)
      """.stripMargin

    runner.run[List[Boolean]](code("Double")).foreach(assertFalse)
    runner.run[List[Boolean]](code("Float")).foreach(assertFalse)
  }

  @Test
  def genericEqNe(): Unit = {
    def code(tp: String) =
      s"""def a[T](x: T, y: T) = x == y
         |def b[T](x: T, y: T) = x != y
         |val n = $tp.NaN
         |a(n, n) :: a(n, 0) :: a (0, n) :: !b(n, n) :: !b(n, 0) :: !b(0, n) :: Nil
      """.stripMargin
    runner.run[List[Boolean]](code("Double")).foreach(assertFalse)
    runner.run[List[Boolean]](code("Float")).foreach(assertFalse)
  }
}
