import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    val sumOfSquares1 = (for (i <- 1 to 100; if (i % 3 == 0)) yield Math.pow(i, 2)).sum
    val sumOfSquares2 = (1 to 100).filter(_ % 3 == 0).map(Math.pow(_, 2)).sum
    assert(sumOfSquares1 == sumOfSquares2)
  }.eval
}