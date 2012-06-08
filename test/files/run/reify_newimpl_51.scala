import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  {
    var counter = 0
    lazy val x = { counter += 1; counter }
    lazy val y = { counter += 1; counter }
    val code = reify {
      def foo = y // ensures that y is the first freevar we find
      val bar = reify { println(x * y) }
      bar.eval
      println(x)
      println(y)
    }
    code.eval
  }
}