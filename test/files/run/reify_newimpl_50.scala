import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  {
    var y = 1
    def x = { y += 2; y }
    val code = reify {
      println(x)
      println(y)
      println(x)
    }
    code.eval
  }
}