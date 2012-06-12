import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  {
    var _x = 42
    def x = { val x0 = _x; _x += 1; x0 }
    var _y = 1
    def y = { val y0 = _y + _x; _y += y0; y0 }
    val code = reify {
      println(x)
      println(y)
      println(x)
    }
    code.eval
  }
}