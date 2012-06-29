import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  {
    type U = Int
    type T = U
    val code = reify {
      List[T](2)
    }
    println(code.eval)
  }
}