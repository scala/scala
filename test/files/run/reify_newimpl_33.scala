import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  object C {
    type T = Int
    val c = C
    val code = reify {
      List[c.T](2)
    }
    println(code.eval)
  }

  C
}