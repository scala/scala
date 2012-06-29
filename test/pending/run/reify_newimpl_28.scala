import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  {
    object C {
      type T = Int
      val code = reify {
        List[T](2)
      }
      println(code.eval)
    }

    C
  }
}