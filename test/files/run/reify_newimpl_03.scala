import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  {
    val code = reify {
      val x = 2
      reify{x}.eval
    }
    println(code.eval)
  }
}