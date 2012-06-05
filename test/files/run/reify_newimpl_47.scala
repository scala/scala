import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  val outer = {
    val x = 2
    reify{x}
  }

  val code = reify{
    val x = 42
    outer.splice
  }

  println(code.eval)
}