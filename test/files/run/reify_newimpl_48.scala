import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  val outer1 = {
    val x = 2
    reify{x}
  }

  val outer2 = {
    val x = 3
    reify{x}
  }

  val code = reify{
    val x = 4
    x + outer1.splice + outer2.splice
  }

  println(code.eval)
}