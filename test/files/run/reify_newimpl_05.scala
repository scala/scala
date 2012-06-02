import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  {
    val code = reify {
      var x = 2
      def y = x // forcibly captures x
      reify{x}.eval
    }
    println(code.eval)
  }
}