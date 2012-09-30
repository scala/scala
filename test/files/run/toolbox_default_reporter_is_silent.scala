import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val toolbox = cm.mkToolBox()
  toolbox.eval(reify{
    object Utils {
      @deprecated("test", "2.10.0")
      def foo { println("hello") }
    }

    Utils.foo
  }.tree)
}