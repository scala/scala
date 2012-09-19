import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  def foo(y: Int) = {
    class Foo(val y: Int) {
      def fun = reify{y}
    }

    val toolbox = cm.mkToolBox()
    val dyn = toolbox.eval(new Foo(y).fun.tree)
    dyn.asInstanceOf[Int]
  }

  println(foo(10))
}