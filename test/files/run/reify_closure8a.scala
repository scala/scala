import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  class Foo(val y: Int) {
    def fun = reify{y}
  }

  val toolbox = cm.mkToolBox()
  val dyn = toolbox.runExpr(new Foo(10).fun.tree)
  val foo = dyn.asInstanceOf[Int]
  println(foo)
}