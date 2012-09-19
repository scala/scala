import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  def fun() = {
    def z() = 2
    reify{z}
  }

  val toolbox = cm.mkToolBox()
  val dyn = toolbox.eval(fun().tree)
  val foo = dyn.asInstanceOf[Int]
  println(foo)
}