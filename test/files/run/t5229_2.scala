import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val code = reify {
    object C {
      val x = 2
    }

    println(C.x)
  };

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.eval(code.tree)
  println("evaluated = " + evaluated)
}