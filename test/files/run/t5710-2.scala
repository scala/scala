import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val code = reify {
    val (x, y) = "abc" match { case x => (x, x) }
    (x, y)
  };

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.eval(code.tree)
  println("evaluated = " + evaluated)
}