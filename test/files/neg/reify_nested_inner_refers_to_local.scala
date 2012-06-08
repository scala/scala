import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val code = reify{
    val x = 2
    reify{x}.splice
  };

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}