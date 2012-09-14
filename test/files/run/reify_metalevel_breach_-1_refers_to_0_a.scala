import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  val x = 2
  val outer = reify{reify{x}}
// was:  val code = reify{outer.splice.splice}
  val code = reify{outer.eval.eval}

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.eval(code.tree)
  println("evaluated = " + evaluated)
}