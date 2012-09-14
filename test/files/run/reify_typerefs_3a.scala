import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object foo {
  class Expression {
    override def toString = "Expression"
  }
}

object Test extends App {
  val code = reify {
    List(new foo.Expression, new foo.Expression)
  };

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.eval(code.tree)
  println("evaluated = " + evaluated)
}