import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object foo {
  object Expression {
    override def toString = "Expression"
  }
}

object Test extends App {
  val code = reify {
    List(foo.Expression, foo.Expression)
  };

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}