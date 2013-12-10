import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val toolbox = cm.mkToolBox()

  def printSource[T](expr: Expr[T]) {
    val ttree = toolbox typecheck expr.tree
    println(ttree.toString)
  }

  var y = 3
  printSource(reify {
    5 + y
  })
}