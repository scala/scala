import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  // test 1: reify
  val tree = reify{ trait Foo { val a: Int } }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val toolbox = cm.mkToolBox()
  val ttree = toolbox.typecheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.eval(tree)
}
