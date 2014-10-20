import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

class ann(bar: String = "bar", quux: Array[String] = Array()) extends annotation.ClassfileAnnotation

object Test extends App {
  // test 1: reify
  val tree = reify{
    @ann(bar="1", quux=Array("2", "3")) class C
  }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val toolbox = cm.mkToolBox()
  val ttree = toolbox.typecheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.eval(tree)
}