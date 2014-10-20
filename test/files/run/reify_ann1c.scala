import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

class ann(bar: String) extends annotation.RuntimeAnnotation

object Test extends App {
  // test 1: reify
  val tree = reify{
    @ann(bar="1a") class C[@ann(bar="2a") T](@ann(bar="3a") x: T @ann(bar="4a")) {
      @ann(bar="5a") def f(x: Int @ann(bar="6a")) = {
        @ann(bar="7a") val r = (x + 3): @ann(bar="8a")
        val s = 4: Int @ann(bar="9a")
        r + s
      }
    }
  }.tree
  println(tree.toString)

  // test 2: import and typecheck
  val toolbox = cm.mkToolBox()
  val ttree = toolbox.typecheck(tree)
  println(ttree.toString)

  // test 3: import and compile
  toolbox.eval(tree)
}