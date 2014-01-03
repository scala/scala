import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

class ann(bar: String) extends annotation.ClassfileAnnotation

object Test extends App {
  // test 1: reify
  val tree = reify{
    @ann(bar="1a") @ann(bar="1b") class C[@ann(bar="2a") @ann(bar="2b") T](@ann(bar="3a") @ann(bar="3b") x: T @ann(bar="4a") @ann(bar="4b")) {
      @ann(bar="5a") @ann(bar="5b") def f(x: Int @ann(bar="6a") @ann(bar="6b")) = {
        @ann(bar="7a") @ann(bar="7b") val r = (x + 3): @ann(bar="8a") @ann(bar="8b")
        val s = 4: Int @ann(bar="9a") @ann(bar="9b")
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