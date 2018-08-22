import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

class ann0(bar: String) extends annotation.ClassfileAnnotation
class ann1(bar: String) extends annotation.ClassfileAnnotation

object Test extends App {
  // test 1: reify
  val tree = reify{
    @ann0(bar="1a") @ann1(bar="1b") class C[@ann0(bar="2a") @ann1(bar="2b") T](@ann0(bar="3a") @ann1(bar="3b") x: T @ann0(bar="4a") @ann1(bar="4b")) {
      @ann0(bar="5a") @ann1(bar="5b") def f(x: Int @ann0(bar="6a") @ann1(bar="6b")) = {
        @ann0(bar="7a") @ann1(bar="7b") val r = (x + 3): @ann0(bar="8a") @ann1(bar="8b")
        val s = 4: Int @ann0(bar="9a") @ann1(bar="9b")
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