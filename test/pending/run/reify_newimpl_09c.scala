import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  {
    def foo[W] = {
      type U = W
      type T = U
      reify {
        List[T](2)
      }
    }
    val code = foo[Int]
    println(code.tree.freeTypes)
    val W = code.tree.freeTypes(2)
    cm.mkToolBox().eval(code.tree, Map(W -> definitions.IntTpe))
    println(code.eval)
  }
}