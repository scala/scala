import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  val code = reify {
    object C { def qwe = 4 }
    case class C(foo: Int, bar: Int)
    val c = C(2, 2)
    println(c.foo * c.bar == C.qwe)
  };

  val toolbox = cm.mkToolBox()
  println(code.tree)
  println(code.eval)
}