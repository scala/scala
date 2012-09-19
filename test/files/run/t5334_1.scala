import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val code = reify {
    class C { override def toString = "C" }
    val ret = new C
    ret.asInstanceOf[Object]
  };

  val toolbox = cm.mkToolBox()
  println(toolbox.eval(code.tree))
}