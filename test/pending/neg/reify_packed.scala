import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  reify {
    class C { override def toString() = "C" }
    val ret = List((new C, new C))
    ret.asInstanceOf[List[_]]
  };

  val toolbox = cm.mkToolBox()
  println(toolbox.eval(code.tree))
}