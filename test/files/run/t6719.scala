import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
object Test extends App {
  val tb = cm.mkToolBox()
  val tree = tb.parse("(); val res = 0")
  println(tb.eval(tree))
}