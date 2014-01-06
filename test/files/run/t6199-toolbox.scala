import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = cm.mkToolBox()
  println(tb.eval(q"()"))
}