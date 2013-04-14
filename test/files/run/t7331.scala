import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  val tb = cm.mkToolBox()
  println(tb.parse("x").pos != NoPosition)
}