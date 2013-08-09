import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = cm.mkToolBox()
  val code = tb.parse("{ val x: test.Separate$$anonfun$1 = null; x }")
  println(tb.eval(code))
}