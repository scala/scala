import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = currentMirror.mkToolBox()
  tb.compile(q"true > true")
  tb.typecheck(q"true > true")
}