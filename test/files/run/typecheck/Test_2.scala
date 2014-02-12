import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  Macros.foo

  val tb = cm.mkToolBox()
  tb.typecheck(q"class C")
}