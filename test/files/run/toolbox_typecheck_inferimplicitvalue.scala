import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

class C
object C {
  implicit object MC extends C
}

object Test extends App {
  val tb = cm.mkToolBox()
  println(tb.inferImplicitValue(typeOf[C]))
}