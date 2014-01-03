import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

// pending until https://issues.scala-lang.org/browse/SI-6393 is fixed
object Test extends App {
  val tb = cm.mkToolBox()
  val expr = tb.parse("math.sqrt(4.0)")
  println(tb.typecheck(expr))
}