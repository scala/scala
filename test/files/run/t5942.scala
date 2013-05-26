import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect._

object Test extends App {
  val tb = cm.mkToolBox()
  tb.parse("def x = {}")
  try { tb.parse("def x = {") } catch { case _: Throwable => }
  tb.parse("def x = {}")
}
