import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val toolbox = cm.mkToolBox()
  println(toolbox.parse("package foo { object bar }"))
}