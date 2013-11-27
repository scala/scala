import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val toolbox = cm.mkToolBox(options = "-Yrangepos")
  val tree = toolbox.parse("2 + 2")
  println(tree.pos)
}
