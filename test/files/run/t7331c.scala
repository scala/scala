import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = cm.mkToolBox()
  val tree = tb.parse("class C").asInstanceOf[ClassDef]
  println(showRaw(tree))
  println(tree.pos)
  println(tree.impl.self.pos)
}