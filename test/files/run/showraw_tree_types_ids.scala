import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val tree1 = reify(new collection.immutable.HashMap[String, String])
  val tree2 = reify(new collection.mutable.HashMap[String, String])
  def stabilize(s: String) = """#\d+""".r.replaceAllIn(s, "#<id>")
  println(stabilize(showRaw(tb.typecheck(tree1.tree), printIds = true, printTypes = true)))
  println(stabilize(showRaw(tb.typecheck(tree2.tree), printIds = true, printTypes = true)))
}