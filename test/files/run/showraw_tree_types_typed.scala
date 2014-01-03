import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val tree1 = reify(new collection.immutable.HashMap[String, String])
  val tree2 = reify(new collection.mutable.HashMap[String, String])
  println(showRaw(tb.typecheck(tree1.tree), printTypes = true))
  println(showRaw(tb.typecheck(tree2.tree), printTypes = true))
}