import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
  val tree1 = reify(new collection.immutable.HashMap[String, String])
  val tree2 = reify(new collection.mutable.HashMap[String, String])
  println(showRaw(tb.typeCheck(tree1.tree), printIds = true, printKinds = true, printTypes = true))
  println(showRaw(tb.typeCheck(tree2.tree), printIds = true, printKinds = true, printTypes = true))
}