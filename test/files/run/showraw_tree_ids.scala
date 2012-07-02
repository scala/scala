import scala.reflect.runtime.universe._

object Test extends App {
  val tree1 = reify(new collection.immutable.HashMap[String, String])
  val tree2 = reify(new collection.mutable.HashMap[String, String])
  def stabilize(s: String) = """#\d+""".r.replaceAllIn(s, "#<id>")
  println(stabilize(showRaw(tree1.tree, printIds = true)))
  println(stabilize(showRaw(tree2.tree, printIds = true)))
}