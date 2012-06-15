import scala.reflect.runtime.universe._

object Test extends App {
  val tree = reify{trait C { private[this] val x = 2; var y = x; lazy val z = y }}
  println(showRaw(tree.tree))
}