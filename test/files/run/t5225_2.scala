import scala.reflect.runtime.universe._

object Test extends App {
  val tree = reify{def foo(@annotation.elidable(0) x: Int) = ""}.tree
  println(tree.toString)
}