import scala.reflect.mirror._

object Test extends App {
  val tree = reify{def foo(@cloneable x: Int) = ""}.tree
  println(tree.toString)
}