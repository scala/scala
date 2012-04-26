import scala.reflect.mirror._

object Test extends App {
  val tree = reify{@transient @volatile var x = 2}.tree
  println(tree.toString)
}