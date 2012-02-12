import scala.reflect._
import scala.reflect.api._

object Test extends App {
  val tree = scala.reflect.Code.lift{@transient @volatile var x = 2}.tree
  println(tree.toString)
}