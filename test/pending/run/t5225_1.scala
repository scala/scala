import scala.reflect._
import scala.reflect.api._

object Test extends App {
  println(scala.reflect.Code.lift{
    @transient @volatile var x = 2
  }.tree.toString)
}