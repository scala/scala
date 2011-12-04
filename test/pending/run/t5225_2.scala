import scala.reflect._
import scala.reflect.api._

object Test extends App {
  println(scala.reflect.Code.lift{
    def foo(@cloneable x: Int) = ""
  }.tree.toString)
}