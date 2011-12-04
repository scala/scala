import scala.reflect._
import scala.reflect.api._

object Test extends App {
  println(scala.reflect.Code.lift{
    @serializable class C
  }.tree.toString)
}