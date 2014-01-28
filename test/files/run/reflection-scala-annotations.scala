import scala.reflect.runtime.universe._
import scala.annotation._

class sann(x: Int, y: List[Int]) extends StaticAnnotation
class jann(x: Int, y: Array[Int]) extends ClassfileAnnotation

@sann(1, List(1, 2))
class S

@jann(y = Array(1, 2), x = 2)
class J

object Test extends App {
  println(symbolOf[S].annotations.head.tree)
  println(symbolOf[J].annotations.head.tree)
}
