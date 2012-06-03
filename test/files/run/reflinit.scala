import scala.reflect.runtime.universe._

object Test extends App {
  val tt2 = typeOf[List[Int]]
  println(tt2)
}