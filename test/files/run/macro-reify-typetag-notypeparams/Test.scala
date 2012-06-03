import scala.reflect.runtime.universe._

object Test extends App {
  println(implicitly[TypeTag[Int]])
  println(implicitly[TypeTag[List[Int]]])
}