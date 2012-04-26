import scala.reflect.mirror._

object Test extends App {
  println(implicitly[TypeTag[Int]])
  println(implicitly[TypeTag[List[Int]]])
}