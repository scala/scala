import scala.reflect.runtime.universe._

object Test extends App {
  println(implicitly[TypeTag[Int]])
  println(implicitly[TypeTag[Array[Int]]])
  println(implicitly[TypeTag[Array[Array[Int]]]])
  println(implicitly[TypeTag[Array[Array[Array[Int]]]]])
  println(implicitly[TypeTag[Array[Array[Array[Array[Int]]]]]])
}