import scala.reflect.runtime.universe._

object Test extends App {
  println(typeOf[Int])
  println(typeOf[Array[Int]])
  println(typeOf[Array[Array[Int]]])
  println(typeOf[Array[Array[Array[Int]]]])
  println(typeOf[Array[Array[Array[Array[Int]]]]])
}