import scala.reflect.runtime.universe._

object Test extends App {
  println(implicitly[ConcreteTypeTag[Int]])
  println(implicitly[ConcreteTypeTag[Array[Int]]])
  println(implicitly[ConcreteTypeTag[Array[Array[Int]]]])
  println(implicitly[ConcreteTypeTag[Array[Array[Array[Int]]]]])
  println(implicitly[ConcreteTypeTag[Array[Array[Array[Array[Int]]]]]])
}