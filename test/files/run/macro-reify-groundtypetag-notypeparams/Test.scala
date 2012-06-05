import scala.reflect.runtime.universe._

object Test extends App {
  println(implicitly[ConcreteTypeTag[Int]])
  println(implicitly[ConcreteTypeTag[List[Int]]])
}