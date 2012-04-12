import scala.reflect.mirror._

object Test extends App {
  println(implicitly[ConcreteTypeTag[Int]])
  println(implicitly[ConcreteTypeTag[List[Int]]])
}