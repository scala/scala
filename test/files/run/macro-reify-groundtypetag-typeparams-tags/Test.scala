import scala.reflect.mirror._

object Test extends App {
  def fooTypeTag[T: ConcreteTypeTag] = {
    println(implicitly[ConcreteTypeTag[T]])
    println(implicitly[ConcreteTypeTag[List[T]]])
  }
  fooTypeTag[Int]
}