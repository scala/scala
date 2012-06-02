import scala.reflect.runtime.universe._

object Test extends App {
  def fooTypeTag[T: TypeTag] = {
    println(implicitly[ConcreteTypeTag[T]])
    println(implicitly[ConcreteTypeTag[List[T]]])
  }
  fooTypeTag[Int]
}