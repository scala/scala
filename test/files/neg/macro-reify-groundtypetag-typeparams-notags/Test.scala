import scala.reflect.mirror._

object Test extends App {
  def fooNoTypeTag[T] = {
    println(implicitly[ConcreteTypeTag[T]])
    println(implicitly[ConcreteTypeTag[List[T]]])
  }
  fooNoTypeTag[Int]
}