import scala.reflect.runtime.universe._

object Test extends App {
  def fooNoTypeTag[T] = {
    println(implicitly[TypeTag[T]])
    println(implicitly[TypeTag[List[T]]])
  }
  fooNoTypeTag[Int]
}