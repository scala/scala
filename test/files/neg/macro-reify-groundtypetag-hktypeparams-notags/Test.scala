import scala.reflect.runtime.universe._

object Test extends App {
  def fooNoTypeTagHK[C[_], T] = {
    println(implicitly[ConcreteTypeTag[C[T]]])
    println(implicitly[ConcreteTypeTag[List[C[T]]]])
  }
  fooNoTypeTagHK[List, Int]
}