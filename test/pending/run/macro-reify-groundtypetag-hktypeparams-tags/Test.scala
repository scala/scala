import scala.reflect.mirror._

object Test extends App {
  def fooTypeTagHK[C[_]: ConcreteTypeTag, T: ConcreteTypeTag] = {
    println(implicitly[ConcreteTypeTag[C[T]]])
    println(implicitly[ConcreteTypeTag[List[C[T]]]])
  }
  fooTypeTagHK[List, Int]
}