import scala.reflect.runtime.universe._

object Test extends App {
  def fooTypeTag[T: TypeTag] = {
    println(implicitly[AbsTypeTag[T]])
    println(implicitly[AbsTypeTag[List[T]]])
  }
  fooTypeTag[Int]
}