import scala.reflect.runtime.universe._

object Test extends App {
  def fooTypeTag[T: AbsTypeTag] = {
    println(implicitly[AbsTypeTag[T]])
    println(implicitly[AbsTypeTag[List[T]]])
  }
  fooTypeTag[Int]
}