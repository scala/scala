import scala.reflect.runtime.universe._

object Test extends App {
  def fooNoTypeTag[T] = {
    println(implicitly[AbsTypeTag[T]])
    println(implicitly[AbsTypeTag[List[T]]])
  }
  fooNoTypeTag[Int]
}