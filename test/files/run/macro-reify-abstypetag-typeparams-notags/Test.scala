import scala.reflect.runtime.universe._

object Test extends App {
  def fooNoTypeTag[T] = {
    println(implicitly[WeakTypeTag[T]])
    println(implicitly[WeakTypeTag[List[T]]])
  }
  fooNoTypeTag[Int]
}