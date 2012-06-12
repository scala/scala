import scala.reflect.runtime.universe._

object Test extends App {
  def foo[T](a: T) = reify {
    val x1 = a
    val x2 = reify(a)
  }
}