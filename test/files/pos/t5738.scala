import scala.reflect.runtime.universe._

object Test extends App {
  def f[T](a: T, b: T) = {
    reify(a.toString + b)
    reify(a + b.toString)
  }
}