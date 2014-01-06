
object Macros {
  import scala.language.experimental.macros
  import scala.language.implicitConversions
  implicit def foo(x: String): Option[Int] = macro Impls.foo
}

object Test extends App {
  import Macros._
  def bar[T <% Option[Int]](x: T) = println(x)
  println("2")
}
