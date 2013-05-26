object Macros {
  import scala.language.implicitConversions
  implicit def foo(x: String): Option[Int] = macro Impls.foo
}

object Test extends App {
  import Macros._
  println("2": Option[Int])
  val s: Int = "2" getOrElse 0
  println(s)
}
