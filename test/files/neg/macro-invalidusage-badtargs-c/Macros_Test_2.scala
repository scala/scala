object Macros {
  def foo[T[_]](x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  val s: String = foo[String](42)
}