object Macros {
  def foo[T[U[_]]](x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  val s: String = foo[List](42)
}