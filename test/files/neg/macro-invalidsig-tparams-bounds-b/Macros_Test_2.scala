object Macros {
  def foo[U <: Int] = macro Impls.foo[U]
}

object Test extends App {
  import Macros._
  foo[Int]
}