class D extends C

object Macros {
  def foo[T <: D] = macro Impls.foo[T]
}

object Test extends App {
  import Macros._
  foo[D]
}