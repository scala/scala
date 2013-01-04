object Macros {
  type Foo[T[U[_]]](x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  class D extends Foo[List](2)
  val x: Foo[List](2) = new C
}