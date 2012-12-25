object Macros {
  type Foo(x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  class D extends Foo(2)(3)
  val x: Foo(2)(3) = 2
}