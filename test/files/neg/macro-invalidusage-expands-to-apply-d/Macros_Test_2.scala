object Macros {
  type Foo(x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._
  class D extends AnyRef with Foo(2)
}