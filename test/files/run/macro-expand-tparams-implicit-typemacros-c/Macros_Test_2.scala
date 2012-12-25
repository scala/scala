object Macros {
  type Foo[T](x: T) = macro Impls.impl[T]
  type FooFoo[T](x: T) = macro Impls.implImpl[T]
}

object Test extends App {
  import Macros._
  class D extends FooFoo(42)
  println(new D().tpe)
}