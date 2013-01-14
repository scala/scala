object Macros {
  type Foo_varargs(xs: Int*) = macro Impls.impl_varargs
  type Foo(xs: Int*) = macro Impls.impl
}

object Test extends App {
  import Macros._

  class D1_varargs extends Foo_varargs(1, 2, 3, 4, 5)
  class D1 extends Foo(1, 2, 3, 4, 5)

  val numbers = List(1, 2, 3, 4, 5)
  class D2_varargs extends Foo_varargs(numbers: _*)
  class D2 extends Foo(numbers: _*)
}