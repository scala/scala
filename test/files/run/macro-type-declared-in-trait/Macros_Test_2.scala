trait Base {
  type Foo(x: Int) = macro Impls.foo
}

object Macros extends Base

class Macros extends Base

object Test extends App {
  val prefix1 = new Base {}
  class D1 extends prefix1.Foo(2)
  val x1: prefix1.Foo(2) = new D1

  class D2 extends Macros.Foo(2)
  val x2: Macros.Foo(2) = new D2

  val prefix3 = new Macros()
  class D3 extends prefix3.Foo(2)
  val x3: prefix3.Foo(2) = new D3
}