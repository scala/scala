object Macros {
  type Foo1(x: Int) = macro Impls.foo
  type Foo2[T](x: Int) = macro Impls.foo
  type Foo3[T, U](x: Int) = macro Impls.foo
  type Foo4[T[_]](x: Int) = macro Impls.foo
  type Foo5[T[U[_]]](x: Int) = macro Impls.foo
}

object Test extends App {
  import Macros._

  class D1 extends Foo1[String](2)
  val x1: Foo1[String](2) = new C

  class D2 extends Foo2[String, String](2)
  val x2: Foo2[String, String](2) = new C

  class D3 extends Foo3[String](2)
  val x3: Foo3[String](2) = new C

  class D4 extends Foo4[String](2)
  val x4: Foo4[String](2) = new C

  class D5 extends Foo5[List](2)
  val x5: Foo5[List](2) = new C
}