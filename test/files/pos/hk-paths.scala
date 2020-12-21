object Test {
  // scala/bug#12142
  trait Bounds {
    type Upper <: Bounds
  }

  trait Narrow extends Bounds {
    type Upper >: Narrow <: Bounds
  }

  trait Template[+X <: Bounds] extends Bounds {
    val body :X
    type Bound >: body.Upper <: Bounds
    type Copy[+A <: Bound] <: Template[A]
    type High[T[+A <: Narrow] <: Bounds]

    def applied(narrow: Template[Narrow]): High[narrow.Copy] //ok
    def indirect(narrow: Template[Narrow]): High[({ type T[+A <: Narrow] = narrow.Copy[A] })#T] //also ok
  }

  trait Expr[X, E] {
    def applyTo[F[A >: E <: E]] :Expr[X, F[E]]
  }

  trait Functor[F, A <: U, U] {
    type Apply[+X <: U]
    type Super[+X >: A <: U] >: F
  }

  trait Implicit[X, Y] {
    type S >: Y
    val expr :Expr[X, S]
  }

  def test[F, A <: U, U, X](fun :Functor[F, A, U], x :Implicit[X, A] { type S >: A <: U }) = {
    x.expr.applyTo[({ type E[B >: A <: U] = fun.Super[B] })#E]
    x.expr.applyTo[fun.Super]
  }

  // scala/bug#10186
  trait Foo {
    type A
    type F[_ <: A]
  }

  def noop[A, F[_ <: A]]: Unit = ()
  def f(foo: Foo): Unit = noop[foo.A, foo.F]

  // scala/bug#9625
  trait `* -> *`[F[_]]

  trait Bar {
    type Qux[A]
    implicit val `* -> *`: `* -> *`[Qux]
  }

  def foo(bar: Bar): `* -> *`[bar.Qux] = {
    import bar._
    implicitly[`* -> *`[bar.Qux]]
  }
}
