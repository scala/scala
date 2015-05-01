import language.higherKinds

trait Foo[A <: AnyRef] {
  type Repr
  def f(a: A): Repr
  def g(a: A): Option[Repr]

  type M[X]
  def m(a: A): M[a.type]

  type Id[X] = X
  def n(a: A): Id[(Repr, M[a.type])]

}

object Foo {
  type Aux[A <: AnyRef, B] = Foo[A] { type Repr = B; type M[X] = Int }

}

object Main extends App {
  def mapWithFoo[A <: AnyRef, B](as: List[A])(implicit foo: Foo.Aux[A, B]) = {
    // Should be Eta expandable because the result type of `f` is not
    // dependent on the value, it is just `B`.
    as map foo.f
    as map foo.g
    as map foo.m
    as map foo.n
  }
}
