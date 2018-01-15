object Test {
  trait Holder[A]
  trait NilHolder[A] extends Holder[A]

  trait Solve[A, H <: Holder[A]] {
    type Output <: Holder[A]
  }
  type SolveAux[A, H <: Holder[A], O <: Holder[A]] = Solve[A, H] {type Output = O}

  implicit def nilSolve[A] = new Solve[A, NilHolder[A]] {
    override type Output = NilHolder[A]
  }

  trait WrapSolve[A, H <: Holder[A]] {
    type Output <: Holder[A]
  }

  implicit def wrapAux[A, H <: Holder[A], O <: Holder[A]](implicit one : SolveAux[A, H, O]) =
    new WrapSolve[A, H] {
      override type Output = O
    }

  val wrapped = implicitly[WrapSolve[String, NilHolder[String]]]
}

object Test2 {
  class Inv[T]
  class Foo[T, U <: Inv[T]]

  implicit def foo[T]: Foo[T, Inv[T]] = new Foo[T, Inv[T]]

  def bar[T, U <: Inv[T]](implicit foo: Foo[T, U]): Inv[T] = new Inv[T]

  val baz: Inv[Int] = bar
}
