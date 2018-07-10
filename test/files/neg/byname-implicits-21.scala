object Test {
  trait Generic[T] {
    type Repr
  }
  object Generic {
    type Aux[T, R] = Generic[T] { type Repr = R }
  }

  trait Foo[T]
  object Foo {
    implicit def fooOption[T](implicit fooT: Foo[T]): Foo[Option[T]] = ???
    implicit def fooGen[T, R](implicit gen: Generic.Aux[T, R], fr: => Foo[R]): Foo[T] = ???
  }

  trait A[T]
  object A {
    implicit def genA[T]: Generic[A[T]] { type Repr = Option[A[A[T]]] } = ???
  }

  implicitly[Foo[A[Unit]]]
}
