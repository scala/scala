object Test {
  trait Generic[T] {
    type Repr
  }
  object Generic {
    type Aux[T, R] = Generic[T] { type Repr = R }
  }

  trait GNil

  trait Foo[T]
  object Foo {
    implicit val fooUnit: Foo[Unit] = ???
    implicit val fooInt: Foo[Int] = ???
    implicit def fooPair[T, U](implicit fooT: Foo[T], fooU: Foo[U]): Foo[(T, U)] = ???
    implicit def fooGen[T, R](implicit gen: Generic.Aux[T, R], fr: Foo[R]): Foo[T] = ???
  }

  trait A
  object A {
    implicit val genA: Generic[A] { type Repr = (B, (Unit, Unit)) } = ???
  }

  trait B
  object B {
    implicit val genB: Generic[B] { type Repr = (Unit, (Unit, (Unit, Unit))) } = ???
  }

  implicitly[Foo[A]]
}
