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

  case class A(b: B, c: C, i: Int)
  object A {
    implicit val genA: Generic[A] { type Repr = (B, (C, (Int, Unit))) } = ???
  }

  case class B(c0: C, c1: C, c2: C, i: Int)
  object B {
    implicit val genB: Generic[B] { type Repr = (C, (C, (C, (Int, Unit)))) } = ???
  }

  case class C(i0: Int, i1: Int, i2: Int, i3: Int, i4: Int)
  object C {
    implicit val genC: Generic[C] { type Repr = (Int, (Int, (Int, (Int, (Int, Unit))))) } = ???
  }

  implicitly[Foo[A]]
}
