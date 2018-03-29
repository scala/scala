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
    implicit val fooString: Foo[String] = ???
    implicit val fooBoolean: Foo[Boolean] = ???
    implicit def fooPair[T, U](implicit fooT: Foo[T], fooU: Foo[U]): Foo[(T, U)] = ???
    implicit def fooGen[T, R](implicit gen: Generic.Aux[T, R], fr: Foo[R]): Foo[T] = ???
  }

  case class A(b: B, i: Int)
  object A {
    implicit val genA: Generic[A] { type Repr = (B, (Int, Unit)) } = ???
  }

  case class B(c: C, i: Int, b: Boolean)
  object B {
    implicit val genB: Generic[B] { type Repr = (C, (Int, (Boolean, Unit))) } = ???
  }

  case class C(i: Int, s: String, b: Boolean)
  object C {
    implicit val genC: Generic[C] { type Repr = (Int, (String, (Boolean, Unit))) } = ???
  }

  implicitly[Foo[A]]
}
