// https://github.com/scala/bug/issues/8252
object t1 {
  import scala.language.higherKinds
  type Id[A] = A
  def foo[F[_], G[_], A](a: F[G[A]]): F[G[A]] = { println("what?! " + a); a }
  def oops(): Unit = {
    foo[Id, Id, Int](1)
  }
  oops()
  def expected(): Unit = {
    val kaboom = foo[Id, Id, Int](1)
  }
}

// https://github.com/scala/bug/issues/8252#issuecomment-534822175
object t2 {
  trait Foo { type X }
  trait HL extends Foo { override type X }
  trait HC[H <: Foo, T <: HL] extends HL { override type X = H#X with T#X }
  trait HN extends HL { override type X = Any }
  class A extends Foo { trait X } ; class B extends Foo { trait X }
  class Test {
      def test: Unit = {
          val bad = new HC[A, HC[B, HN]] {}
          val xx: bad.X = ???
      }
  }
}

// https://github.com/scala/bug/issues/8252#issuecomment-347417206
object t3 {
  import scala.language.reflectiveCalls

  trait Schema

  class Thing[S] {
    def apply[T](col: S => T): T = ???
  }

  type :+:[H, T <: Schema] = H with T
  type End = Schema

  class Test {

    def test = {
      new Thing[{val foo: String} :+: End].apply(_.foo)

      new Thing[{val foo: String} :+: {val bar: Int} :+: End].apply(x => x.foo)
    }
  }

  // https://github.com/scala/bug/issues/8252#issuecomment-347456209
  trait Example[T] {
    type Out

    def apply[A](fn: Out => A): A = ???
  }

  object Example {
    def apply[A](implicit inst: Example[A]): Aux[A, inst.Out] = inst

    type Aux[T, Out0] = Example[T] {type Out = Out0}

    implicit def forall[T]: Aux[T, T] = new Example[T] {
      type Out = T
    }
  }

  Example[ {val foo: Int} :+: {val bar: String} :+: {val baz: Boolean} :+: {val buzz: Double} :+: {val booze: Float} :+: End].apply(_.foo)

  // https://github.com/scala/bug/issues/8252#issuecomment-347562144
  new Example[Unit] {
    type Out = {val foo: Int} :+: {val bar: String} :+: {val baz: Boolean} :+: {val buzz: Double} :+: {val booze: Float} :+: End
  }.apply(_.foo)

  // https://github.com/scala/bug/issues/8252#issuecomment-347562502
  new Example[Unit] {
    type Out = {val foo: Int} :+: End
  }.apply(_.foo)
}

// https://github.com/scala/bug/issues/8252#issuecomment-347565398
object t4 {
  import scala.language.reflectiveCalls

  object Example1 {

    trait Schema

    type :+:[H, T <: Schema] = H with T
    type End = Schema

    class Thing[S] {
      type Out = S

      def apply[T](fn: Out => T): T = ???
    }

    new Thing[ {val foo: String} :+: {val bar: Int} :+: {val baz: Boolean} :+: {val booze: Double} :+: End
    ].apply(x => x.foo)

    val foo = new Thing[ {val foo: String} :+: {val bar: Int} :+: {val baz: Boolean} :+: {val booze: Double} :+: End
    ]

    foo.apply(x => x.foo)
  }

  object Example2 {

    trait Schema

    type :+:[H, T <: Schema] = H with T
    type End = Schema

    class Thing[S]

    implicit class ThingOps[S](self: Thing[S]) {
      type Out = S

      def apply[T](fn: Out => T): T = ???
    }

    val foo = new Thing[ {val foo: String} :+: {val bar: Int} :+: {val baz: Boolean} :+: {val booze: Double} :+: End
    ]

    foo.apply(x => x.foo)

  }
}