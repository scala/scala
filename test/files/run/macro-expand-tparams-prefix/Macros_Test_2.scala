object Macros1 {
  class C[T] {
    def foo[U](x: U) = macro Impls1.foo[U]
    def bar[U](x: _) = macro Impls1.foo[U]
  }
}

object Macros2 {
  class C[T] {
    def foo[U](x: U) = macro Impls2.foo[T, U]
    def bar[U](x: _) = macro Impls2.foo[T, U]
  }
}

object Macros3 {
  class D[T] {
    class C[U] {
      def foo[V] = macro Impls345.foo[T, U, V]
      def bar[V](x: _) = macro Impls345.bar[T, U, V]
    }
  }
}

// object Macros4 is declared in Impls_1.scala

object Macros5 {
  class D[T] {
    class C[U] {
      def foo[V] = macro Impls345.foo[T, U, V]
      foo[Boolean]

      def bar[V](x: _) = macro Impls345.bar[T, U, V]
      bar[Boolean](42)
    }
  }
}

object Test extends App {
  println("===Macros1 typed===")
  new Macros1.C[Int]().foo(42)
  new Macros1.C[Boolean]().foo(42)
  new Macros1.C[Int]().foo("42")
  new Macros1.C[String]().foo(true)

  println("===Macros1 untyped===")
  new Macros1.C[Int]().bar(42)
  new Macros1.C[Boolean]().bar(42)
  new Macros1.C[Int]().bar("42")
  new Macros1.C[String]().bar(true)

  println("===Macros2 typed===")
  object D2T extends Macros2.C[Boolean]
  D2T.foo(42)
  D2T.foo("42")

  println("===Macros2 untyped===")
  object D2U extends Macros2.C[Boolean]
  D2U.bar(42)
  D2U.bar("42")

  println("===Macros3 typed===")
  val outer31t = new Macros3.D[Int]
  val outer32t = new outer31t.C[String]
  outer32t.foo[Boolean]

  println("===Macros3 untyped===")
  val outer31u = new Macros3.D[Int]
  val outer32u = new outer31u.C[String]
  outer32u.bar[Boolean](42)

  println("===Macros4 typed===")
  val outer41t = new Macros4.D[Int]
  val outer42t = new outer41t.C[String]
  outer42t.foo[Boolean]

  println("===Macros4 untyped===")
  val outer41u = new Macros4.D[Int]
  val outer42u = new outer41u.C[String]
  outer42u.bar[Boolean](42)

  println("===Macros5 typed and untyped===")
  val outer51 = new Macros5.D[Int]
  new outer51.C[String]
}