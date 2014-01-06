object Macros1 {
  class C[T] {
    def foo[U](x: U): Unit = macro Impls1.foo[U]
  }
}

object Macros2 {
  class C[T] {
    def foo[U](x: U): Unit = macro Impls2.foo[T, U]
  }
}

object Macros3 {
  class D[T] {
    class C[U] {
      def foo[V]: Unit = macro Impls345.foo[T, U, V]
    }
  }
}

// object Macros4 is declared in Impls_1.scala

object Macros5 {
  class D[T] {
    class C[U] {
      def foo[V]: Unit = macro Impls345.foo[T, U, V]
      foo[Boolean]
    }
  }
}

object Test extends App {
  println("===Macros1===")
  new Macros1.C[Int]().foo(42)
  new Macros1.C[Boolean]().foo(42)
  new Macros1.C[Int]().foo("42")
  new Macros1.C[String]().foo(true)

  println("===Macros2===")
  object D2 extends Macros2.C[Boolean]
  D2.foo(42)
  D2.foo("42")

  println("===Macros3===")
  val outer31 = new Macros3.D[Int]
  val outer32 = new outer31.C[String]
  outer32.foo[Boolean]

  println("===Macros4===")
  val outer41 = new Macros4.D[Int]
  val outer42 = new outer41.C[String]
  outer42.foo[Boolean]

  println("===Macros5===")
  val outer1 = new Macros5.D[Int]
  new outer1.C[String]
}