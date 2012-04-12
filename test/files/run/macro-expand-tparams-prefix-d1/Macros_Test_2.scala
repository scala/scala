object Test extends App {
  class D[T] {
    class C[U] {
      def foo[V] = macro Impls.foo[T, U, V]
      foo[Boolean]
    }
  }

  val outer1 = new D[Int]
  new outer1.C[String]
}