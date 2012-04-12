class D[T] {
  class C[U] {
    def foo[V] = macro Impls.foo
  }
}

object Test extends App {
  val outer1 = new D[Int]
  val outer2 = new outer1.C[String]
  outer2.foo[Boolean]
}