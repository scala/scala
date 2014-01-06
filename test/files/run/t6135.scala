object Test extends App {
  class A { class V }

  abstract class B[S] {
    def foo(t: S, a: A)(v: a.V)
  }

  val b1 = new B[String] {
    def foo(t: String, a: A)(v: a.V) = () // Bridge method required here!
  }

  b1.foo("", null)(null)
}
