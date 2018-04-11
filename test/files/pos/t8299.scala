object Test extends App {
  class C

  implicit class CompatibleC(c: C) {
    def foo(x: Int) = ???
  }

  val c: C = ???
  println(c.foo _)

  object B {
    def f(x: Int) = x
    def f(x: String) = x
  }

  val fun: Int => Int = B.f _
  val funs: String => String = B.f _
}
