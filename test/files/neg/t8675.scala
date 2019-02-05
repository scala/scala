class A(s: String) {
  def foo(x: A) = x
}

class isString(s: String)

class Test {

  def x[A](a: Any): A = ???

  def test: Unit = {
    val a = Array[A]()
    a.update(0, x[A]({new isString(true)})) // !!! allowed

    // boils down to
    class X {
      def m(p: Any): Unit = {}
    }
    implicit class XOps(x: X) {
      def m(p: Any): Unit = {}
    }
    new X().m(x[A]({new isString(true)})) // !!! allowed
  }
}
