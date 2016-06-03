abstract class A { def foo(a: Int): A }
class B extends A {
  implicit def spackle(x: Int): A = new B
  def foo(a) = a
}