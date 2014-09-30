class A
class B[T](x: T)
case class C(a: A, b: B[_])

case class D(a: A, b: B[_]*)

case class E(c: Class[_])

object Test extends App {
  def f1(c: C) = c match {
    case C(a, b) => ()
  }

  def f2(d: D) = d match {
    case D(a, b1, b2) => ()
  }

  def f3(e: E) = e match {
    case E(c) => ()
  }

  f1(C(new A, new B(1)))
  f2(D(new A, new B(1), new B(2)))
  f3(E(classOf[E]))
}
