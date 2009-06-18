abstract class C[@specialized T](_f: T) {
  def m(x: T): T
  def n(x: T): T = x

  val f: T = _f
/*
  class Inner[@specialized B] {
    def foo(x: T): T = x
  }

  new Inner
*/
}

class D extends C[Int](0) {
  def m(x: Int): Int = x * x
}
