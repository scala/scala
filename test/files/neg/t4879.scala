case class C(d: Double) { }
case class D[T, U, V](bingo: Int, donkey: String, private val vegas: Set[A])(jehovah: Int) { }

class A {
  def f = (new C(5)) match {
    case C  => true
    case _  => false
  }
  def g[T, U, V](x: D[T, U, V]) = x match {
    case D  => true
    case _  => false
  }
}


