object ImplicitClass {
  def X(i: Int) {}
  implicit class X(val x: Int) { def xx = x }

  def Z[A](i: A) {}
  implicit class Z[A](val i: A) { def zz = i }
}