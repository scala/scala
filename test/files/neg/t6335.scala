object ImplicitClass {
  def X(i: Int): Unit = {}
  implicit class X(val x: Int) { def xx = x }

  def Z[A](i: A): Unit = {}
  implicit class Z[A](val i: A) { def zz = i }
}
