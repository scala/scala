class K[E] {
  case class A(v: E) {}
}

class K2 extends K[Int] {
  val A(v) = A(42)
}
