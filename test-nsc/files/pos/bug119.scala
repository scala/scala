class K[E] {
  case class A(v:E){};
}

class K2 extends K[int] {
  val A(v) = A(42);
}
