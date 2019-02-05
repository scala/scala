object Test {
  class Z
  class O[T]
  class E[T]

  class Expand[T, U]
  object Expand {
    implicit def expando[T]: Expand[O[T], E[O[T]]] = ???
    implicit def expande[T]: Expand[E[T], O[E[T]]] = ???
  }

  implicit def mkN[T, U](implicit e: => Expand[T, U], u: => U): T = ???

  implicitly[O[Z]]
}
