class C[T] extends D[T] {
  private def c1 = 0
  private[this] def c2 = 0
}

trait D[T] {
  self: C[T] =>

  private def d1 = 0
  private[this] def d2 = 0

  c1 // a member, but inaccessible.
  c2 // a member, but inaccessible.

  d1 // okay
  d2 // okay


  class C {
    d1
    d2
  }

  def x(other: D[Any]) {
    other.d1
    other.d2 // not a member
  }
}
