class Test {
  val X: OnNext[Any] = null
  def Y: OnNext[Any] = null
  (null: Any) match {
    case X() => ()             // allowed
    case Y() => ()             // not allowed
    case OnNext[Any]() => ()   // should *not* be allowed, but was.
  }
}

class OnNext[+T] {
  def unapply(x: Any) = false
}
