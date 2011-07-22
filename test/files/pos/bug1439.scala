// no unchecked warnings
class View[C[A]] { }

object Test {
  null match {
    case v: View[_] =>
  }
}
