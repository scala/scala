trait F1[T, R] {
  def andThen[A](g: R => A): Int = 0
}
class C1[TT, RR] extends F1[TT, RR]
