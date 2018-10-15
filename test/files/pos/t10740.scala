object Test {
  case class Inv[A](x: A)
  def diff1[A](i: Inv[A], j: Inv[_ <: A]) = 1
  def diff2[A](i: Inv[_ >: A], j: Inv[A]) = 2
  def diff3[A](i: Inv[_ >: A], j: Inv[_ <: A]) = 3
  val i = Inv(Option.whenNonNull(42))
  val j = Inv(Some(42))
  diff1(i, j)
  diff2(i, j)
  diff3(i, j)
}
