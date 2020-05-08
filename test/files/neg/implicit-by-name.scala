object Test {
  implicit def reverseOrd[A](implicit ord: => Ordering[A]): Ordering[A] =
    ord.reverse
}
