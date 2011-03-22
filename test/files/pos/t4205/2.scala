trait B[OUT[_]] {
  def b1[A](a: A) = b2[OUT]
  def b2[OUT1[_]] = ()
}
