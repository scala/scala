class DivergingImplicitReported {
  trait TC[M]
  trait S

  implicit def tc[M](implicit M0: TC[M]): TC[S] = null
  def breakage[F: TC] = 0
  breakage // correct: diverging implicit expansion
}