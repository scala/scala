trait S[T]

object P {
  def x(t: Int)(ss: Seq[S[_]]): Seq[S[_]] = ss
}

