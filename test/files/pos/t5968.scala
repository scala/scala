object X {
  def f(e: Either[Int, X.type]) = e match {
    case Left(i) => i
    case Right(X) => 0
    // SI-5986 spurious exhaustivity warning here
  }
}

