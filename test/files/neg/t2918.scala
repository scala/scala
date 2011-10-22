object Test {
  def g[X, A[X] <: A[X]](x: A[X]) = x
}
